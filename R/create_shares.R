#' Create shares to disaggregate GTAP land rents
#'
#' This function creates country-BIOME share variables so the FAO production
#' data downloaded using the download_fao function can be disaggregated.
#'
#' @param monfreda_fao_concord_file File path to concordance linking monfreda
#'    crop names to fao crop codes
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Saves  the three .rds files below to ./workdir/. These files contain share variables for country and
#'    subnational area combinations.
#'
#' gadm_bio_crop_prod_shares.rds
#'
#' gadm_bio_landcov_shares.rds
#'
#' [NV: What are these data for and which function uses them?]
#'
#' @export
create_shares <- function(monfreda_fao_concord_file, workdir_dir=getwd()) {
  #### Crop production in base year ####
  #First load and process crop production data
  base_year_crop_production <- readRDS(file.path(workdir_dir, 'workdir/base_year/crop_production_tons.rds'))
  base_year_crop_production <- tidyr::pivot_longer(base_year_crop_production,
                                                   cols = abaca_Production:yautia_Production,
                                                   names_to = 'crop',
                                                   values_to = 'production_tons') %>%
    dplyr::mutate(crop = gsub(pattern = '_Production', replacement = '', crop))
  #Merge in the FAO names by matching with the names Monfreda used for raster values
  #Available in "Global Harvested Area and Yield for 175 Crops Metadata and Technical Documentation"
  #at https://geodata.ucdavis.edu/geodata/crops/monfreda/METADATA_HarvestedAreaYield175Crops_June2018.pdf
  monfreda_fao_concord <- readxl::read_excel(monfreda_fao_concord_file)

    monfreda_fao_concord <-
        monfreda_fao_concord %>%
        dplyr::rename(item = monfreda_detailed,
               crop = crop_raster_name) %>%
        dplyr::mutate(crop = gsub(' ', '', crop))

  #Merge in the fao_codes - from Step 4
  crop_fao_item_codes <- readRDS(file = file.path(paste(workdir_dir, '/workdir/fao_data/',sep=""), "fao_cpc_item_codes.rds"))
  monfreda_fao_concord <- dplyr::left_join(monfreda_fao_concord, crop_fao_item_codes, by = c('item'))
  #Merge
  base_year_crop_production <- dplyr::left_join(base_year_crop_production, monfreda_fao_concord, by = c('crop'))
  #Replace missing values with 0191 - Forage products
  base_year_crop_production <- base_year_crop_production %>%
    dplyr::mutate(item_code_cpc = ifelse(is.na(item_code_cpc), "0191", item_code_cpc))

  #### Create Shares ####
  #Make totals for countries
  crop_production_shares <- base_year_crop_production %>%
    dplyr::group_by(GADM, crop, item_code_cpc) %>%
    dplyr::mutate(reg_total_prod = sum(production_tons, na.rm = T)) %>%
    dplyr::ungroup()
  #Make shares for country-BIOME combinations
  crop_production_shares <- crop_production_shares %>%
    dplyr::mutate(share_gadm_output = ifelse(reg_total_prod==0, 0, production_tons/reg_total_prod))
  #Save
  saveRDS(crop_production_shares, file.path(workdir_dir, 'workdir/gadm_bio_crop_prod_shares.rds'))

  #### Livestock production in base year
  rm(list=ls()[! ls() %in% c("workdir_dir")])
  gc()
  base_year_livestock_production <- readRDS(file.path(workdir_dir, 'workdir/base_year/livestock_production_head.rds'))
  #Add up the animals by GTAP category
  #  ctl - cattle, buffaloes, camels, other camelids, sheep, goats, live animals NES, horses, asses, mules, crude organic materials NES (bovine semen)
  #  rmk - Cow milk, whole (fresh), Buffalo milk, Sheep milk, Goat milk, Camel milk
  base_year_livestock_production <- base_year_livestock_production %>%
    dplyr::mutate(ctl = cattle + goats + sheep,
                  rmk = cattle + goats + sheep,
                  wol = goats + sheep)
  base_year_livestock_production <- base_year_livestock_production %>%
    dplyr::select(GADM, BIO, GADM_BIO, ctl:wol)
  #Pivot to longer format
  base_year_livestock_production <- base_year_livestock_production %>%
    tidyr::pivot_longer(cols = ctl:wol,
                        names_to = "GSC3",
                        values_to = "head")
  #### Create Shares ####
  #Make totals for countries
  lvstk_production_shares <- base_year_livestock_production %>%
    dplyr::group_by(GADM, GSC3) %>%
    dplyr::mutate(reg_total_prod = sum(head, na.rm = T)) %>%
    dplyr::ungroup()
  #Make shares for country-BIOME combinations
  lvstk_production_shares <- lvstk_production_shares %>%
    dplyr::mutate(share_gadm_output = ifelse(reg_total_prod==0, 0, head/reg_total_prod))
  #Save
  saveRDS(lvstk_production_shares, file = file.path(workdir_dir, 'workdir/gadm_bio_lvstk_prod_shares.rds'))

  #### Land Cover from Step 2 ####
  #Select country-BIO with cropland, pasture, and other land cover types
  #From Baldos (2009) GTAP land cover v.9:
  #"Note that land cover is updated only in region-108AEZ wherein both cropland, pasture and other
  #land types are present (if this condition is not met, land cover in a region-108AEZ is fixed to the
  #2001 base year values)."
  base_lc_df <- readRDS(file.path(workdir_dir, 'workdir/base_year/GADM_BIO_landcover_rasterdata_ha.rds')) %>%
    dplyr::mutate(GADM = substr(GADM_BIO, 1, 3),
                  BIO = substr(GADM_BIO, 5, nchar(paste0(GADM_BIO)))) %>%
    dplyr::select(GADM, BIO, GADM_BIO, everything())
  base_lc_df <- base_lc_df %>% dplyr::mutate(not_crop_pasture_ha = rowSums(across(forest_ha:urban_ha)))
  cntry_bio_landcov_update <- base_lc_df %>% dplyr::filter(cropland_ha > 0 &
                                                             pasture_ha > 0 &
                                                             not_crop_pasture_ha > 0)
  #calculate share of country's land cover within each BIOME
  #First calculate total hectares in each land cover by country
  cntry_bio_landcov_update <- cntry_bio_landcov_update %>% dplyr::group_by(GADM) %>%
    dplyr::mutate(country_total_cropland = sum(cropland_ha),
                  country_total_pasture = sum(pasture_ha),
                  country_total_forest = sum(forest_ha),
                  country_total_urban = sum(urban_ha),
                  country_total_other = sum(other_ha),
                  country_total_grassland = sum(grassland_ha),
                  country_total_shrubland = sum(shrubland_ha)) %>% dplyr::ungroup()
  #Now create share of country's hectares in each BIOME, and total hectares for countries
  cntry_bio_landcov_update <- cntry_bio_landcov_update %>%
    dplyr::mutate(bio_share_country_cropland = cropland_ha/country_total_cropland,
                  bio_share_country_pasture = pasture_ha/country_total_pasture,
                  bio_share_country_forest = forest_ha/country_total_forest,
                  bio_share_country_urban = urban_ha/country_total_urban,
                  bio_share_country_other = other_ha/country_total_other,
                  bio_share_country_grassland = grassland_ha/country_total_grassland,
                  bio_share_country_shrubland = shrubland_ha/country_total_shrubland,
                  country_total_area = rowSums(across(country_total_cropland:country_total_shrubland)))
  #Finally create the share of each country's total hectares in each land cover type
  cntry_bio_landcov_update <- cntry_bio_landcov_update %>%
    dplyr::mutate(lc_share_country_cropland = country_total_cropland/country_total_area,
                  lc_share_country_pasture = country_total_pasture/country_total_area,
                  lc_share_country_forest = country_total_forest/country_total_area,
                  lc_share_country_urban = country_total_urban/country_total_area,
                  lc_share_country_other = country_total_other/country_total_area,
                  lc_share_country_grassland = country_total_grassland/country_total_area,
                  lc_share_country_shrubland = country_total_shrubland/country_total_area)
  #Remove the country total area variable
  cntry_bio_landcov_update <- cntry_bio_landcov_update %>%
    dplyr::select(-country_total_area)
  #Save
  saveRDS(cntry_bio_landcov_update, file = file.path(workdir_dir, 'workdir/gadm_bio_landcov_shares.rds'))
}
