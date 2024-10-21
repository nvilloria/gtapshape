#' Download FAO data
#'
#' This function downloads FAO crop and livestock production data for a
#' specified year, as well as price data.
#'
#' @param gtap_year Year of FAO data to be downloaded
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Saves .rds files containing production and price data
#'
#' The following files are saved in ./workdir/fao_data/
#'
#' fao_cpc_item_codes.rds
#'
#' fao_land_cover.rds
#'
#' fao_prices.rds
#'
#' fao_production_crops.rds
#'
#' fao_production.rds
#'
#' fao_val_prod.rds
#'
#' And the following files are saved in:
#'
#' fao_land_cover.rds
#'
#' fao_livestock_numanimals.rds
#'
#' fao_prod_harvarea.rds
#'
#' fao_producer_prices.rds
#'
#' fao_val_prod_1000usd.rds
#'
#' [NV: where are these data used and for what. Link to function. I think is [create_shares]
#'
#' @export
download_fao <- function(gtap_year, workdir_dir=getwd()) {
  #Define GTAP year
  gtap_ref_year <- gtap_year

  #Specify the data folder where raw FAO data gets downloaded to
  data_folder <- paste(workdir_dir, '/workdir/fao_data/',sep="")

  #### Production ####
  #Bulk download production data for crops and livestock
  fao_production <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder = data_folder)
  #Keep only specified GTAP reference year data, rename and reformat columns
  fao_production <- fao_production %>%
    dplyr::filter(year_code == gtap_ref_year) %>%
    dplyr::rename(area_code_m49 = area_code__m49_,
                  item_code_cpc = item_code__cpc_) %>%
    dplyr::mutate(element_code = as.numeric(element_code),
                  area_code_m49 = substr(area_code_m49, 2, 4),
                  area_code_m49 = as.numeric(area_code_m49),
                  item_code_cpc = gsub("'", "", item_code_cpc),
                  item_code_fao = item_code)
  #Save
  saveRDS(fao_production, file = file.path(data_folder, "fao_production.rds"))
  #Also save list linking the item names to their FAO codes and CPC codes
  fao_cpc_item_codes <- fao_production %>% dplyr::select(item, item_code_fao, item_code_cpc) %>% unique
  saveRDS(fao_cpc_item_codes, file = file.path(data_folder, "fao_cpc_item_codes.rds"))

  #Select only production and harvested area data
  prod_harvarea <- fao_production %>%
    dplyr::filter(element_code == 5510 | element_code == 5312)
  saveRDS(prod_harvarea, file = file.path(data_folder, "fao_production_crops.rds"))
  #Make a new column showing the iso3 code
  prod_harvarea <- prod_harvarea %>%
    dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
  # #Show areas missing ISO3 codes
  # missing_iso <- prod_harvarea %>%
  #   dplyr::filter(is.na(ISO3)) %>%
  #   dplyr::select(area, area_code_m49, area_code) %>%
  #   unique()
  #The bulk download includes total for the world and various regions, so we
  #drop all areas where the ISO3 code is missing
  prod_harvarea <- prod_harvarea %>%
    dplyr::filter(!is.na(ISO3))
  #Select necessary columns
  prod_harvarea <- prod_harvarea %>%
    dplyr::select(ISO3, year_code, item, item_code_cpc, item_code_fao, element, unit, value)
  #Save
  saveRDS(prod_harvarea, file.path(workdir_dir, 'workdir/gtap_ref_year/fao_prod_harvarea.rds'))

  # Number of livestock animals data - just need ruminants (ctl, rmk, wol GTAP categories)
  livestock_numanimals <- fao_production %>%
    dplyr::filter(element_code == 5111)
  #Make a new column showing the iso3 code
  livestock_numanimals <- livestock_numanimals %>%
    dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
  #The bulk download includes total for the world and various regions, so we
  #drop all areas where the ISO3 code is missing
  livestock_numanimals <- livestock_numanimals %>%
    dplyr::filter(!is.na(ISO3))
  #Select necessary columns
  livestock_numanimals <- livestock_numanimals %>%
    dplyr::select(ISO3, year_code, item, item_code_cpc, item_code_fao, element, unit, value)
  #Save
  saveRDS(livestock_numanimals, file.path(workdir_dir, 'workdir/gtap_ref_year/fao_livestock_numanimals.rds'))

  #### Prices ####
  # Bulk download prices - by choosing the "PP" code we select the Producer Price in USD/tonne
  producer_prices <- FAOSTAT::get_faostat_bulk(code = "PP", data_folder = data_folder)
  #Format columns, select price in USD/tonne, choose years
  producer_prices <- producer_prices %>%
    dplyr::filter(year_code == gtap_ref_year) %>%
    dplyr::rename(area_code_m49 = area_code__m49_,
                  item_code_cpc = item_code__cpc_) %>%
    dplyr::mutate(element_code = as.numeric(element_code),
                  area_code_m49 = substr(area_code_m49, 2, 4),
                  area_code_m49 = as.numeric(area_code_m49),
                  item_code_cpc = gsub("'", "", item_code_cpc),
                  item_code_fao = item_code) %>%
    dplyr::filter(element_code == 5532)
  saveRDS(producer_prices, file = file.path(data_folder, "fao_prices.rds"))
  #Make a new column showing the iso3 code
  producer_prices <- producer_prices %>%
    dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
  #The bulk download includes total for the world and regions, so we
  #drop all areas where the ISO3 code is missing
  producer_prices <- producer_prices %>%
    dplyr::filter(!is.na(ISO3))
  #Select necessary columns
  producer_prices <- producer_prices %>%
    dplyr::select(ISO3, year_code, item, item_code_cpc, item_code_fao, element, unit, value)
  #Save
  saveRDS(producer_prices, file.path(workdir_dir, 'workdir/gtap_ref_year/fao_producer_prices.rds'))

  #### Value of Agricultural Production ####
  #Bulk download value of production data for crops and livestock
  fao_val_production <- FAOSTAT::get_faostat_bulk(code = "QV", data_folder = data_folder)
  #Keep only specified GTAP reference year data, rename and reformat columns
  fao_val_production <- fao_val_production  %>%
    dplyr::filter(year_code == gtap_ref_year) %>%
    dplyr::rename(area_code_m49 = area_code__m49_,
                  item_code_cpc = item_code__cpc_) %>%
    dplyr::mutate(element_code = as.numeric(element_code),
                  area_code_m49 = substr(area_code_m49, 2, 4),
                  area_code_m49 = as.numeric(area_code_m49),
                  item_code_cpc = gsub("'", "", item_code_cpc),
                  item_code_fao = item_code) %>%
    dplyr::filter(element_code == 57)
  saveRDS(fao_val_production, file = file.path(data_folder, "fao_val_prod.rds"))
  #Make a new column showing the iso3 code
  fao_val_production <- fao_val_production %>%
    dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
  #The bulk download includes total for the world and regions, so we
  #drop all areas where the ISO3 code is missing
  fao_val_production <- fao_val_production %>%
    dplyr::filter(!is.na(ISO3))
  #Select necessary columns
  fao_val_production <- fao_val_production %>%
    dplyr::select(ISO3, year_code, item, item_code_cpc, item_code_fao, element, unit, value)
  #Save
  saveRDS(fao_val_production, file.path(workdir_dir, 'workdir/gtap_ref_year/fao_val_prod_1000usd.rds'))

  #### Land cover ####
  # Bulk download land cover data - 1000 hectares in arable land (item code 6621),
  #permanent crops (6650), and permanent meadows (6655), and total land area (6601)
  # item_list <- c("Arable land","Permanent crops", "Permanent meadows and pastures")
  item_code_list <- c(6621, 6650, 6655, 6601)
  land_cover <- FAOSTAT::get_faostat_bulk(code = "RL", data_folder = data_folder)
  #Format columns, select the three item codes, choose years
  land_cover <- land_cover %>%
    dplyr::filter(year_code == gtap_ref_year) %>%
    dplyr::rename(area_code_m49 = area_code__m49_) %>%
    dplyr::mutate(element_code = as.numeric(element_code),
                  area_code_m49 = substr(area_code_m49, 2, 4),
                  area_code_m49 = as.numeric(area_code_m49),
                  item_code_fao = item_code) %>%
    dplyr::filter(element_code == 5110) %>%
    dplyr::filter(item_code %in% item_code_list)
  saveRDS(land_cover, file = file.path(data_folder, "fao_land_cover.rds"))
  #Make a new column showing the iso3 code
  land_cover <- land_cover %>%
    dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
  #The bulk download includes total for the world and regions, so we
  #drop all areas where the ISO3 code is missing
  land_cover <- land_cover %>%
    dplyr::filter(!is.na(ISO3))
  #Pivot wider so we can make columns for cropland and pastureland
  land_cover_wide <- land_cover %>%
    dplyr::select(ISO3, year_code, item, value) %>%
    tidyr::pivot_wider(id_cols = c(ISO3, year_code),
                       names_from = item,
                       values_from =  value)
  #Rename categories, replace NA values with zeroes
  land_cover_wide <- land_cover_wide %>%
    dplyr::rename(arable = `Arable land`,
                  permanent_crop = `Permanent crops`,
                  pasture_ha = `Permanent meadows and pastures`,
                  land_area_ha = `Land area`) %>%
    dplyr::mutate(arable = ifelse(is.na(arable), 0, arable),
                  permanent_crop = ifelse(is.na(permanent_crop), 0, permanent_crop),
                  pasture_ha = ifelse(is.na(pasture_ha), 0, pasture_ha),
                  land_area_ha = ifelse(is.na(land_area_ha), 0, land_area_ha))
  #Now add up the categories to get the values for cropland and pasture.
  #Values are in 1000 ha, convert to ha
  land_cover_wide <- land_cover_wide %>%
    dplyr::mutate(cropland_ha = 1000*(arable + permanent_crop),
                  pasture_ha = 1000*pasture_ha,
                  land_area_ha = 1000*land_area_ha) %>%
    dplyr::select(ISO3, year_code, cropland_ha, pasture_ha, land_area_ha)
  #Save
  saveRDS(land_cover_wide, file.path(workdir_dir, 'workdir/gtap_ref_year/fao_land_cover.rds'))
}
