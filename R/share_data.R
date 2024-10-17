#' Share out land rents
#'
#' This function uses the country-BIOME share variables to disaggregate the fao
#' data for the chosen GTAP reference year. National level data from the FAO
#' are first split by subnational division, and then we aggregate to the REG
#' -subnational division level using the aggr function.
#'
#' @param lvstk_prices_4spec_file File name of file containing prices for four
#'    livestock species. Should have five columns: "QLIVE_18", "X1.BUFFALOES",
#'    "X2.CATTLE", "X3.GOATS", "X4.SHEEP")
#' @param cpc_gsc3_concordance_file File name of file containing concordance
#'    which links CPC codes to their GSC3 GTAP sector codes
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @return Saves .rds files containing land cover, crop production, livestock
#'  production, and value of production data for country and subnational
#'  area combinations.
#' @export
share_data <- function(lvstk_prices_4spec_file, cpc_gsc3_concordance_file, tmp_dir=getwd()) {
  #### Land Cover ####
  #Read in data for the year specified
  landcov_2017 <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_land_cover.rds')) %>%
    dplyr::rename(GADM = ISO3)
  #Read in the shares
  #NOTE - If a country is missing shares, it means that we did not have
  #cropland, pastureland, and other land type data. So we keep the original
  #base year acreage instead of updating the data.
  landcov_shares <- readRDS(file.path(tmp_dir, 'tmp/gadm_bio_landcov_shares.rds')) %>%
    dplyr::select(GADM, BIO, country_total_urban, starts_with('lc_share'), starts_with('bio_share')) %>%
    unique()
  #Join to the land cover data by GADM
  landcov_update_2017 <- dplyr::left_join(landcov_shares, landcov_2017, by = c('GADM'))
  #Make a variable showing how much land area needs to be filled in after
  #accounting for cropland, pasture, and urban areas. Then use this to
  #update the non-cropland/pasture land cover areas
  ###### URBAN DOESNT CHANGE ####
  landcov_update_2017 <- landcov_update_2017 %>%
    dplyr::mutate(fillin_ha = land_area_ha - cropland_ha - pasture_ha - country_total_urban,
                  lc_share_noncroppasturban = lc_share_country_forest +
                    lc_share_country_other + lc_share_country_grassland +
                    lc_share_country_shrubland,
                  country_total_cropland = cropland_ha,
                  country_total_pasture = pasture_ha,
                  country_total_forest = fillin_ha*(lc_share_country_forest/lc_share_noncroppasturban),
                  country_total_other = fillin_ha*(lc_share_country_other/lc_share_noncroppasturban),
                  country_total_grassland = fillin_ha*(lc_share_country_grassland/lc_share_noncroppasturban),
                  country_total_shrubland = fillin_ha*(lc_share_country_shrubland/lc_share_noncroppasturban))

  #Now share out countries' total acreage by BIOME
  landcov_update_2017 <- landcov_update_2017 %>%
    dplyr::mutate(cropland_ha = country_total_cropland*bio_share_country_cropland,
                  pasture_ha = country_total_pasture*bio_share_country_pasture,
                  forest_ha = country_total_forest*bio_share_country_forest,
                  urban_ha = country_total_urban*bio_share_country_urban,
                  other_ha = country_total_other*bio_share_country_other,
                  grassland_ha = country_total_grassland*bio_share_country_grassland,
                  shrubland_ha = country_total_shrubland*bio_share_country_shrubland)

  #Keep only the land cover data for the countries that we updated
  bio_fao_updated <- landcov_update_2017 %>%
    dplyr::select(GADM, BIO, cropland_ha, pasture_ha,
                  forest_ha, urban_ha, grassland_ha, shrubland_ha, other_ha)

  #Now we need to add in the countries which will not have updated land
  #cover data because they do not have cropland, pastureland, and
  #other land cover data in the base year
  base_lc_df <- readRDS(file.path(tmp_dir, 'tmp/base_year/GADM_BIO_landcover_rasterdata_ha.rds')) %>%
    dplyr::mutate(GADM = substr(GADM_BIO, 1, 3),
                  BIO = substr(GADM_BIO, 5, nchar(paste0(GADM_BIO)))) %>%
    dplyr::select(GADM, BIO, GADM_BIO, everything())
  #Make a not in operator
  '%!in%' <- function(x,y)!('%in%'(x,y))
  #Keep ones not being updated
  gadm_bio_NONupdated <- base_lc_df %>%
    dplyr::filter(GADM %!in% bio_fao_updated$GADM) %>%
    dplyr::select(GADM, BIO, cropland_ha, pasture_ha,
                  forest_ha, urban_ha, grassland_ha, shrubland_ha, other_ha)
  #Combine with updated GADM-BIO combinations
  gadm_bio_land_cover_output <- rbind(bio_fao_updated, gadm_bio_NONupdated)
  # #Test to see that we have data for all GADM-BIO combinations in the base
  #   #year data
  # base_count_gadm_bio <- base_lc_df %>%
  #   dplyr::select(GADM_BIO) %>% unique()

  #Replace NaN and NA values with zeroes
  gadm_bio_land_cover_output <- gadm_bio_land_cover_output %>%
    dplyr::mutate(across(cropland_ha:other_ha, ~ ifelse(is.nan(.x), 0, .x))) %>%
    dplyr::mutate(across(cropland_ha:other_ha, ~ ifelse(is.na(.x), 0, .x)))

  # #Drop countries with no data for all land cover types
  # gadm_bio_land_cover_output <- gadm_bio_land_cover_output %>%
  #   dplyr::mutate(all_lcov_ha = rowSums(across(cropland_ha:other_ha)))

  #Save
  saveRDS(gadm_bio_land_cover_output, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_land_cover.rds'))

  #### Crop production ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load in the set of biome names
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))
  #Load fao data for chosen gtap year (2017)
  crop_production_fao <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_prod_harvarea.rds')) %>%
    dplyr::filter(element == 'production') %>%
    dplyr::rename(GADM = ISO3)
  #Load the GADM-BIOME shares for crop production
  crop_production_shares <- readRDS(file = file.path(tmp_dir, 'tmp/gadm_bio_crop_prod_shares.rds'))
  #Make it wide
  crop_production_shares_wide <- crop_production_shares %>%
    tidyr::pivot_wider(id_cols = c(GADM, crop, item_code_cpc), names_from = BIO, values_from = share_gadm_output)
  #Define a function to replace missing values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  crop_production_shares_wide <- crop_production_shares_wide %>%
    dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  #Merge the shares with the fao data for 2017
  merged <- dplyr::left_join(crop_production_fao, crop_production_shares_wide, by = c('GADM', 'item_code_cpc'))
  #Drop the data without production shares
  merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  #Multiply the tons of production by the BIO shares
  merged <- merged %>%
    dplyr::mutate(across(set_BIO_list, ~.*value))

  #Pivot to long format
  merged_long <- merged %>% dplyr::select(-value) %>%
    tidyr::pivot_longer(cols = set_BIO_list,
                        values_to = 'tons',
                        names_to = 'BIO')
  #Save
  saveRDS(merged_long, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_crop_production.rds'))

  #### Value of crop production ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load the crop production data just created at the GADM-BIO intersection
  crop_val_production <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_crop_production.rds'))
  #Merge in prices
  producer_prices <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_producer_prices.rds')) %>%
    dplyr::rename(GADM = ISO3,
                  price_usd_ton = value) %>%
    dplyr::select(GADM, item_code_cpc, price_usd_ton)
  #Make a world price for when the prices are missing
  crop_world_price <- producer_prices %>%
    dplyr::group_by(item_code_cpc) %>%
    dplyr::summarise(world_price = mean(price_usd_ton, na.rm = T))
  #Merge the prices with the production data
  crop_val_production <- dplyr::left_join(crop_val_production, producer_prices, by = c('GADM','item_code_cpc'))
  crop_val_production <- dplyr::left_join(crop_val_production, crop_world_price, by = c('item_code_cpc'))

  #Replace missings with world value
  crop_val_production <- crop_val_production %>%
    dplyr::mutate(price_usd_ton = ifelse(is.na(price_usd_ton), world_price, price_usd_ton)) %>%
    dplyr::select(-world_price)

  #Calculate value of production
  crop_val_production <- crop_val_production %>%
    dplyr::mutate(usd1000 = tons*price_usd_ton/1000)
  #Save
  saveRDS(crop_val_production, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_val_crop_production.rds'))


  #### Harvested Area ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load in the set of biome names
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))
  #Load fao data for chosen gtap year (2017)
  harv_area_fao <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_prod_harvarea.rds')) %>%
    dplyr::filter(element == 'area_harvested') %>%
    dplyr::rename(GADM = ISO3)
  #Load the GADM-BIOME shares for crop production
  crop_production_shares <- readRDS(file = file.path(tmp_dir, 'tmp/gadm_bio_crop_prod_shares.rds'))
  #Make it wide
  crop_production_shares_wide <- crop_production_shares %>%
    tidyr::pivot_wider(id_cols = c(GADM, crop, item_code_cpc), names_from = BIO, values_from = share_gadm_output)
  #Define a function to replace missing values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  crop_production_shares_wide <- crop_production_shares_wide %>%
    dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  #Merge the shares with the fao data for 2017
  merged <- dplyr::left_join(harv_area_fao, crop_production_shares_wide, by = c('GADM', 'item_code_cpc'))
  #Drop the data without production shares
  merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  #Multiply the hectares of crop area by the BIO shares
  merged <- merged %>%
    dplyr::mutate(across(set_BIO_list, ~.*value))

  #Pivot to long format
  merged_long <- merged %>% dplyr::select(-value) %>%
    tidyr::pivot_longer(cols = set_BIO_list,
                        values_to = 'ha',
                        names_to = 'BIO')
  #Save
  saveRDS(merged_long, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_crop_harvarea.rds'))

  #### Livestock number of animals ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load in the set of biome names
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))
  #Load fao data for chosen gtap year (2017)
  fao_livestock_numanimals <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_livestock_numanimals.rds')) %>%
    dplyr::rename(GADM = ISO3)
  #Drop categories with multiple items and ones we don't need
  fao_livestock_numanimals <- fao_livestock_numanimals %>%
    dplyr::filter(item != "Cattle and Buffaloes" &
                    item != "Sheep and Goats" &
                    item != "Swine / pigs")
  #Define the list of animals in each RUMINANT Category
  ctl_list <- c('Cattle', 'Buffalo', 'Camels', 'Other camelids', 'Sheep', 'Goats', 'Horses', 'Asses', 'Mules and hinnies')
  rmk_list <- c('Cattle', 'Buffalo', 'Camels', 'Other camelids', 'Sheep', 'Goats')
  wol_list <- c('Sheep', 'Goats')
  #Add up animals in each category by region and year
  ctl_fao_livestock_numanimals <- fao_livestock_numanimals %>%
    dplyr::filter(item %in% ctl_list) %>%
    dplyr::group_by(GADM) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GSC3 = 'ctl')
  rmk_fao_livestock_numanimals <- fao_livestock_numanimals %>%
    dplyr::filter(item %in% rmk_list) %>%
    dplyr::group_by(GADM) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GSC3 = 'rmk')
  wol_fao_livestock_numanimals <- fao_livestock_numanimals %>%
    dplyr::filter(item %in% wol_list) %>%
    dplyr::group_by(GADM) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GSC3 = 'wol')
  #Combine the three into one dataframe
  fao_livestock_numanimals <- rbind(ctl_fao_livestock_numanimals, rmk_fao_livestock_numanimals, wol_fao_livestock_numanimals)
  #Divide number of animals by 1000
  fao_livestock_numanimals <- fao_livestock_numanimals %>%
    dplyr::mutate(head1000 = value/1000) %>%
    dplyr::select(-value)

  #Load in livestock shares
  lvstk_production_shares <- readRDS(file.path(tmp_dir, 'tmp/gadm_bio_lvstk_prod_shares.rds'))
  #Make it wide
  lvstk_production_shares_wide <- lvstk_production_shares %>%
    tidyr::pivot_wider(id_cols = c(GADM, GSC3), names_from = BIO, values_from = share_gadm_output)
  #Define a function to replace missing values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  lvstk_production_shares_wide <- lvstk_production_shares_wide %>%
    dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  #Merge the shares with the fao data
  merged <- dplyr::left_join(fao_livestock_numanimals, lvstk_production_shares_wide, by = c('GADM', 'GSC3'))
  #Drop the data without production shares
  merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  #Multiply the total head of animals by the BIO shares
  merged <- merged %>%
    dplyr::mutate(across(set_BIO_list, ~.*head1000))

  #Pivot to long format
  merged_long <- merged %>% dplyr::select(-head1000) %>%
    tidyr::pivot_longer(cols = set_BIO_list,
                        values_to = 'head1000',
                        names_to = 'BIO')
  #Save
  saveRDS(merged_long, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_numanimals.rds'))

  #### Value of livestock production - wol and rmk ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load in the set of biome names
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))
  #### Need to do wol and rmk categories separately from ctl???
  #Load fao production data for chosen gtap year (2017)
  production_fao <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_prod_harvarea.rds')) %>%
    dplyr::filter(element == 'production') %>%
    dplyr::rename(GADM = ISO3) %>%
    dplyr::mutate(item_code_cpc_2 = substr(item_code_cpc, 1, 2),
                  item_code_cpc_3 = substr(item_code_cpc, 1, 3),
                  item_code_cpc_4 = substr(item_code_cpc, 1, 4),
                  item_code_cpc_5 = substr(item_code_cpc, 1, 5))
  #Load in the concordance linking CPC codes to their GSC3 GTAP sector codes
  CPC_GSC_conc <- read.csv(cpc_gsc3_concordance_file,
                           header = T,
                           colClasses = c(cpc_code = "character"))
  #Keep entries in concordance for three livestock categories
  CPC_GSC_conc <- CPC_GSC_conc %>%
    dplyr::filter(GSC_code %in% c('ctl', 'rmk', 'wol')) %>%
    dplyr::rename(GSC3 = GSC_code,
                  item_code_cpc = cpc_code)
  #Make list of livestock categories and cpc codes in for each
  gsc3_list <- c('ctl', 'rmk', 'wol')
  gsc3_cpc_codes <- lapply(gsc3_list, function (x) {
    gsc3_vec <- CPC_GSC_conc %>% dplyr::filter(GSC3==x) %>% dplyr::select(item_code_cpc)
    gsc3_vec$item_code_cpc
  })
  names(gsc3_cpc_codes) <- gsc3_list
  #Assign GSC3 GTAP sector codes to FAO data
  fao_livestock_production <- production_fao %>%
    dplyr::mutate(GSC3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ctl'])), 'ctl',
                                ifelse(item_code_cpc_5 %in% c(unlist(gsc3_cpc_codes['ctl'])), 'ctl',
                                       ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['rmk'])), 'rmk',
                                              ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wol'])), 'wol', NA)))))
  #Drop entries without GSC3 code
  fao_livestock_production <- fao_livestock_production %>%
    dplyr::filter(!is.na(GSC3))

  #Merge in prices
  producer_prices <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_producer_prices.rds')) %>%
    dplyr::rename(GADM = ISO3,
                  price_usd_ton = value) %>%
    dplyr::select(GADM, item_code_cpc, price_usd_ton)
  #Make a world price for when the prices are missing
  world_prices <- producer_prices %>%
    dplyr::group_by(item_code_cpc) %>%
    dplyr::summarise(world_price = mean(price_usd_ton, na.rm = T))
  #Merge the prices with the production data
  fao_livestock_production <- dplyr::left_join(fao_livestock_production, producer_prices, by = c('GADM','item_code_cpc'))
  fao_livestock_production <- dplyr::left_join(fao_livestock_production, world_prices, by = c('item_code_cpc'))
  #Replace missings with world value
  fao_livestock_production <- fao_livestock_production %>%
    dplyr::mutate(price_usd_ton = ifelse(is.na(price_usd_ton), world_price, price_usd_ton)) %>%
    dplyr::select(-world_price)
  #Calculate value of production
  fao_val_livestock_production <- fao_livestock_production %>%
    dplyr::mutate(usd1000 = value*price_usd_ton/1000)

  #Load in livestock shares
  lvstk_production_shares <- readRDS(file.path(tmp_dir, 'tmp/gadm_bio_lvstk_prod_shares.rds'))
  #Make it wide
  lvstk_production_shares_wide <- lvstk_production_shares %>%
    tidyr::pivot_wider(id_cols = c(GADM, GSC3), names_from = BIO, values_from = share_gadm_output)
  #Define a function to replace missing values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  lvstk_production_shares_wide <- lvstk_production_shares_wide %>%
    dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  #Merge the shares with the fao data
  merged <- dplyr::left_join(fao_val_livestock_production, lvstk_production_shares_wide, by = c('GADM', 'GSC3'))
  #Drop the data without production shares
  merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  #Multiply the total head of animals by the BIO shares
  merged <- merged %>%
    dplyr::mutate(across(set_BIO_list, ~.*usd1000))

  #Pivot to long format
  merged_long <- merged %>% dplyr::select(-usd1000) %>%
    tidyr::pivot_longer(cols = set_BIO_list,
                        values_to = 'usd1000',
                        names_to = 'BIO')
  #Save
  saveRDS(merged_long, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_vallvstkprod_rmk_wol.rds'))

  #### Livestock production - 4 ruminant species ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load in the set of biome names
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))
  #Load fao data for chosen gtap year
  fao_livestock_production <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/fao_livestock_numanimals.rds')) %>%
    dplyr::rename(GADM = ISO3)
  #Drop categories with multiple items and ones we don't need
  fao_livestock_production <- fao_livestock_production %>%
    dplyr::filter(item != "Cattle and Buffaloes" &
                    item != "Sheep and Goats" &
                    item != "Swine / pigs")
  #List the 4 species which make up 'ctl' in gtaplulc18.har dataset?
  ctl_list <- c('Buffalo', 'Cattle', 'Goats', 'Sheep')
  #Add up animals in each category by region and year
  ctl_fao_livestock_production <- fao_livestock_production %>%
    dplyr::filter(item %in% ctl_list) %>%
    dplyr::group_by(GADM, item) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(LVSTCK = item) %>%
    dplyr::mutate(GSC3 = 'ctl') %>%
    dplyr::ungroup()

  #Load in livestock shares
  lvstk_production_shares <- readRDS(file.path(tmp_dir, 'tmp/gadm_bio_lvstk_prod_shares.rds'))
  #Make it wide
  lvstk_production_shares_wide <- lvstk_production_shares %>%
    tidyr::pivot_wider(id_cols = c(GADM, GSC3), names_from = BIO, values_from = share_gadm_output)
  #Define a function to replace missing values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  lvstk_production_shares_wide <- lvstk_production_shares_wide %>%
    dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  #Merge the shares with the fao data
  merged <- dplyr::left_join(ctl_fao_livestock_production, lvstk_production_shares_wide, by = c('GADM', 'GSC3'))
  #Drop the data without production shares
  merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  #Multiply the total head of animals by the BIO shares
  merged <- merged %>%
    dplyr::mutate(across(set_BIO_list, ~.*value/1000))

  #Pivot to long format
  merged_long <- merged %>% dplyr::select(-value) %>%
    tidyr::pivot_longer(cols = set_BIO_list,
                        values_to = 'head1000',
                        names_to = 'BIO')
  #Save
  saveRDS(merged_long, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_production_4species.rds'))

  #### Value of livestock production - 4 ruminant species ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  gc()
  #Load in the set of biome names
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))
  #Load production data from above
  val_lvstk_prod_4spec <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_production_4species.rds'))
  #Load in prices calculated using gtaplulc18.har file
  lvstk_prices_4spec <- read.csv(lvstk_prices_4spec_file, header = T) %>%
    dplyr::rename("GADM" = "QLIVE_18",
                  "Buffalo" = "X1.BUFFALOES",
                  "Cattle" = "X2.CATTLE",
                  "Goats" = "X3.GOATS",
                  "Sheep" = "X4.SHEEP")
  #Make function to get last three characters of REG values
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  lvstk_prices_4spec <- lvstk_prices_4spec %>%
    dplyr::mutate(GADM = substrRight(GADM, 3),
                  GADM = toupper(GADM))
  #Pivot to long
  lvstk_prices_4spec <- lvstk_prices_4spec %>%
    tidyr::pivot_longer(cols = Buffalo:Sheep,
                        values_to = 'price',
                        names_to = 'LVSTCK')
  #Replace the "#DIV/0!" values with NA and destring
  lvstk_prices_4spec <- lvstk_prices_4spec %>%
    dplyr::mutate(price = ifelse(price == "#DIV/0!", NA, price),
                  price = as.numeric(price))
  #Make a world price for when the prices are missing
  lvstk_world_prices_4spec <- lvstk_prices_4spec %>%
    dplyr::group_by(LVSTCK) %>%
    dplyr::summarise(world_price = mean(price, na.rm = T))
  #Merge the prices with the production data
  val_lvstk_prod_4spec <- dplyr::left_join(val_lvstk_prod_4spec, lvstk_prices_4spec, by = c('GADM', 'LVSTCK'))
  val_lvstk_prod_4spec <- dplyr::left_join(val_lvstk_prod_4spec, lvstk_world_prices_4spec, by = c('LVSTCK'))
  #Replace missings with world value
  val_lvstk_prod_4spec <- val_lvstk_prod_4spec %>%
    dplyr::mutate(price = ifelse(is.na(price), world_price, price)) %>%
    dplyr::select(-world_price)

  #Calculate value of production
  val_lvstk_prod_4spec <- val_lvstk_prod_4spec %>%
    dplyr::mutate(usd1000 = head1000*1000*price/1000)

  #Save
  saveRDS(val_lvstk_prod_4spec, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_valprod_4species.rds'))

  #### Combine 'ctl', 'rmk', and 'wol' data ####
  rmk_wol_valprod <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_vallvstkprod_rmk_wol.rds')) %>%
    dplyr::select(GADM, BIO, GSC3, usd1000)
  #Sum val of prod across 4 ctl species
  ctl_valprod <- val_lvstk_prod_4spec %>%
    dplyr::group_by(GADM, BIO, GSC3) %>%
    dplyr::summarise(usd1000 = sum(usd1000, na.rm = T)) %>%
    dplyr::select(GADM, BIO, GSC3, usd1000)
  #Combine the three categories
  val_lvstk_prod <- rbind(ctl_valprod, rmk_wol_valprod)
  #Save
  saveRDS(val_lvstk_prod, file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_valprod_ctlrmkwol.rds'))
}
