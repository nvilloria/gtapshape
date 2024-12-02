d <- function(){
  ## #Load in the set of biome names
  ##   set_BIO_list <- readRDS(file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
  ##   workdir_dir <- getwd()
  ## #### Need to do wol and rmk categories separately from ctl???
  ## #Load fao production data for chosen gtap year (2017)
  ## production_fao <- readRDS(file.path(workdir_dir, 'workdir/gtap_ref_year/fao_prod_harvarea.rds')) %>%
  ##   dplyr::filter(element == 'production') %>%
  ##   dplyr::rename(GADM = ISO3) %>%
  ##   dplyr::mutate(item_code_cpc_2 = substr(item_code_cpc, 1, 2),
  ##                 item_code_cpc_3 = substr(item_code_cpc, 1, 3),
  ##                 item_code_cpc_4 = substr(item_code_cpc, 1, 4),
  ##                 item_code_cpc_5 = substr(item_code_cpc, 1, 5))
  ##                                       #Load in the concordance linking CPC codes to their GSC3 GTAP sector codes
  ##   cpc_gsc3_concordance_file = "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/gtap_GSC_CPC_concordance.csv"

  ## CPC_GSC_conc <- read.csv(cpc_gsc3_concordance_file,
  ##                          header = T,
  ##                          colClasses = c(cpc_code = "character"))
  ## #Keep entries in concordance for three livestock categories
  ## CPC_GSC_conc <- CPC_GSC_conc %>%
  ##   dplyr::filter(GSC_code %in% c('ctl', 'rmk', 'wol')) %>%
  ##   dplyr::rename(GSC3 = GSC_code,
  ##                 item_code_cpc = cpc_code)
  ## #Make list of livestock categories and cpc codes in for each
  ## gsc3_list <- c('ctl', 'rmk', 'wol')
  ## gsc3_cpc_codes <- lapply(gsc3_list, function (x) {
  ##   gsc3_vec <- CPC_GSC_conc %>% dplyr::filter(GSC3==x) %>% dplyr::select(item_code_cpc)
  ##   gsc3_vec$item_code_cpc
  ## })
  ## names(gsc3_cpc_codes) <- gsc3_list
  ## #Assign GSC3 GTAP sector codes to FAO data
  ## fao_livestock_production <- production_fao %>%
  ##   dplyr::mutate(GSC3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ctl'])), 'ctl',
  ##                               ifelse(item_code_cpc_5 %in% c(unlist(gsc3_cpc_codes['ctl'])), 'ctl',
  ##                                      ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['rmk'])), 'rmk',
  ##                                             ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wol'])), 'wol', NA)))))
  ## #Drop entries without GSC3 code
  ## fao_livestock_production <- fao_livestock_production %>%
  ##   dplyr::filter(!is.na(GSC3))

  ## #Merge in prices
  ## producer_prices <- readRDS(file.path(workdir_dir, 'workdir/gtap_ref_year/fao_producer_prices.rds')) %>%
  ##   dplyr::rename(GADM = ISO3,
  ##                 price_usd_ton = value) %>%
  ##   dplyr::select(GADM, item_code_cpc, price_usd_ton)
  ## #Make a world price for when the prices are missing
  ## world_prices <- producer_prices %>%
  ##   dplyr::group_by(item_code_cpc) %>%
  ##   dplyr::summarise(world_price = mean(price_usd_ton, na.rm = T))
  ## #Merge the prices with the production data
  ## fao_livestock_production <- dplyr::left_join(fao_livestock_production, producer_prices, by = c('GADM','item_code_cpc'))
  ## fao_livestock_production <- dplyr::left_join(fao_livestock_production, world_prices, by = c('item_code_cpc'))
  ## #Replace missings with world value
  ## fao_livestock_production <- fao_livestock_production %>%
  ##   dplyr::mutate(price_usd_ton = ifelse(is.na(price_usd_ton), world_price, price_usd_ton)) %>%
  ##   dplyr::select(-world_price)
  ## #Calculate value of production
  ## fao_val_livestock_production <- fao_livestock_production %>%
  ##   dplyr::mutate(usd1000 = value*price_usd_ton/1000)

  ## #Load in livestock shares
  ##   lvstk_production_shares <- readRDS(file.path(workdir_dir, 'workdir/gadm_bio_lvstk_prod_shares.rds'))
  ##   head(lvstk_production_shares)
  ## #Make it wide
  ## lvstk_production_shares_wide <- lvstk_production_shares %>%
  ##   tidyr::pivot_wider(id_cols = c(GADM, GSC3), names_from = BIO, values_from = share_gadm_output)
  ## #Define a function to replace missing values with zeroes
  ## function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  ## lvstk_production_shares_wide <- lvstk_production_shares_wide %>%
  ##   dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  ## #Merge the shares with the fao data
  ## merged <- dplyr::left_join(fao_val_livestock_production, lvstk_production_shares_wide, by = c('GADM', 'GSC3'))
  ## #Drop the data without production shares
  ## merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  ## #Multiply the total head of animals by the BIO shares
  ## merged <- merged %>%
  ##   dplyr::mutate(across(set_BIO_list, ~.*usd1000))

  ## #Pivot to long format
  ## merged_long <- merged %>% dplyr::select(-usd1000) %>%
  ##   tidyr::pivot_longer(cols = set_BIO_list,
  ##                       values_to = 'usd1000',
  ##                       names_to = 'BIO')
  ## #Save
  ## saveRDS(merged_long, file = file.path(workdir_dir, 'workdir/gtap_ref_year/gadm_bio_vallvstkprod_rmk_wol.rds'))

  ##   ## I AM HERE:
  ## #### Livestock production - 4 ruminant species ####
  ## rm(list=ls()[! ls() %in% c("workdir_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  ## gc()
  ## #Load in the set of biome names
  ## set_BIO_list <- readRDS(file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
  ## #Load fao data for chosen gtap year
  ## fao_livestock_production <- readRDS(file.path(workdir_dir, 'workdir/gtap_ref_year/fao_livestock_numanimals.rds')) %>%
  ##   dplyr::rename(GADM = ISO3)
  ## #Drop categories with multiple items and ones we don't need
  ## fao_livestock_production <- fao_livestock_production %>%
  ##   dplyr::filter(item != "Cattle and Buffaloes" &
  ##                   item != "Sheep and Goats" &
  ##                   item != "Swine / pigs")
  ## #List the 4 species which make up 'ctl' in gtaplulc18.har dataset?
  ## ctl_list <- c('Buffalo', 'Cattle', 'Goats', 'Sheep')
  ## #Add up animals in each category by region and year
  ## ctl_fao_livestock_production <- fao_livestock_production %>%
  ##   dplyr::filter(item %in% ctl_list) %>%
  ##   dplyr::group_by(GADM, item) %>%
  ##   dplyr::summarise(value = sum(value)) %>%
  ##   dplyr::ungroup() %>%
  ##   dplyr::rename(LVSTCK = item) %>%
  ##   dplyr::mutate(GSC3 = 'ctl') %>%
  ##   dplyr::ungroup()

  ## #Load in livestock shares
  ## lvstk_production_shares <- readRDS(file.path(workdir_dir, 'workdir/gadm_bio_lvstk_prod_shares.rds'))
  ## #Make it wide
  ## lvstk_production_shares_wide <- lvstk_production_shares %>%
  ##   tidyr::pivot_wider(id_cols = c(GADM, GSC3), names_from = BIO, values_from = share_gadm_output)
  ## #Define a function to replace missing values with zeroes
  ## function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  ## lvstk_production_shares_wide <- lvstk_production_shares_wide %>%
  ##   dplyr::mutate(across(set_BIO_list, function_na_to_zero))

  ## #Merge the shares with the fao data
  ## merged <- dplyr::left_join(ctl_fao_livestock_production, lvstk_production_shares_wide, by = c('GADM', 'GSC3'))
  ## #Drop the data without production shares
  ## merged <- merged %>% dplyr::filter(!is.na(set_BIO_list[[1]]))

  ## #Multiply the total head of animals by the BIO shares
  ## merged <- merged %>%
  ##   dplyr::mutate(across(set_BIO_list, ~.*value/1000))

  ## #Pivot to long format
  ## merged_long <- merged %>% dplyr::select(-value) %>%
  ##   tidyr::pivot_longer(cols = set_BIO_list,
  ##                       values_to = 'head1000',
  ##                       names_to = 'BIO')
  ## #Save
  ## saveRDS(merged_long, file = file.path(workdir_dir, 'workdir/gtap_ref_year/gadm_bio_livestock_production_4species.rds'))

  ## #### Value of livestock production - 4 ruminant species ####
  ## rm(list=ls()[! ls() %in% c("workdir_dir", "lvstk_prices_4spec_file",  "cpc_gsc3_concordance_file")])
  ## gc()
  ## #Load in the set of biome names
  ## set_BIO_list <- readRDS(file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
  ## #Load production data from above
  ## val_lvstk_prod_4spec <- readRDS(file.path(workdir_dir, 'workdir/gtap_ref_year/gadm_bio_livestock_production_4species.rds'))
  ##                                       #Load in prices calculated using gtaplulc18.har file

  ##   lvstk_prices_4spec_file = "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/gtaplulc18/lvstk_prices_4spec.csv"

  ## lvstk_prices_4spec <- read.csv(lvstk_prices_4spec_file, header = T) %>%
  ##   dplyr::rename("GADM" = "QLIVE_18",
  ##                 "Buffalo" = "X1.BUFFALOES",
  ##                 "Cattle" = "X2.CATTLE",
  ##                 "Goats" = "X3.GOATS",
  ##                 "Sheep" = "X4.SHEEP")
  ## #Make function to get last three characters of REG values
  ## substrRight <- function(x, n){
  ##   substr(x, nchar(x)-n+1, nchar(x))
  ## }
  ## lvstk_prices_4spec <- lvstk_prices_4spec %>%
  ##   dplyr::mutate(GADM = substrRight(GADM, 3),
  ##                 GADM = toupper(GADM))
  ## #Pivot to long
  ## lvstk_prices_4spec <- lvstk_prices_4spec %>%
  ##   tidyr::pivot_longer(cols = Buffalo:Sheep,
  ##                       values_to = 'price',
  ##                       names_to = 'LVSTCK')
  ## #Replace the "#DIV/0!" values with NA and destring
  ## lvstk_prices_4spec <- lvstk_prices_4spec %>%
  ##   dplyr::mutate(price = ifelse(price == "#DIV/0!", NA, price),
  ##                 price = as.numeric(price))
  ## #Make a world price for when the prices are missing
  ## lvstk_world_prices_4spec <- lvstk_prices_4spec %>%
  ##   dplyr::group_by(LVSTCK) %>%
  ##   dplyr::summarise(world_price = mean(price, na.rm = T))
  ## #Merge the prices with the production data
  ## val_lvstk_prod_4spec <- dplyr::left_join(val_lvstk_prod_4spec, lvstk_prices_4spec, by = c('GADM', 'LVSTCK'))
  ## val_lvstk_prod_4spec <- dplyr::left_join(val_lvstk_prod_4spec, lvstk_world_prices_4spec, by = c('LVSTCK'))
  ## #Replace missings with world value
  ## val_lvstk_prod_4spec <- val_lvstk_prod_4spec %>%
  ##   dplyr::mutate(price = ifelse(is.na(price), world_price, price)) %>%
  ##   dplyr::select(-world_price)

  ## #Calculate value of production
  ## val_lvstk_prod_4spec <- val_lvstk_prod_4spec %>%
  ##   dplyr::mutate(usd1000 = head1000*1000*price/1000)

  ## #Save
  ## saveRDS(val_lvstk_prod_4spec, file = file.path(workdir_dir, 'workdir/gtap_ref_year/gadm_bio_livestock_valprod_4species.rds'))

  #### Combine 'ctl', 'rmk', and 'wol' data ####
  rmk_wol_valprod <- readRDS(file.path(workdir_dir, 'workdir/gtap_ref_year/gadm_bio_vallvstkprod_rmk_wol.rds')) %>%
    dplyr::select(GADM, BIO, GSC3, usd1000)
  #Sum val of prod across 4 ctl species
  ctl_valprod <- val_lvstk_prod_4spec %>%
    dplyr::group_by(GADM, BIO, GSC3) %>%
    dplyr::summarise(usd1000 = sum(usd1000, na.rm = T)) %>%
    dplyr::select(GADM, BIO, GSC3, usd1000)
  #Combine the three categories
  val_lvstk_prod <- rbind(ctl_valprod, rmk_wol_valprod)
  #Save
  saveRDS(val_lvstk_prod, file = file.path(workdir_dir, 'workdir/gtap_ref_year/gadm_bio_livestock_valprod_ctlrmkwol.rds'))
}
