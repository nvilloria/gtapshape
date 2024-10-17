#' Aggregate data to GTAP commodities
#'
#' This function aggregates the country-subnational division data to the
#' regions defined in the REG set made by the define_sets function. It also
#' aggregates crop production and the value of crop production to the 9
#' GTAP crop categories.
#'
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @param reg_country_conc_file Location of a file linking the GADM countries to
#'    the GTAP regions specified
#' @param cpc_gsc3_concordance_file File name of file containing concordance
#'    which links CPC codes to their GSC3 GTAP sector codes
#' @return Saves .rds files containing aggregated data.
#' @export
aggr <- function(tmp_dir=getwd(), reg_country_conc_file, cpc_gsc3_concordance_file) {
  #Load in the REG-GADM concordance
  reg_country_conc <- read.csv(reg_country_conc_file, header = TRUE) %>%
    dplyr::select(-name) %>%
    dplyr::rename(GADM = ISO3,
                  REG = GTAP_160)

  #### Land cover ####
  land_cov <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_land_cover.rds')) %>%
    dplyr::mutate(GADM = tolower(GADM))
  #Merge with the REG-GADM concordance
  land_cov <- dplyr::left_join(land_cov, reg_country_conc, by = c('GADM'))
  # #Test for missing GTAP REG
  # test <- land_cov %>%
  #   dplyr::select(GADM, REG) %>%
  #   unique() %>%
  #   dplyr::filter(is.na(REG))
  #Drop those without GTAP 160 REG
  land_cov <- land_cov %>%
    dplyr::filter(!is.na(REG))
  #Add up by REG
  land_cov <- land_cov %>%
    group_by(REG, BIO) %>%
    summarise(across(cropland_ha:other_ha, sum)) %>%
    ungroup()
  #Save
  saveRDS(land_cov, file = file.path(tmp_dir, 'tmp/output_data/reg_bio_land_cover.rds'))

  #### Crop production ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "reg_country_conc", "cpc_gsc3_concordance_file")])
  gc()
  #Load in crop production data
  crop_prod <- readRDS(file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_crop_production.rds')) %>%
    dplyr::mutate(GADM = tolower(GADM))
  #Merge with the REG-GADM concordance
  crop_prod <- dplyr::left_join(crop_prod, reg_country_conc, by = c('GADM'))
  # #Test for missing GTAP REG
  # test <- crop_prod %>%
  #   dplyr::select(GADM, REG) %>%
  #   unique() %>%
  #   dplyr::filter(is.na(REG))
  #Add up by REG
  crop_prod <- crop_prod %>%
    dplyr::group_by(REG, BIO, crop, item_code_cpc) %>%
    dplyr::summarise(tons = sum(tons, na.rm = T)) %>%
    dplyr::ungroup()
  # Aggregate within 9 GTAP CROP categories #
  #Create new cpc codes for specific number of digits
  crop_prod <- crop_prod %>%
    dplyr::mutate(item_code_cpc_2 = substr(item_code_cpc, 1, 2),
                  item_code_cpc_3 = substr(item_code_cpc, 1, 3),
                  item_code_cpc_4 = substr(item_code_cpc, 1, 4),
                  item_code_cpc_5 = substr(item_code_cpc, 1, 5))
  #Now add in GTAP GSC3 sectors
  #Load in the concordance linking cpc codes to their GTAP commodity categories
  #also known as GSC sectors
  cpc_gsc3_concordance <- read.csv(cpc_gsc3_concordance_file,
                           header = T,
                           colClasses = c(cpc_code = "character"))
  gsc3_list <- cpc_gsc3_concordance$GSC_code %>% unique()
  gsc3_cpc_codes <- lapply(gsc3_list, function (x) {
    gsc3_vec <- cpc_gsc3_concordance %>% dplyr::filter(GSC_code==x) %>% dplyr::select(cpc_code)
    gsc3_vec$cpc_code
  })
  names(gsc3_cpc_codes) <- gsc3_list
  #Assign GTAP codes
  crop_prod_GSC3 <- crop_prod %>%
    dplyr::mutate(GSC3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pdr'])), 'pdr',
                                ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wht'])), 'wht',
                                       ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['gro'])), 'gro',
                                              ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['v_f'])), 'v_f',
                                                     ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['osd'])), 'osd',
                                                            ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['c_b'])), 'c_b',
                                                                   ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pfb'])), 'pfb',
                                                                          ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr',
                                                                                 ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr', NA))))))))))
  #Check for missing values
  missing_GSC3 <- crop_prod_GSC3 %>% dplyr::filter(is.na(GSC3)) %>% dplyr::select(crop) %>% unique()
  #Only rapeseed is missing value
  crop_prod_GSC3 <- crop_prod_GSC3 %>%
    dplyr::mutate(GSC3 = ifelse(crop == "rapeseed", 'osd', GSC3)) %>%
    dplyr::filter(!is.na(GSC3))
  #Summarise by GSC3 within REG-BIO combinations
  crop_prod_GSC3 <- crop_prod_GSC3 %>%
    dplyr::group_by(REG, BIO, GSC3) %>%
    dplyr::summarise(tons = sum(tons, na.rm = T)) %>%
    dplyr::ungroup()
  #Save
  saveRDS(crop_prod_GSC3, file = file.path(tmp_dir, 'tmp/output_data/reg_bio_crop_prod.rds'))

  #### Value of crop production ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "reg_country_conc", "cpc_gsc3_concordance", "gsc3_cpc_codes")])
  gc()
  #Load in crop production at country-BIOME level
  crop_valprod <- readRDS(file = file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_val_crop_production.rds')) %>%
    dplyr::mutate(GADM = tolower(GADM))
  #Merge with the REG-GADM concordance
  crop_valprod <- dplyr::left_join(crop_valprod, reg_country_conc, by = c('GADM'))
  # #Test for missing GTAP REG
  # test <- crop_valprod %>%
  #   dplyr::select(GADM, REG) %>%
  #   unique() %>%
  #   dplyr::filter(is.na(REG))
  #Add up by REG
  crop_valprod <- crop_valprod %>%
    dplyr::group_by(REG, BIO, crop, item_code_cpc) %>%
    dplyr::summarise(usd1000 = sum(usd1000, na.rm = T)) %>%
    dplyr::ungroup()
  # Aggregate within 9 GTAP CROP categories #
  #Create new cpc codes for specific number of digits
  crop_valprod <- crop_valprod %>%
    dplyr::mutate(item_code_cpc_2 = substr(item_code_cpc, 1, 2),
                  item_code_cpc_3 = substr(item_code_cpc, 1, 3),
                  item_code_cpc_4 = substr(item_code_cpc, 1, 4),
                  item_code_cpc_5 = substr(item_code_cpc, 1, 5))
  #Now add in GTAP GSC3 sectors
  #Assign GTAP codes
  crop_valprod_GSC3 <- crop_valprod %>%
    dplyr::mutate(GSC3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pdr'])), 'pdr',
                                ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wht'])), 'wht',
                                       ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['gro'])), 'gro',
                                              ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['v_f'])), 'v_f',
                                                     ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['osd'])), 'osd',
                                                            ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['c_b'])), 'c_b',
                                                                   ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pfb'])), 'pfb',
                                                                          ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr',
                                                                                 ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr', NA))))))))))
  # #Check for missing values
  missing_GSC3 <- crop_valprod_GSC3 %>% dplyr::filter(is.na(GSC3)) %>% dplyr::select(crop) %>% unique()
  #Only rapeseed is missing value
  crop_valprod_GSC3 <- crop_valprod_GSC3 %>%
    dplyr::mutate(GSC3 = ifelse(crop == "rapeseed", 'osd', GSC3)) %>%
    dplyr::filter(!is.na(GSC3))
  #Summarise by GSC3 within REG-BIO combinations
  crop_valprod_GSC3 <- crop_valprod_GSC3 %>%
    dplyr::group_by(REG, BIO, GSC3) %>%
    dplyr::summarise(usd1000 = sum(usd1000, na.rm = T)) %>%
    dplyr::ungroup()
  #Save
  saveRDS(crop_valprod_GSC3, file = file.path(tmp_dir, 'tmp/output_data/reg_bio_crop_valprod.rds'))

  #### Harvested Area ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "reg_country_conc", "cpc_gsc3_concordance", "gsc3_cpc_codes")])
  gc()
  #Load in harvested area data at country-BIOME level
  harv_area <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_crop_harvarea.rds')) %>%
    dplyr::mutate(GADM = tolower(GADM))
  #Merge with the REG-GADM concordance
  harv_area <- dplyr::left_join(harv_area, reg_country_conc, by = c('GADM'))
  # #Test for missing GTAP REG
  # test <- harv_area %>%
  #   dplyr::select(GADM, REG) %>%
  #   unique() %>%
  #   dplyr::filter(is.na(REG))
  #Add up by REG
  harv_area <- harv_area %>%
    dplyr::group_by(REG, BIO, crop, item_code_cpc) %>%
    dplyr::summarise(ha = sum(ha, na.rm = T)) %>%
    dplyr::ungroup()
  # Aggregate within 9 GTAP CROP categories #
  #Create new cpc codes for specific number of digits
  harv_area <- harv_area %>%
    dplyr::mutate(item_code_cpc_2 = substr(item_code_cpc, 1, 2),
                  item_code_cpc_3 = substr(item_code_cpc, 1, 3),
                  item_code_cpc_4 = substr(item_code_cpc, 1, 4),
                  item_code_cpc_5 = substr(item_code_cpc, 1, 5))
  #Now add in GTAP GSC3 sectors
  #Assign GTAP codes
  harv_area_GSC3 <- harv_area %>%
    dplyr::mutate(GSC3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pdr'])), 'pdr',
                                ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wht'])), 'wht',
                                       ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['gro'])), 'gro',
                                              ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['v_f'])), 'v_f',
                                                     ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['osd'])), 'osd',
                                                            ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['c_b'])), 'c_b',
                                                                   ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pfb'])), 'pfb',
                                                                          ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr',
                                                                                 ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr', NA))))))))))
  # #Check for missing values
  # missing_GSC3 <- harv_area_GSC3 %>% dplyr::filter(is.na(GSC3)) %>% dplyr::select(crop) %>% unique()
  # #No missing values
  harv_area_GSC3 <- harv_area_GSC3 %>% dplyr::filter(!is.na(GSC3))
  #Summarise by GSC3 within REG-BIO combinations
  harv_area_GSC3 <- harv_area_GSC3 %>%
    dplyr::group_by(REG, BIO, GSC3) %>%
    dplyr::summarise(ha = sum(ha, na.rm = T)) %>%
    dplyr::ungroup()
  #Save
  saveRDS(harv_area_GSC3, file = file.path(tmp_dir, 'tmp/output_data/reg_bio_harv_area.rds'))

  #### Livestock number of animals ####
  rm(list=ls()[! ls() %in% c("tmp_dir", "reg_country_conc", "cpc_gsc3_concordance", "gsc3_cpc_codes")])
  gc()
  #Load in livestock production at country-BIOME level
  livestock_numanimals <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_numanimals.rds')) %>%
    dplyr::mutate(GADM = tolower(GADM))
  #Merge with the REG-GADM concordance
  livestock_numanimals <- dplyr::left_join(livestock_numanimals, reg_country_conc, by = c('GADM'))
  # #Test for missing GTAP REG
  # test <- livestock_numanimals %>%
  #   dplyr::select(GADM, REG) %>%
  #   unique() %>%
  #   dplyr::filter(is.na(REG))
  #Add up by REG
  livestock_numanimals <- livestock_numanimals %>%
    dplyr::group_by(REG, BIO, GSC3) %>%
    dplyr::summarise(head1000 = sum(head1000, na.rm = T)) %>%
    dplyr::ungroup()
  #Save
  saveRDS(livestock_numanimals, file = file.path(tmp_dir, 'tmp/output_data/reg_bio_livestock_numanimals.rds'))

  #### Value of Livestock production ####
  #Created using the prices for the 4 species calculated using gtaplulc18.har
  #file.
  rm(list=ls()[! ls() %in% c("tmp_dir", "reg_country_conc", "cpc_gsc3_concordance", "gsc3_cpc_codes")])
  gc()
  #Load in livestock production at country-BIOME level
  val_livestock_production <- readRDS(file.path(tmp_dir, 'tmp/gtap_ref_year/gadm_bio_livestock_valprod_ctlrmkwol.rds')) %>%
    dplyr::mutate(GADM = tolower(GADM))
  #Merge with the REG-GADM concordance
  val_livestock_production <- dplyr::left_join(val_livestock_production, reg_country_conc, by = c('GADM'))
  # #Test for missing GTAP REG
  # test <- val_livestock_production %>%
  #   dplyr::select(GADM, REG) %>%
  #   unique() %>%
  #   dplyr::filter(is.na(REG))
  #Add up by REG
  val_livestock_production <- val_livestock_production %>%
    dplyr::group_by(REG, BIO, GSC3) %>%
    dplyr::summarise(usd1000 = sum(usd1000, na.rm = T)) %>%
    dplyr::ungroup()
  #Save
  saveRDS(val_livestock_production, file = file.path(tmp_dir, 'tmp/output_data/reg_bio_val_livestock_production.rds'))
}
