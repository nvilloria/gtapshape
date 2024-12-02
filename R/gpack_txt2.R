#' Output aggregated data as GEMPACK txt files
#'
#' This function outputs the aggregated data from the "aggr" command
#' as GEMPACK text files so they can be read into GTAP analyses. [NV:
#' Add the files that this function reads, and the functions that
#' created them.]
#'
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @param cpc_gsc3_concordance_file File name of file containing concordance
#'    which links CPC codes to their GSC3 GTAP sector codes
#'
#' @return Saves the six .txt files below in ./workdir/output_data. These are GEMPACK txt file format containing data to the GTAP regions and sub-national geographies created by [NV: link to function].
#'
#' The GEMPACK text files in directory ./workdir/output_data are:
#'
#' lulc_cropprod_data.txt
#'
#' lulc_harvarea_data.txt
#'
#' lulc_lcov_data.txt
#'
#' lulc_lvstkprod_data.txt
#'
#' lulc_valcropprod_data.txt
#'
#' lulc_vallvstkprod_data.txt
#'
#' [NV: These files are used by [link to function]].
#' @export
gpack_txt <- function(workdir_dir=getwd(), cpc_gsc3_concordance_file) {
  #Make a "not in" function
  '%!in%' <- function(x,y)!('%in%'(x,y))

  #Load in the REG set created by 'define_sets'
  REG_set <- readRDS(file.path(workdir_dir, 'workdir/sets/set_REG.rds'))
  REG_set <- data.frame(REG = REG_set)
  #Make column showing the number so we can keep things in order
  REG_set <- REG_set %>%
    dplyr::mutate(REG_num = row_number())

  #Load in the BIO set created by 'define_sets'
  BIO_set <- readRDS(file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
  BIO_set <- data.frame(BIO = BIO_set)
  #Make column showing the number so we can keep things in order
  BIO_set <- BIO_set %>%
    dplyr::mutate(BIO_num = row_number())

  #Load in the concordance linking cpc codes to their GTAP commodity categories
  #also known as GSC sectors
  cpc_gsc3_concordance <- read.csv(cpc_gsc3_concordance_file,
                                   header = T,
                                   colClasses = c(cpc_code = "character")) %>%
    dplyr::rename(GSC3 = GSC_code,
                  GSC3_num = GSC_num) %>%
    dplyr::select(GSC3, GSC3_num) %>%
    dplyr::arrange(GSC3_num) %>%
    unique()
  #Make one for just crop codes
  cpc_gsc3_concordance_crop <- cpc_gsc3_concordance %>%
    dplyr::filter(GSC3_num <= 8) %>%
    dplyr::arrange(GSC3_num) %>%
    unique()
  #Make another for just livestock codes
  cpc_gsc3_concordance_lvstk <- cpc_gsc3_concordance %>%
    dplyr::filter(GSC3 %in% c('ctl', 'rmk', 'wol')) %>%
    dplyr::arrange(GSC3_num) %>%
    unique()

  #### Land Cover ####
                                        #Read in land cover data
    require(dplyr)
    workdir_dir <- getwd()
  landcover <- readRDS(file.path(workdir_dir, 'workdir/output_data/reg_bio_land_cover.rds'))
  #Add in the REG and BIO numbers
  landcover <- dplyr::left_join(landcover, REG_set, by = c('REG'))
  landcover <- dplyr::left_join(landcover, BIO_set, by = c('BIO'))
  #Expand grid to get all combinations of REG and BIOME
  output_grid <- expand.grid(REG_num = REG_set$REG_num, BIO_num = BIO_set$BIO_num)
  #Merge with the grid of all possible REG and BIOME combinations
  landcover_output <- dplyr::left_join(output_grid, landcover, by = c('REG_num', 'BIO_num')) %>%
    dplyr::select(REG, REG_num, BIO, BIO_num, forest_ha, grassland_ha, shrubland_ha, cropland_ha, pasture_ha, urban_ha, other_ha)
  #Replace NA values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  landcover_output <- landcover_output %>%
    dplyr::mutate(across(forest_ha:other_ha, function_na_to_zero))
  #Sort the data in increasing order for REG, then AEZ
  landcover_output <- landcover_output %>%
    dplyr::arrange(REG_num, BIO_num)
  #Divide the land cover values by 1000 as GTAP database values are in 1000 ha
  #Also change names to match gtaplulc18.har naming
  landcover_output <- landcover_output %>%
    dplyr::mutate(across(forest_ha:other_ha, ~ .x/1000)) %>%
    dplyr::rename(Forest = forest_ha,
                  SavnGrasslnd = grassland_ha,
                  Shrubland = shrubland_ha,
                  Cropland = cropland_ha,
                  Pastureland = pasture_ha,
                  Builtupland = urban_ha,
                  Otherland = other_ha)

  #Create a .txt file in GEMPACK Textfile format
  #How much data line - REG, then BIO, then LCOV
  hmd <- c(paste0(nrow(REG_set), ' ', nrow(BIO_set), ' 7 real row_order Header "LAND" ;'))
  #List the LCOVER categories
  LCOV_GTAP_list <- colnames(landcover_output)[5:11]
  #Write a function that selects the commodity category, reshapes to wide, and exports matrix
  to_wide <- function(lcov) {
    vars <- c("REG_num", "BIO_num", paste0(lcov))
    #Remove unneccesary columns
    working_df <- landcover_output %>%
      dplyr::select(all_of(vars))
    #Pivot to wide after making sure everything is in correct order
    working_df <- working_df %>%
      dplyr::arrange(REG_num, BIO_num)
    working_wide_df <- tidyr::pivot_wider(working_df,
                                          names_from = BIO_num,
                                          values_from = all_of(lcov),
                                          names_prefix = 'BIO')
    working_wide_mat <- working_wide_df[ , 2:ncol(working_wide_df)]
    #Output a matrix
    working_wide_mat
  }
  #Now lapply the function to the list to get the matrix for each land cover
  #For example, the first (.,.,1) matrix is for the first LCOV category ('Forest')
  matrix_list <- lapply(LCOV_GTAP_list, to_wide)
  #Now append all the matrices
  filename <- file.path(workdir_dir, 'workdir/output_data/lulc_lcov_data.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(LCOV_GTAP_list)) {
    comment_text <- c(paste0('! Now the matrix for LCOV category ', LCOV_GTAP_list[[i]], ' !'))
    write.table(comment_text, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
    write.table(matrix_list[[i]], file=filename, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### CROP PRODUCTION ####
  rm(list=ls()[! ls() %in% c("workdir_dir", "REG_set", "BIO_set", "cpc_gsc3_concordance", "cpc_gsc3_concordance_crop", "cpc_gsc3_concordance_lvstk")])
  gc()
  #Load in crop production data
  crop_prod <- readRDS(file.path(workdir_dir, 'workdir/output_data/reg_bio_crop_prod.rds'))
  #Add in the GSC3 codes so we can keep them in order
  crop_prod <- dplyr::left_join(crop_prod, cpc_gsc3_concordance_crop, by = c('GSC3'))
  crop_prod %>% dplyr::select(GSC3) %>% unique
  #Add in the REG and BIO numbers
  crop_prod <- dplyr::left_join(crop_prod, REG_set, by = c('REG'))
  crop_prod <- dplyr::left_join(crop_prod, BIO_set, by = c('BIO'))
  #Make a grid with all possible REG-BIOME-GSC3 combinations
  output_grid <- expand.grid(REG_num = REG_set$REG_num, BIO_num = BIO_set$BIO_num, GSC3_num = cpc_gsc3_concordance_crop$GSC3_num %>% unique())
  #Merge crop data to grid
  crop_prod_output <- dplyr::left_join(output_grid, crop_prod, by = c('REG_num', 'BIO_num', 'GSC3_num')) %>%
    dplyr::select(REG, REG_num, BIO, BIO_num, GSC3, GSC3_num, everything())
  #Replace NA values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  crop_prod_output <- crop_prod_output %>%
    dplyr::mutate(across(tons, function_na_to_zero))

  #Sort the data in increasing order for REG, then AEZ
  crop_prod_output <- crop_prod_output %>%
    dplyr::arrange(REG_num, BIO_num, GSC3_num)
  #Divide the crop production values by 1000 as GTAP database values are in 1000 MT
  crop_prod_output <- crop_prod_output %>%
    dplyr::mutate(across(tons, ~ .x/1000))
  #Create a .txt file in GEMPACK Textfile format
  #How much data line - REG, then BIO, then GSC3
  hmd <- c(paste0(nrow(REG_set), ' ', nrow(BIO_set), ' 8 real row_order Header "QCR8" ;'))
  #List the GSC3 crop categories
  CROP_GTAP_name_list <- cpc_gsc3_concordance_crop$GSC3 %>% unique
  CROP_GTAP_num_list <- cpc_gsc3_concordance_crop$GSC3_num %>% unique
  #Write a function that selects the commodity category, reshapes to wide, and exports matrix
  to_wide <- function(crop_num) {
    working_df <- crop_prod_output %>%
      dplyr::filter(GSC3_num == crop_num)
    #Remove unneccesary columns
    vars <- c("REG_num", "BIO_num", "tons")
    working_df <- working_df %>%
      dplyr::select(all_of(vars))
    #Pivot to wide after making sure everything is in correct order
    working_df <- working_df %>%
      dplyr::arrange(REG_num, BIO_num)
    working_wide_df <- tidyr::pivot_wider(working_df,
                                          names_from = BIO_num,
                                          values_from = tons,
                                          names_prefix = 'BIO')
    working_wide_mat <- working_wide_df[ , 2:ncol(working_wide_df)]
    #Output a matrix
    working_wide_mat
  }
  #Now lapply the function to the list to get the matrix for each GSC3 crop sector
  #For example, the first (.,.,1) matrix is for the first sector ('pdr')
  matrix_list <- lapply(CROP_GTAP_num_list, to_wide)
  #Now append all the matrices
  filename <- file.path(workdir_dir, 'workdir/output_data/lulc_cropprod_data.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(CROP_GTAP_name_list)) {
    comment_text <- c(paste0('! Now the matrix for GSC3 sector ', CROP_GTAP_name_list[[i]], ' !'))
    write.table(comment_text, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
    write.table(matrix_list[[i]], file=filename, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### Value of CROP PRODUCTION ####
  rm(list=ls()[! ls() %in% c("workdir_dir", "REG_set", "BIO_set", "cpc_gsc3_concordance", "cpc_gsc3_concordance_crop", "cpc_gsc3_concordance_lvstk")])
  gc()
  #Load in crop production data
  val_crop_prod <- readRDS(file.path(workdir_dir, 'workdir/output_data/reg_bio_crop_valprod.rds'))
  #Add in the GSC3 codes so we can keep them in order
  val_crop_prod <- dplyr::left_join(val_crop_prod, cpc_gsc3_concordance_crop, by = c('GSC3'))
  val_crop_prod %>% dplyr::select(GSC3) %>% unique
  #Add in the REG and BIO numbers
  val_crop_prod <- dplyr::left_join(val_crop_prod, REG_set, by = c('REG'))
  val_crop_prod <- dplyr::left_join(val_crop_prod, BIO_set, by = c('BIO'))
  #Make a grid with all possible REG-BIOME-GSC3 combinations
  output_grid <- expand.grid(REG_num = REG_set$REG_num, BIO_num = BIO_set$BIO_num, GSC3_num = cpc_gsc3_concordance_crop$GSC3_num %>% unique())
  #Merge crop data to grid
  val_crop_prod_output <- dplyr::left_join(output_grid, val_crop_prod, by = c('REG_num', 'BIO_num', 'GSC3_num')) %>%
    dplyr::select(REG, REG_num, BIO, BIO_num, GSC3, GSC3_num, everything())
  #Replace NA values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  val_crop_prod_output <- val_crop_prod_output %>%
    dplyr::mutate(across(usd1000, function_na_to_zero))

  #Sort the data in increasing order for REG, then AEZ
  val_crop_prod_output <- val_crop_prod_output %>%
    dplyr::arrange(REG_num, BIO_num, GSC3_num)
  #Values are already in 1000 USD
  #Create a .txt file in GEMPACK Textfile format
  #How much data line - REG, then BIO, then GSC3
  hmd <- c(paste0(nrow(REG_set), ' ', nrow(BIO_set), ' 8 real row_order Header "VCR8" ;'))
  #List the GSC3 crop categories
  CROP_GTAP_name_list <- cpc_gsc3_concordance_crop$GSC3 %>% unique
  CROP_GTAP_num_list <- cpc_gsc3_concordance_crop$GSC3_num %>% unique
  #Write a function that selects the commodity category, reshapes to wide, and exports matrix
  to_wide <- function(crop_num) {
    working_df <- val_crop_prod_output %>%
      dplyr::filter(GSC3_num == crop_num)
    #Remove unneccesary columns
    vars <- c("REG_num", "BIO_num", "usd1000")
    working_df <- working_df %>%
      dplyr::select(all_of(vars))
    #Pivot to wide after making sure everything is in correct order
    working_df <- working_df %>%
      dplyr::arrange(REG_num, BIO_num)
    working_wide_df <- tidyr::pivot_wider(working_df,
                                          names_from = BIO_num,
                                          values_from = usd1000,
                                          names_prefix = 'BIO')
    working_wide_mat <- working_wide_df[ , 2:ncol(working_wide_df)]
    #Output a matrix
    working_wide_mat
  }
  #Now lapply the function to the list to get the matrix for each GSC3 crop sector
  #For example, the first (.,.,1) matrix is for the first sector ('pdr')
  matrix_list <- lapply(CROP_GTAP_num_list, to_wide)
  #Now append all the matrices
  filename <-  file.path(workdir_dir, 'workdir/output_data/lulc_valcropprod_data.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(CROP_GTAP_name_list)) {
    comment_text <- c(paste0('! Now the matrix for GSC3 sector ', CROP_GTAP_name_list[[i]], ' !'))
    write.table(comment_text, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
    write.table(matrix_list[[i]], file=filename, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### Harvested Area ####
  rm(list=ls()[! ls() %in% c("workdir_dir", "REG_set", "BIO_set", "cpc_gsc3_concordance", "cpc_gsc3_concordance_crop", "cpc_gsc3_concordance_lvstk")])
  gc()
  #Load in crop harvested area data
  harv_area <- readRDS(file.path(workdir_dir, 'workdir/output_data/reg_bio_harv_area.rds'))
  #Add in the GSC3 codes so we can keep them in order
  harv_area <- dplyr::left_join(harv_area, cpc_gsc3_concordance_crop, by = c('GSC3'))
  harv_area %>% dplyr::select(GSC3) %>% unique
  #Add in the REG and BIO numbers
  harv_area <- dplyr::left_join(harv_area, REG_set, by = c('REG'))
  harv_area <- dplyr::left_join(harv_area, BIO_set, by = c('BIO'))
  #Make a grid with all possible REG-BIOME-GSC3 combinations
  output_grid <- expand.grid(REG_num = REG_set$REG_num, BIO_num = BIO_set$BIO_num, GSC3_num = cpc_gsc3_concordance_crop$GSC3_num %>% unique())
  #Merge crop data to grid
  harv_area_output <- dplyr::left_join(output_grid, harv_area, by = c('REG_num', 'BIO_num', 'GSC3_num')) %>%
    dplyr::select(REG, REG_num, BIO, BIO_num, GSC3, GSC3_num, everything())
  #Replace NA values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  harv_area_output <- harv_area_output %>%
    dplyr::mutate(across(ha, function_na_to_zero))

  #Sort the data in increasing order for REG, then AEZ
  harv_area_output <- harv_area_output %>%
    dplyr::arrange(REG_num, BIO_num, GSC3_num)
  #Divide the crop harvested area values by 1000 as GTAP database values are in 1000 hectares
  harv_area_output <- harv_area_output %>%
    dplyr::mutate(across(ha, ~ .x/1000))
  #Create a .txt file in GEMPACK Textfile format
  #How much data line - REG, then BIO, then GSC3
  hmd <- c(paste0(nrow(REG_set), ' ', nrow(BIO_set), ' 8 real row_order Header "HARV" ;'))
  #List the GSC3 crop categories
  CROP_GTAP_name_list <- cpc_gsc3_concordance_crop$GSC3 %>% unique
  CROP_GTAP_num_list <- cpc_gsc3_concordance_crop$GSC3_num %>% unique

  #Write a function that selects the commodity category, reshapes to wide, and exports matrix
  to_wide <- function(crop_num) {
    working_df <- harv_area_output %>%
      dplyr::filter(GSC3_num == crop_num)
    #Remove unneccesary columns
    vars <- c("REG_num", "BIO_num", "ha")
    working_df <- working_df %>%
      dplyr::select(all_of(vars))
    #Pivot to wide after making sure everything is in correct order
    working_df <- working_df %>%
      dplyr::arrange(REG_num, BIO_num)
    working_wide_df <- tidyr::pivot_wider(working_df,
                                          names_from = BIO_num,
                                          values_from = ha,
                                          names_prefix = 'BIO')
    working_wide_mat <- working_wide_df[ , 2:ncol(working_wide_df)]
    #Output a matrix
    working_wide_mat
  }
  #Now lapply the function to the list to get the matrix for each GSC3 crop sector
  #For example, the first (.,.,1) matrix is for the first sector ('pdr')
  matrix_list <- lapply(CROP_GTAP_num_list, to_wide)
  #Now append all the matrices
  filename <- file.path(workdir_dir, 'workdir/output_data/lulc_harvarea_data.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(CROP_GTAP_name_list)) {
    comment_text <- c(paste0('! Now the matrix for GSC3 sector ', CROP_GTAP_name_list[[i]], ' !'))
    write.table(comment_text, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
    write.table(matrix_list[[i]], file=filename, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### Livestock Production ####
  rm(list=ls()[! ls() %in% c("workdir_dir", "REG_set", "BIO_set", "cpc_gsc3_concordance", "cpc_gsc3_concordance_crop", "cpc_gsc3_concordance_lvstk")])
  gc()
  #Load in livestock production data
  lvstk_prod <- readRDS(file.path(workdir_dir, 'workdir/output_data/reg_bio_livestock_numanimals.rds'))
  #Add in the GSC3 codes so we can keep them in order
  lvstk_prod <- dplyr::left_join(lvstk_prod, cpc_gsc3_concordance_lvstk, by = c('GSC3'))
  lvstk_prod %>% dplyr::select(GSC3) %>% unique
  #Add in the REG and BIO numbers
  lvstk_prod <- dplyr::left_join(lvstk_prod, REG_set, by = c('REG'))
  lvstk_prod <- dplyr::left_join(lvstk_prod, BIO_set, by = c('BIO'))
  #Make a grid with all possible REG-BIOME-GSC3 combinations
  output_grid <- expand.grid(REG_num = REG_set$REG_num, BIO_num = BIO_set$BIO_num, GSC3_num = cpc_gsc3_concordance_lvstk$GSC3_num %>% unique())
  #Merge livestock data to grid
  lvstk_prod_output <- dplyr::left_join(output_grid, lvstk_prod, by = c('REG_num', 'BIO_num', 'GSC3_num')) %>%
    dplyr::select(REG, REG_num, BIO, BIO_num, GSC3, GSC3_num, everything())
  #Replace NA values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  lvstk_prod_output <- lvstk_prod_output %>%
    dplyr::mutate(across(head1000, function_na_to_zero))

  #Sort the data in increasing order for REG, then AEZ
  lvstk_prod_output <- lvstk_prod_output %>%
    dplyr::arrange(REG_num, BIO_num, GSC3_num)
  #Create a .txt file in GEMPACK Textfile format
  #How much data line - REG, then BIO, then GSC3
  hmd <- c(paste0(nrow(REG_set), ' ', nrow(BIO_set), ' 3 real row_order Header "QLV3" ;'))
  #List the GSC3 livestock categories
  LVSTK_GTAP_name_list <- cpc_gsc3_concordance_lvstk$GSC3 %>% unique
  LVSTK_GTAP_num_list <- cpc_gsc3_concordance_lvstk$GSC3_num %>% unique
  #Write a function that selects the commodity category, reshapes to wide, and exports matrix
  to_wide <- function(lvstk_num) {
    working_df <- lvstk_prod_output %>%
      dplyr::filter(GSC3_num == lvstk_num)
    #Remove unneccesary columns
    vars <- c("REG_num", "BIO_num", "head1000")
    working_df <- working_df %>%
      dplyr::select(all_of(vars))
    #Pivot to wide after making sure everything is in correct order
    working_df <- working_df %>%
      dplyr::arrange(REG_num, BIO_num)
    working_wide_df <- tidyr::pivot_wider(working_df,
                                          names_from = BIO_num,
                                          values_from = head1000,
                                          names_prefix = 'BIO')
    working_wide_mat <- working_wide_df[ , 2:ncol(working_wide_df)]
    #Output a matrix
    working_wide_mat
  }
  #Now lapply the function to the list to get the matrix for each GSC3 crop sector
  #For example, the first (.,.,1) matrix is for the first sector ('pdr')
  matrix_list <- lapply(LVSTK_GTAP_num_list, to_wide)
  #Now append all the matrices
  filename <- file.path(workdir_dir, 'workdir/output_data/lulc_lvstkprod_data.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(LVSTK_GTAP_name_list)) {
    comment_text <- c(paste0('! Now the matrix for GSC3 sector ', LVSTK_GTAP_name_list[[i]], ' !'))
    write.table(comment_text, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
    write.table(matrix_list[[i]], file=filename, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### Livestock Value of Production ####
  rm(list=ls()[! ls() %in% c("workdir_dir", "REG_set", "BIO_set", "cpc_gsc3_concordance", "cpc_gsc3_concordance_crop", "cpc_gsc3_concordance_lvstk")])
  gc()
  #Load in Value of Livestock production data
  val_lvstk_prod <- readRDS(file.path(workdir_dir, 'workdir/output_data/reg_bio_val_livestock_production.rds'))
  #Add in the GSC3 codes so we can keep them in order
  val_lvstk_prod <- dplyr::left_join(val_lvstk_prod, cpc_gsc3_concordance_lvstk, by = c('GSC3'))
  val_lvstk_prod %>% dplyr::select(GSC3) %>% unique
  #Add in the REG and BIO numbers
  val_lvstk_prod <- dplyr::left_join(val_lvstk_prod, REG_set, by = c('REG'))
  val_lvstk_prod <- dplyr::left_join(val_lvstk_prod, BIO_set, by = c('BIO'))
  #Make a grid with all possible REG-BIOME-GSC3 combinations
  output_grid <- expand.grid(REG_num = REG_set$REG_num, BIO_num = BIO_set$BIO_num, GSC3_num = cpc_gsc3_concordance_lvstk$GSC3_num %>% unique())
  #Merge crop data to grid
  val_lvstk_prod_output <- dplyr::left_join(output_grid, val_lvstk_prod, by = c('REG_num', 'BIO_num', 'GSC3_num')) %>%
    dplyr::select(REG, REG_num, BIO, BIO_num, GSC3, GSC3_num, everything())
  #Replace NA values with zeroes
  function_na_to_zero <- function(x) (ifelse(is.na(x), 0, x))
  val_lvstk_prod_output <- val_lvstk_prod_output %>%
    dplyr::mutate(across(usd1000, function_na_to_zero))

  #Sort the data in increasing order for REG, then AEZ
  val_lvstk_prod_output <- val_lvstk_prod_output %>%
    dplyr::arrange(REG_num, BIO_num, GSC3_num)
  #Create a .txt file in GEMPACK Textfile format
  #How much data line - REG, then BIO, then GSC3
  hmd <- c(paste0(nrow(REG_set), ' ', nrow(BIO_set), ' 3 real row_order Header "VLV3" ;'))
  #List the GSC3 livestock categories
  LVSTK_GTAP_name_list <- cpc_gsc3_concordance_lvstk$GSC3 %>% unique
  LVSTK_GTAP_num_list <- cpc_gsc3_concordance_lvstk$GSC3_num %>% unique
  #Write a function that selects the commodity category, reshapes to wide, and exports matrix
  to_wide <- function(lvstk_num) {
    working_df <- val_lvstk_prod_output %>%
      dplyr::filter(GSC3_num == lvstk_num)
    #Remove unneccesary columns
    vars <- c("REG_num", "BIO_num", "usd1000")
    working_df <- working_df %>%
      dplyr::select(all_of(vars))
    #Pivot to wide after making sure everything is in correct order
    working_df <- working_df %>%
      dplyr::arrange(REG_num, BIO_num)
    working_wide_df <- tidyr::pivot_wider(working_df,
                                          names_from = BIO_num,
                                          values_from = usd1000,
                                          names_prefix = 'BIO')
    working_wide_mat <- working_wide_df[ , 2:ncol(working_wide_df)]
    #Output a matrix
    working_wide_mat
  }
  #Now lapply the function to the list to get the matrix for each GSC3 crop sector
  #For example, the first (.,.,1) matrix is for the first sector ('pdr')
  matrix_list <- lapply(LVSTK_GTAP_num_list, to_wide)
  #Now append all the matrices
  filename <- file.path(workdir_dir, 'workdir/output_data/lulc_vallvstkprod_data.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(LVSTK_GTAP_name_list)) {
    comment_text <- c(paste0('! Now the matrix for GSC3 sector ', LVSTK_GTAP_name_list[[i]], ' !'))
    write.table(comment_text, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
    write.table(matrix_list[[i]], file=filename, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
}
