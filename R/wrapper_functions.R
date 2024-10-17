#####
#' Wrapper function to run first three steps (Define sets, land cover, land use)
#'
#' @param parent_dir Path to the parent directory where raw data, intermediate
#'      outputs, and final datasets stored
#' @param reg_df Data frame containing the set of regions
#' @param boundary_sf Simple feature object containing the subnational boundaries
#' @param hectare_rasters_needed Choice indicating whether the 7 land cover
#'      rasters need to be created or whether they already exist ("YES" or "NO")
#' @param year_cropland_pasture_data Choice of whether to use 2000 or 2015 data
#'      for cropland and pastureland area ("2000" or "2015")
#'
#' @return Sets for the final set of endowments (BIO_ENDW), 9 Crop commodity,
#'         aggregates (CRP9) and 7 land cover types (LCOV), land cover data in
#'         hectares for each combination of GADM country and subnational division,
#'         files containing land use data for each combination of GADM
#'         country and subnational division. Also produces raster indicating
#'         concatenation of GADM countries and user-specified subnational divisions.
#'
#' @export
#'
#' @examples
#' definesets_lcover_luse("myfiles/gtapfiles/", GTAP_REG_dataframe, AEZ_shapefile, "YES", "2015")
definesets_lcover_luse <- function(parent_dir, reg_df, boundary_sf, hectare_rasters_needed, year_cropland_pasture_data) {
  #Format function inputs
    #Define parent directory
    parent_dir <- paste0(parent_dir)
    setwd(parent_dir)
    #Load in the sf for use in "define_sets" function
    subnat_bound_sf <- boundary_sf
    #Load in the dataframe containing the REG list
    reg_set_df <- reg_df
  #Run first three functions in the process
    define_sets(parent_dir, reg_set_df, subnat_bound_sf)
    rm(list=ls()[! ls() %in% c("parent_dir", "hectare_rasters_needed")])
    gc()
    land_cover(parent_dir, hectare_rasters_needed, year_cropland_pasture_data)
    rm(list=ls()[! ls() %in% c("parent_dir")])
    gc()
    land_use(parent_dir)
    rm(list=ls()[! ls() %in% c("parent_dir")])
    gc()
}

#####
#' Wrapper function to run steps 4-6 (download FAO, create shares, share out data)
#'
#' @param parent_dir Path to the parent directory where raw data, intermediate
#'      outputs, and final datasets stored
#' @param gtap_year_choice Year of FAO data to be downloaded
#'
#' @return FAO crop and livestock production data and prices for a specified year,
#'     and GADM country-BIOME share variables so the FAO production can be
#'     disaggregated. The GADM country-BIOME share variables are then used to
#'     disaggregate the national-level FAO data for the chosen GTAP reference year
#'     by the user-specified subnational divisions.
#' @export
#'
#' @examples
#' faodata_shares_sharedata("myfiles/gtapfiles/", "2017")
faodata_shares_sharedata <- function(parent_dir, gtap_year_choice) {
  #Format parent directory function input
  parent_dir <- paste0(parent_dir)
  setwd(parent_dir)
  #specify GTAP reference year used to choose which FAO data to download
  gtap_year <- gtap_year_choice
  #Run functions for steps 4-6
  download_fao(parent_dir, gtap_year)
    rm(list=ls()[! ls() %in% c("parent_dir", "gtap_year_choice")])
    gc()
  create_shares(parent_dir)
    rm(list=ls()[! ls() %in% c("parent_dir", "gtap_year_choice")])
    gc()
  share_data(parent_dir)
}

#####
#' Wrapper function to aggregate data by GTAP region and crop commodity
#' categories, create LULC .har files, create the input files, and then run
#' gtapland-ragg routines.
#'
#' @param parent_dir Path to the parent directory where raw data, intermediate
#'      outputs, and final datasets are stored
#' @param output_folder Desired name of folder containing 4 GTAP-LULC files:
#'      BIOMEmod.txt (aggregation file), gtaplulcaez.har (land use and
#'      land cover dataset), gtaplulcset.har (file containing the sets
#'      used by GTAP-AEZ model). This folder is located in the "output_data"
#'      folder within the specified parent directory.
#' @param direc_name Desired name of the final, aggregated database containing:
#'      basedata.har, baserate.har, baseview.har, default.prm, inputs.zip, and
#'      sets.har. This is output as a folder in the parent directory.
#'
#' @return Creates two folders. One contains the final, aggregated database and
#'      a zipped folder with its input files. The second contains the GTAP-LULC
#'      database for all GTAP regions (REG) defined by the definesets_lcover_luse
#'      function.
#' @export
#'
#' @examples
#' aggr_gtaplulc_3outputs("myfiles/gtapfiles/", "GTAP_LULC_AEZ18_2017", "GTAP_AEZ18_2017")
aggr_gtaplulc_3outputs <- function(parent_dir, output_folder, direc_name) {
  #Format parent directory function input
  parent_dir <- paste0(parent_dir)
  setwd(parent_dir)
  #Specify name of output folder
  out_folder_name <- output_folder
  #Run 5 functions from steps 7-11
  aggr(parent_dir)
  gpack_txt(parent_dir)
  gtaplulc(parent_dir)
  input_file(parent_dir)
  save_3final_outputs(parent_dir, out_folder_name)
  rm(list=ls()[! ls() %in% c("parent_dir", "out_folder_name", "direc_name")])
  gc()
  #Run Nelson's function from gtapland-ragg.r
  subDir <- paste0("output_data/", out_folder_name)
  createdat(
    mapfile = paste0(parent_dir, "GTAP_BIOMES/output_data/", out_folder_name,  "/BIOMEmod.txt"),
    setfile = paste0(parent_dir, "GTAP_BIOMES/output_data/", out_folder_name,  "/gtaplulcset.har"),
    datfile = paste0(parent_dir, "GTAP_BIOMES/output_data/", out_folder_name,  "/gtaplulcaez.har"),
    stdprm = paste0(parent_dir, "GTAP_BIOMES/raw_data/gsdgpar11cMV6.har"),
    dir=direc_name
  )
}
