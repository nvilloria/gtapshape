#' Land use
#'
#' This function creates crop output, harvested area (ha) data, and livestock
#' numbers for country-BIOME combinations using the rasters from Monfreda et
#' al (2008) and the Gridded Livestock of the World livestock density data.
#' The countries will be aggregated to the regions (REG) defined in using the
#' define_sets function later. The concatenated country-BIOME raster is created
#' by the land_cover function.
#'
#' @param crop_production_rast_dir Directory containing rasters with
#'    data on production of 175 crops
#' @param crop_area_rast_dir Directory containing rasters with data on
#'    harvested area of 175 crops
#' @param livestock_density_rast_dir Directory containing rasters with data
#'    on density of livestock animals
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Saves files containing land use data for each combination of GADM
#'      country and subnational division
#'
#' @import terra
#' @import dplyr
#'
#' @export
land_use <- function(crop_production_rast_dir, crop_area_rast_dir, livestock_density_rast_dir, workdir_dir=getwd()) {
  #### Crop production ####
  #Load in the crop output and harvested area (ha) data
  crop_output_raster_list <- list.files(crop_production_rast_dir,
                                        pattern = '_Production.tif$',
                                        recursive = TRUE,
                                        full.names = TRUE)
  #Make a stack of the production(tons) rasters
  crop_output_raster_stack <- terra::rast(crop_output_raster_list)

  #Load in the GADM-BIO combination raster
  GADM_BIOME_rast <- terra::rast(file.path(workdir_dir, 'workdir/rasters/GADM_BIOME_rast.tif'))

  #Combine with the production rasters
  output_rast <- c(GADM_BIOME_rast, crop_output_raster_stack)
  #Turn into dataframe
  output_df <- as.data.frame(output_rast)
  #remove objects from environment
  rm(crop_output_raster_stack, output_rast)
  gc()
  #Summarize output at the GADM-BIOME level
  output_df <- output_df %>% dplyr::filter(!is.na(GADM_BIO)) %>%
    dplyr::group_by(GADM_BIO) %>%
    dplyr::summarise_all(sum, na.rm = T)
  #Create columns showing individual GADM and BIOME values
  output_df <- output_df %>% dplyr::mutate(GADM = substr(GADM_BIO, 1, 3),
                                           BIO = substr(GADM_BIO, 5, nchar(paste0(GADM_BIO)))) %>%
    dplyr::select(GADM, BIO, GADM_BIO, everything())
  #Save as rds
  saveRDS(output_df, file = file.path(workdir_dir, 'workdir/base_year/crop_production_tons.rds'))
  rm(list=ls()[! ls() %in% c("GADM_BIOME_rast", "crop_production_rast_dir", "crop_area_rast_dir", "livestock_density_rast_dir", "workdir_dir")])
  gc()

  #### Harvested Area ####
  #Load in the crop output and harvested area (ha) data from Monfreda et al (2008)
  #List only the harvested area rasters
  harvested_area_raster_list <- list.files(crop_area_rast_dir,
                                           pattern = '_HarvestedAreaHectares.tif$',
                                           recursive = TRUE,
                                           full.names = TRUE)
  #Make a stack of the production(tons) rasters
  harvested_area_raster_stack <- terra::rast(harvested_area_raster_list)

  #Combine with the ISO-aez raster
  output_rast <- c(GADM_BIOME_rast, harvested_area_raster_stack)
  #Turn into dataframe
  output_df <- as.data.frame(output_rast)
  #remove objects from environment
  rm(harvested_area_raster_stack, output_rast)
  gc()
  #Summarize harvested area at the GADM-BIOME level
  output_df <- output_df %>% dplyr::filter(!is.na(GADM_BIO)) %>%
    dplyr::group_by(GADM_BIO) %>%
    dplyr::summarise_all(sum, na.rm = T)
  #Create columns showing individual GADM and BIOME values
  output_df <- output_df %>% dplyr::mutate(GADM = substr(GADM_BIO, 1, 3),
                                           BIO = substr(GADM_BIO, 5, nchar(paste0(GADM_BIO)))) %>%
    dplyr::select(GADM, BIO, GADM_BIO, everything())
  #Save as rds
  saveRDS(output_df, file = file.path(workdir_dir, 'workdir/base_year/crop_harvarea_ha.rds'))
  rm(list=ls()[! ls() %in% c("GADM_BIOME_rast", "crop_production_rast_dir", "crop_area_rast_dir", "livestock_density_rast_dir", "workdir_dir")])
  gc()

  #### Livestock production ####
  #Load in the Gridded Livestock density data
  #First get a list of all the rasters
  dir_paths <- dir(path = livestock_density_rast_dir,
                   recursive = FALSE,
                   full.names = TRUE)
  livestock_rast_file_list <- lapply(dir_paths, FUN = list.files, pattern = ".tif$", full.names = T)
  #Turn them into a stack of rasters, replace negative values with zeroes
  lvstk_dens_rast_stack <- lapply(livestock_rast_file_list, FUN = terra::rast)
  lvstk_dens_rast_stack <- lapply(lvstk_dens_rast_stack, neg_to_zero)
  #Now multiply density by area of grid cells to get quantity of animals
    lvstk_head_rast_stack <- lapply(lvstk_dens_rast_stack, function(livestock_rast) {
      grid_cell_area <- terra::cellSize(livestock_rast, mask=FALSE, lyrs=FALSE, unit="km", transform=TRUE)
      total_livestock_rast <-  livestock_rast * grid_cell_area
      total_livestock_rast
    })
    rm(lvstk_dens_rast_stack)
    gc()
    #Now we need to aggregate the rasters because they're at a finer resolution
    lvstk_head_rast_stack <- lapply(lvstk_head_rast_stack, terra::aggregate, fact = 10, fun = "sum")
  #Replace NA values with zeroes
  lvstk_head_rast_stack <- lapply(lvstk_head_rast_stack, na_to_zero)
  #Make it into one single stack of rasters instead of a list of rasters
  lvstk_head_rast_stack <- terra::rast(lvstk_head_rast_stack)
  #Name the layers
  species_names <- lapply(dir_paths, function(x) (gsub(livestock_density_rast_dir, "", x)))
  species_names <- lapply(species_names, function(x) (substr(x, 2, regexpr("_", x) - 1)))
  species_names <- unlist(species_names)
  names(lvstk_head_rast_stack) <- species_names
  terra::writeRaster(lvstk_head_rast_stack, filename = file.path(workdir_dir, 'workdir/rasters/lvstk_total_head.tif'), overwrite = TRUE)
  rm(list=ls()[! ls() %in% c("GADM_BIOME_rast", "lvstk_head_rast_stack", "workdir_dir")])
  gc()

  #### Output livestock quantity by GADM and BIOME combinations
  #Load in GADM-BIOME concatenation raster
  GADM_BIOME_rast <- terra::rast(file.path(workdir_dir, 'workdir/rasters/GADM_BIOME_rast.tif'))
  #Combine with the production rasters
  output_rast <- c(GADM_BIOME_rast, lvstk_head_rast_stack)
  #Turn into dataframe
  output_df <- as.data.frame(output_rast)
  #Summarize output at the GADM-BIOME level
  output_df <- output_df %>% dplyr::filter(!is.na(GADM_BIO)) %>%
    dplyr::group_by(GADM_BIO) %>%
    dplyr::summarise_all(sum, na.rm = T)
  #remove objects from environment
  rm(lvstk_head_rast_stack, output_rast,GADM_BIOME_rast)
  gc()
  #Create columns showing individual GADM and BIOME values
  output_df <- output_df %>% dplyr::mutate(GADM = substr(GADM_BIO, 1, 3),
                                           BIO = substr(GADM_BIO, 5, nchar(paste0(GADM_BIO)))) %>%
    dplyr::select(GADM, BIO, GADM_BIO, everything())
  #Save as rds
  saveRDS(output_df, file = file.path(workdir_dir, 'workdir/base_year/livestock_production_head.rds'))
  rm(list=ls())
  gc()
}
