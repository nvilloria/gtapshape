#' Land cover
#'
#' This function creates rasters for potential vegetation and urban land cover,
#' and then combines them with cropland and pastureland rasters. Next, it
#' creates 7 rasters showing the hectares in each land cover. Next, output
#' the number of hectares in each land cover by country-subnational boundary
#' combination.
#'
#' @param gadm_rast_file File name for raster containing GADM countries
#' @param subnat_bound_file Simple feature object containing the subnational boundaries
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Land cover data in hectares for each combination of GADM country and
#'      subnational division
#'
#' @export
land_cover <- function(gadm_rast_file, subnat_bound_file, workdir_dir=getwd()) {
  #### Calculate hectares by country-BIOME combination ####
  #Load in the GADM raster
  gadm_rast <- terra::rast(gadm_rast_file)

  #Now do BIOME Raster
    #List the rasters containing hectares of land covers
    baseyr_lc_ha_list <- list.files(path = file.path(workdir_dir, 'workdir/rasters/'),
                                    pattern = "ha.tif",
                                    full.names = TRUE)
    #Load the cropland hectares raster so we can use its dimensions when
    #rasterizing the region (REG) and biome (BIOME) shapefiles
    cropland_rast <- terra::rast(baseyr_lc_ha_list[[1]])
  #Load in rds file containing the sf object for subnational areas
  BIOME_vect <- readRDS(subnat_bound_file) %>%
    dplyr::rename(biome_num = subnat_num,
                  biome_name = subnat_name)
  BIOME_vect <- sf::st_as_sf(BIOME_vect)
  BIOME_vect <- terra::vect(BIOME_vect)
  BIOME_rast <- terra::rast(crs = terra::crs(cropland_rast), resolution = terra::res(cropland_rast), extent = terra::ext(cropland_rast))
  #Define the levels for the raster
  set_BIO_list <- readRDS(file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
  BIOME_cats <- data.frame(ID=1:length(set_BIO_list), BIO=set_BIO_list)
  #Make a rest of world category with value equal to the final number in specified biome list
  restofworld_num <- as.numeric(length(set_BIO_list))
  BIOME_rast <- terra::rasterize(BIOME_vect, BIOME_rast, 'biome_num', background = restofworld_num)
  #Replace NA values with "7" for the rest of the world
  levels(BIOME_rast) <- BIOME_cats
  terra::writeRaster(BIOME_rast, filename = file.path(workdir_dir, 'workdir/rasters/BIOME_rast.tif'), overwrite = T)

  #Concatenate two categorical rasters to get one that shows the combinations of their levels
  #REG and BIOMES
  GADM_BIOME_rast <- terra::concats(gadm_rast, BIOME_rast)

  ###THIS SECTION SPECIFIC TO BRAZIL ZERO DEFORESTATION PROJECT
  #####
    #Reclassify any countries that aren't Brazil with areas in biomes other than 7
    GADM_BIOME_values <- terra::values(GADM_BIOME_rast) %>% unique() %>%
      as.data.frame() %>%
      dplyr::rename(ID = GADM_BIO)
    GADM_BIOME_levels <- levels(GADM_BIOME_rast) %>% unique() %>% data.frame()
    GADM_BIOME_levels <- dplyr::left_join(GADM_BIOME_values, GADM_BIOME_levels, by = c('ID'))
    #Select the ones with BIOMES 1-6 and not starting with BRA
    GADM_BIOME_replace <- GADM_BIOME_levels %>%
      dplyr::filter(!stringr::str_detect(GADM_BIO, "_ROW")) %>%
      dplyr::filter(!stringr::str_detect(GADM_BIO, "BRA"))
    GADM_BIOME_replace <- GADM_BIOME_replace %>%
      dplyr::mutate(ID_old = ID,
                    GADM_BIO_old = GADM_BIO,
                    GADM_BIO = substr(GADM_BIO, 1, 4),
                    GADM_BIO = paste0(GADM_BIO, "ROW")) %>%
      dplyr::select(ID_old, GADM_BIO_old, GADM_BIO)
    #Merge to the original levels object to get the new values we'll use to reclassify
    GADM_BIOME_replace <- dplyr::left_join(GADM_BIOME_replace, GADM_BIOME_levels, by = c('GADM_BIO'))
    #Make a reclassification matrix
    rcl_mat <- GADM_BIOME_replace %>%
      dplyr::select(ID_old, ID) %>%
      as.matrix()
    #Reclassify matrix
    GADM_BIOME_output_rast <- terra::classify(GADM_BIOME_rast, rcl_mat)
    GADM_BIOME_output_rast_values <- values(GADM_BIOME_output_rast) %>% unique() %>% data.frame() %>%
      dplyr::rename(ID = GADM_BIO)
    GADM_BIOME_output_rast_levels <- dplyr::left_join(GADM_BIOME_output_rast_values, GADM_BIOME_levels, by = c('ID')) %>%
      dplyr::filter(!is.na(GADM_BIO))
    levels(GADM_BIOME_output_rast) <- GADM_BIOME_output_rast_levels
  GADM_BIOME_rast <- GADM_BIOME_output_rast
  #####

  terra::writeRaster(GADM_BIOME_rast, filename = file.path(workdir_dir, 'workdir/rasters/GADM_BIOME_rast.tif'), overwrite = TRUE)
  #Remove old objects, clean up environment
  rm(cropland_rast, gadm_rast, BIOME_rast, BIOME_vect, GADM_BIOME_output_rast,
     GADM_BIOME_levels, GADM_BIOME_values, GADM_BIOME_replace, GADM_BIOME_output_rast_levels,
     GADM_BIOME_output_rast_values)
  gc()

  #Load all the land cover rasters with data in hectares
    base_year_ha_rasts <- lapply(baseyr_lc_ha_list, terra::rast)
    #Need to rename the raster layers
    names(base_year_ha_rasts[[1]]) <- 'cropland_ha'
    names(base_year_ha_rasts[[2]]) <- 'forest_ha'
    names(base_year_ha_rasts[[3]]) <- 'grassland_ha'
    names(base_year_ha_rasts[[4]]) <- 'other_ha'
    names(base_year_ha_rasts[[5]]) <- 'pasture_ha'
    names(base_year_ha_rasts[[6]]) <- 'shrubland_ha'
    names(base_year_ha_rasts[[7]]) <- 'urban_ha'

  #Output hectares for each REG-BIOME combination
  #Make a stack of the REG-BIOME and land cover rasters, turn into df, and summarize by REG and BIOME
  output_df <- c(GADM_BIOME_rast,
                 base_year_ha_rasts[[1]],
                 base_year_ha_rasts[[2]],
                 base_year_ha_rasts[[3]],
                 base_year_ha_rasts[[4]],
                 base_year_ha_rasts[[5]],
                 base_year_ha_rasts[[6]],
                 base_year_ha_rasts[[7]]) %>%
    as.data.frame()
  output_df <- output_df %>% dplyr::filter(!is.na(GADM_BIO)) %>%
    dplyr::group_by(GADM_BIO) %>%
    summarise_all(sum, na.rm = TRUE)
  #Save
  saveRDS(output_df, file = file.path(workdir_dir, 'workdir/base_year/GADM_BIO_landcover_rasterdata_ha.rds'))
}
