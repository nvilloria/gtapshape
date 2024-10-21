#' Combine data to create 7 different land covers
#'
#' Uses cropland, pastureland, urbanland, and potential vegetation data to
#' create 7 land cover rasters. Cropland and pastureland are from Ramankutty
#' et al (2008). Urban land data are from Schneider, A., Friedl, M. A., &
#' Potere, D. (2010). Potential vegetation data are from Ramankutty and Foley
#'
#' @param cropland_rast_file Path to the raster containing cropland area
#' @param pasture_rast_file Path to the raster containing pasture area
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Creates 7 rasters, one for each land cover type. Each raster
#'    is a categorical raster indicating whether a cell is the respective
#'    land cover
#' @export
combine_land_covers <- function(cropland_rast_file, pasture_rast_file, workdir_dir=getwd()) {

  #Cropland and pasture coverage
      #Cropland
      cropland_rast <- terra::rast(cropland_rast_file)
      #Pasture
      pasture_rast <- terra::rast(pasture_rast_file)
  #Urban - created by urban_land_cover function
    urban_rast <- terra::rast(file.path(workdir_dir, 'workdir/rasters/urban_cover.tif'))

  #Join the different ones together
    rast_stack <- c(cropland_rast, pasture_rast, urban_rast)
    #Now make a new raster adding the others together
    sum_covers <- rast_stack[[1]] + rast_stack[[2]] + rast_stack[[3]]
      #some values look like they're above one, so more than 100% landcover
      sum_covers_vals <- c(values(sum_covers))
      sum_covers_df <- as.data.frame(sum_covers_vals)
      sum_covers_df <- sum_covers_df %>% dplyr::mutate(vals = as.numeric(sum_covers_vals))
      sum_covers_df <- sum_covers_df %>% dplyr::filter(!is.na(vals))

  #Now we need to normalize the cells with total covers greater than 1
    rm(cropland_rast, pasture_rast, urban_rast)
    gc()
    #Make reclassification matrix
    m <- c(-1, 1, 1)
      #Make values that aren't >1 equal 1 so when we divide the rasters by it it
      #values below one stay the same and those above 1 are normalzied
    rclmat <- matrix(m, ncol = 3, byrow = TRUE)
    sum_covers_over1 <- terra::classify(sum_covers, rclmat)

    #Make new normalized rasters
    norm_crop <- rast_stack[[1]]/sum_covers_over1
    norm_pasture <- rast_stack[[2]]/sum_covers_over1
    norm_urban <- rast_stack[[3]]/sum_covers_over1

    #Replace NA values with 0's and save the new normalized rasters
    norm_crop <- terra::classify(norm_crop, cbind(NA, 0))
    terra::writeRaster(norm_crop, filename = file.path(workdir_dir, 'workdir/rasters/crop_cover_fraction.tif'), overwrite = TRUE)
    norm_pasture <- terra::classify(norm_pasture, cbind(NA, 0))
    terra::writeRaster(norm_pasture, filename = file.path(workdir_dir, 'workdir/rasters/pasture_cover_fraction.tif'), overwrite = TRUE)
    norm_urban <- terra::classify(norm_urban, cbind(NA, 0))
    terra::writeRaster(norm_urban, filename = file.path(workdir_dir, 'workdir/rasters/urban_cover_fraction.tif'), overwrite = TRUE)

  #Last step is to read in the four potential vegetation raster to fill in the missing areas
    forest_pot_veg <- terra::rast(file.path(workdir_dir, 'workdir/rasters/forest_pot_veg.tif'))
    shrubland_pot_veg <- terra::rast(file.path(workdir_dir, 'workdir/rasters/shrubland_pot_veg.tif'))
    savanna_grass_pot_veg <- terra::rast (file.path(workdir_dir, 'workdir/rasters/savanna_grass_pot_veg.tif'))
    other_pot_veg <- terra::rast(file.path(workdir_dir, 'workdir/rasters/other_pot_veg.tif'))

    #Make a raster where the parcels without cropland, pasture, or urban cover are empty
    #so we can then fill in those areas with the potential vegetation types.
      #Other vegetation types like tundra, bare rock, etc.
      check_norm_sums <- norm_crop + norm_pasture + norm_urban
     #Now make a raster showing where we need to fill in land cover using the
      #potential vegetation rasters.
      pot_veg_to_fill <- 1 - check_norm_sums
    #Make final forest cover
      final_forest_cov <- forest_pot_veg * pot_veg_to_fill
      final_forest_cov <- terra::classify(final_forest_cov, cbind(NA, 0))
      terra::writeRaster(final_forest_cov, filename = file.path(workdir_dir, 'workdir/rasters/forest_cover_fraction.tif'), overwrite = TRUE)
    #Make final shrubland cover
      final_shrubland_cover <- shrubland_pot_veg * pot_veg_to_fill
      final_shrubland_cover <- terra::classify(final_shrubland_cover, cbind(NA, 0))
      writeRaster(final_shrubland_cover, filename = file.path(workdir_dir, 'workdir/rasters/shrubland_cover_fraction.tif'), overwrite = TRUE)
    #Make final grassland cover
      final_grass_cover <- savanna_grass_pot_veg * pot_veg_to_fill
      final_grass_cover <- classify(final_grass_cover, cbind(NA, 0))
      terra::writeRaster(final_grass_cover, filename = file.path(workdir_dir, 'workdir/rasters/grassland_cover_fraction.tif'), overwrite = TRUE)
    #Make final other cover
      final_other_cover <- other_pot_veg * pot_veg_to_fill
      final_other_cover <- terra::classify(final_other_cover, cbind(NA, 0))
      terra::writeRaster(final_other_cover, filename = file.path(workdir_dir, 'workdir/rasters/other_cover_fraction.tif'), overwrite = TRUE)

    #Cleanup
      rm(list=ls())
      gc()
}
