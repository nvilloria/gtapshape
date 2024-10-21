#' Potential vegetation
#'
#' This function creates rasters for potential vegetation. [NV: explain what these data are used for (in which functions) and also where they are stored)
#'
#' @param pot_veg_file Path to the raster file indicating potential vegetation classes
#' @param workdir_dir Location of the workdir dir create using gtap_setup function. The default is the current working directory set by getwd()
#' @return Saves 4 rasters for the "Forests", "Shrubland",
#'  "Savanna + Grassland", and "Other Lands" potential vegetation types from
#'  the original data produced by Ramankutty & Foley.
#' @export
potential_vegetation <- function(pot_veg_file, workdir_dir=getwd()) {
  #Load in the Potential Vegetation Data from Ramankutty and Foley
  pot_veg_rast <- terra::rast(pot_veg_file)

#Now we need to reclassify them into four categories as in Baldoz (2009)
  rcl_mat <- matrix(c(0, 8, 1,
                      10, 12, 2,
                      8, 10, 3,
                      12, 15, 4), nrow = 4, ncol = 3, byrow = T)
  pot_veg_4cat <- terra::classify(pot_veg_rast, rcl_mat)
  #Label the values
  pot_veg_4cat_values <- c(1,2,3,4)
  pot_veg_4cat_labels <- c("Forests", "Shrubland", "Savanna + Grassland", "Other Lands")
  pot_veg_4cat <- terra::subst( pot_veg_4cat,  pot_veg_4cat_values, pot_veg_4cat_labels)
  #Save
  terra::writeRaster(pot_veg_4cat, filename = file.path(workdir_dir, "workdir/rasters/potential_veg.tif"), overwrite = TRUE)

################################################################################
#Create four rasters, one for each category
  #Create a forest potential veg raster
  rcl_mat <- matrix(c(0, 1, 1,
                      1, 4, 0), nrow = 2, ncol = 3, byrow = T)
  forest_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
  terra::writeRaster(forest_pot_veg, filename = file.path(workdir_dir, "workdir/rasters/forest_pot_veg.tif"), overwrite = TRUE)

  #Shrubland potential veg
    #Note, reclassification matrix terms represent the order of the levels
    #of the categorical raster. So the order is now Forest, Other lands, Savanna,
    #then Shrubland.
  rcl_mat <- matrix(c(0, 3, 0,
                      3, 4, 1), nrow = 2, ncol = 3, byrow = T)
  shrubland_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
  terra::writeRaster(shrubland_pot_veg, filename = file.path(workdir_dir, "workdir/rasters/shrubland_pot_veg.tif"), overwrite = TRUE)

  #Savanna and grassland potential veg
  rcl_mat <- matrix(c(0, 2, 0,
                      2, 3, 1,
                      3, 4, 0), nrow = 3, ncol = 3, byrow = T)
  savanna_grass_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
  terra::writeRaster(savanna_grass_pot_veg, filename = file.path(workdir_dir, "workdir/rasters/savanna_grass_pot_veg.tif"), overwrite = TRUE)

  #Other vegetation types like tundra, bare rock, etc.
  rcl_mat <- matrix(c(0, 1, 0,
                      1, 2, 1,
                      2, 4, 0), nrow = 3, ncol = 3, byrow = T)
  other_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
  terra::writeRaster(other_pot_veg,  filename = file.path(workdir_dir, "workdir/rasters/other_pot_veg.tif"), overwrite = TRUE)

  #Cleanup
  rm(list=ls())
  gc()
}
