## ----eval = FALSE-------------------------------------------------------------
#   #Read in the urban land cover raster
#     urban_rast <- terra::rast("./raw_data/urban_land_cover_modis/2002v5_urban_only_geog_mos.bip")
#     #Check the header file
#     hdr_file <- "./raw_data/urban_land_cover_modis/2002v5_urban_only_geog_mos.hdr"
#     hdr_file <- readLines(con = hdr_file)
#     #Display information in header file
#     hdr_file
#     #Correct the extent
#     terra::ext(urban_rast) <- c(-180, 180, -89.979167, 90)

## ----eval = FALSE-------------------------------------------------------------
#  #Reclassify so urban areas have a value of 1, all other areas are 0
#     rcl_mat <- matrix(c(0, 12, 0,
#                         12, 13, 1,
#                         13, 255, 0), nrow = 3, ncol = 3, byrow = T)
#     urban_rast <- terra::classify(urban_rast, rcl_mat, right = T)
# 
#   #Replace NA values with zeroes
#     urban_rast <- terra::classify(urban_rast, cbind(NA, 0))

## ----eval = FALSE-------------------------------------------------------------
#   #Create a new global raster using the "make.global.raster" function with the
#   #target CRS and resolution. We will use it to create a new urban land cover
#   #raster with the same resolution and crs (projection).
#     global_rast <- make.global.raster()
#     #Project the urban cover raster into the new CRS and resample to correct resolution
#     urban_rast <- apply_global_raster_properties(urban_rast, global_rast)
#     #Save resulting output as .tif
#     terra::writeRaster(urban_rast,
#                        filename = file.path(workdir_dir, 'workdir/rasters/urban_cover.tif'),
#                        overwrite = TRUE)
# 
#   #Cleanup
#     rm(list=ls())
#     gc()

## ----eval = FALSE-------------------------------------------------------------
# #Load in the Potential Vegetation Data from Ramankutty and Foley
#   pot_veg_rast <- terra::rast("glpotveg_5min_orig.asc")
# 
# #Now we need to reclassify them into four categories as in Baldoz (2009)
#   rcl_mat <- matrix(c(0, 8, 1,
#                       10, 12, 2,
#                       8, 10, 3,
#                       12, 15, 4), nrow = 4, ncol = 3, byrow = T)
#   pot_veg_4cat <- terra::classify(pot_veg_rast, rcl_mat)
#   #Label the values
#   pot_veg_4cat_values <- c(1,2,3,4)
#   pot_veg_4cat_labels <- c("Forests", "Shrubland", "Savanna + Grassland", "Other Lands")
#   pot_veg_4cat <- terra::subst( pot_veg_4cat,  pot_veg_4cat_values, pot_veg_4cat_labels)

## ----eval = FALSE-------------------------------------------------------------
#   #First create a global raster with the desired CRS and resolution to ensure
#   #the potential vegetation rasters will conform with other raster data.
#     global_rast <- make.global.raster()
# 
#   #Create a forest potential veg raster
#     rcl_mat <- matrix(c(0, 1, 1,
#                         1, 4, 0), nrow = 2, ncol = 3, byrow = T)
#     forest_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
#     #Project the forest potential vegetation raster into the new CRS and
#     #resample to correct resolution
#     forest_pot_veg <- apply_global_raster_properties(forest_pot_veg, global_rast)
#     terra::writeRaster(forest_pot_veg, filename = file.path(workdir_dir, "workdir/rasters/forest_pot_veg.tif"), overwrite = TRUE)
# 
#   #Shrubland potential veg
#     #Note, reclassification matrix terms represent the order of the levels
#     #of the categorical raster. So the order is now Forest, Other lands, Savanna,
#     #then Shrubland.
#     rcl_mat <- matrix(c(0, 3, 0,
#                         3, 4, 1), nrow = 2, ncol = 3, byrow = T)
#     shrubland_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
#     #Project the shrubland potential vegetation raster into the new CRS and
#     #resample to correct resolution
#     shrubland_pot_veg <- apply_global_raster_properties(shrubland_pot_veg, global_rast)
#     terra::writeRaster(shrubland_pot_veg,
#                        filename = file.path(workdir_dir, "workdir/rasters/shrubland_pot_veg.tif"),
#                        overwrite = TRUE)
# 
#   #Savanna and grassland potential veg
#     rcl_mat <- matrix(c(0, 2, 0,
#                         2, 3, 1,
#                         3, 4, 0), nrow = 3, ncol = 3, byrow = T)
#     savanna_grass_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
#     #Project the savanna/grassland potential vegetation raster into the new CRS and
#     #resample to correct resolution
#     savanna_grass_pot_veg <- apply_global_raster_properties(savanna_grass_pot_veg, global_rast)
#     terra::writeRaster(savanna_grass_pot_veg,
#                        filename = file.path(workdir_dir, "workdir/rasters/savanna_grass_pot_veg.tif"),
#                        overwrite = TRUE)
# 
#   #Other vegetation types like tundra, bare rock, etc.
#     rcl_mat <- matrix(c(0, 1, 0,
#                         1, 2, 1,
#                         2, 4, 0), nrow = 3, ncol = 3, byrow = T)
#     other_pot_veg <- terra::classify(pot_veg_4cat, rcl_mat, right = T)
#     #Project the raster for the "other" category of potential vegetation into the
#     #new CRS and resample to correct resolution
#     other_pot_veg <- apply_global_raster_properties(other_pot_veg, global_rast)
#     terra::writeRaster(other_pot_veg,  filename = file.path(workdir_dir, "workdir/rasters/other_pot_veg.tif"), overwrite = TRUE)
# 
#   #Cleanup
#     rm(list=ls())
#     gc()

## ----eval = FALSE-------------------------------------------------------------
#  #Cropland and pasture coverage
#     #First create a global raster with the desired CRS and resolution
#       global_rast <- make.global.raster()
# 
#     #Load cropland raster
#       cropland_rast <- terra::rast("./raw_data/CroplandPastureArea2000_Geotiff/Cropland2000_5m.tif")
#       #Make sure the cropland raster CRS and resolution match other rasters
#       cropland_rast <- apply_global_raster_properties(cropland_rast, global_rast)
# 
#     #Load pastureland raster
#       pasture_rast <- terra::rast("./raw_data/CroplandPastureArea2000_Geotiff/Pasture2000_5m.tif")
#       #Make sure the pastureland raster CRS and resolution match other rasters
#       pasture_rast <- apply_global_raster_properties(pasture_rast, global_rast)

## ----eval = FALSE-------------------------------------------------------------
#   #Urban - created by urban_land_cover function
#     urban_rast <- terra::rast(file.path(workdir_dir, 'workdir/rasters/urban_cover.tif'))
# 
#   #Join the cropland, pastureland, and urban rasters together
#     rast_stack <- c(cropland_rast, pasture_rast, urban_rast)
#     #Now make a new raster adding the others together
#     sum_covers <- rast_stack[[1]] + rast_stack[[2]] + rast_stack[[3]]
#       #some values look like they're above one, so more than 100% landcover
#       sum_covers_vals <- c(values(sum_covers))
#       sum_covers_df <- as.data.frame(sum_covers_vals)
#       sum_covers_df <- sum_covers_df %>% dplyr::mutate(vals = as.numeric(sum_covers_vals))
#       sum_covers_df <- sum_covers_df %>% dplyr::filter(!is.na(vals))
# 
#   #Now we need to normalize the cells with total covers greater than 1. To do
#   #this we create a new raster which we will divide each of the land cover
#   #rasters by.
#     rm(cropland_rast, pasture_rast, urban_rast)
#     gc()
#     #Make reclassification matrix
#     m <- c(-1, 1, 1)
#     #Make values that aren't >1 equal 1. Then when we use the divide the individual
#     #rasters by the resulting raster, all values below one stay the same and those above 1 are normalized
#     rclmat <- matrix(m, ncol = 3, byrow = TRUE)
#     sum_covers_over1 <- terra::classify(sum_covers, rclmat)
# 
#     #Make new normalized rasters for cropland, pastureland, and urban areas
#     norm_crop <- rast_stack[[1]]/sum_covers_over1
#     norm_pasture <- rast_stack[[2]]/sum_covers_over1
#     norm_urban <- rast_stack[[3]]/sum_covers_over1
# 
#     #Replace NA values with 0's and save the new normalized rasters
#     norm_crop <- terra::classify(norm_crop, cbind(NA, 0))
#     terra::writeRaster(norm_crop,
#                        filename = file.path(workdir_dir, 'workdir/rasters/crop_cover_fraction.tif'),
#                        overwrite = TRUE)
#     norm_pasture <- terra::classify(norm_pasture, cbind(NA, 0))
#     terra::writeRaster(norm_pasture,
#                        filename = file.path(workdir_dir, 'workdir/rasters/pasture_cover_fraction.tif'),
#                        overwrite = TRUE)
#     norm_urban <- terra::classify(norm_urban, cbind(NA, 0))
#     terra::writeRaster(norm_urban,
#                        filename = file.path(workdir_dir, 'workdir/rasters/urban_cover_fraction.tif'),
#                        overwrite = TRUE)

## ----eval = FALSE-------------------------------------------------------------
#   #Last step is to read in the four potential vegetation raster to fill in the missing areas
#       forest_pot_veg <- terra::rast(file.path(workdir_dir, 'workdir/rasters/forest_pot_veg.tif'))
#       shrubland_pot_veg <- terra::rast(file.path(workdir_dir, 'workdir/rasters/shrubland_pot_veg.tif'))
#       savanna_grass_pot_veg <- terra::rast (file.path(workdir_dir, 'workdir/rasters/savanna_grass_pot_veg.tif'))
#       other_pot_veg <- terra::rast(file.path(workdir_dir, 'workdir/rasters/other_pot_veg.tif'))
# 
#       #Make a raster indicating the cell are remaining after accounting for the
#       #cropland, pastureland, and urban coverage fractions. Then fill in those
#       #areas with the potential vegetation types.
#         sum_croppasturb_norm_rasters <- norm_crop + norm_pasture + norm_urban
#        #Now make a raster showing where we need to fill in land cover using the
#         #potential vegetation rasters.
#         pot_veg_to_fill <- 1 - sum_croppasturb_norm_rasters
#       #Make final forest cover
#         final_forest_cov <- forest_pot_veg * pot_veg_to_fill
#         final_forest_cov <- terra::classify(final_forest_cov, cbind(NA, 0))
#         terra::writeRaster(final_forest_cov,
#                            filename = file.path(workdir_dir, 'workdir/rasters/forest_cover_fraction.tif'),
#                            overwrite = TRUE)
#       #Make final shrubland cover
#         final_shrubland_cover <- shrubland_pot_veg * pot_veg_to_fill
#         final_shrubland_cover <- terra::classify(final_shrubland_cover, cbind(NA, 0))
#         writeRaster(final_shrubland_cover,
#                     filename = file.path(workdir_dir, 'workdir/rasters/shrubland_cover_fraction.tif'),
#                     overwrite = TRUE)
#       #Make final grassland cover
#         final_grass_cover <- savanna_grass_pot_veg * pot_veg_to_fill
#         final_grass_cover <- classify(final_grass_cover, cbind(NA, 0))
#         terra::writeRaster(final_grass_cover,
#                            filename = file.path(workdir_dir, 'workdir/rasters/grassland_cover_fraction.tif'),
#                            overwrite = TRUE)
#       #Make final other cover
#         final_other_cover <- other_pot_veg * pot_veg_to_fill
#         final_other_cover <- terra::classify(final_other_cover, cbind(NA, 0))
#         terra::writeRaster(final_other_cover,
#                            filename = file.path(workdir_dir, 'workdir/rasters/other_cover_fraction.tif'),
#                            overwrite = TRUE)
# 
#   #Cleanup
#     rm(list=ls())
#     gc()

## ----eval = FALSE-------------------------------------------------------------
# #Load in the land cover rasters containing the fraction of each cell
# baseyr_lc_frac_list <- list.files(path = 'workdir/rasters/',
#                                   pattern = "fraction.tif",
#                                   full.names = TRUE)
# base_year_frac_rasts <- lapply(baseyr_lc_frac_list, terra::rast)
# 
# #Get the area of grid cells in hectares using the terra package
# grid_cell_area <- cellSize(base_year_frac_rasts[[1]], mask=FALSE, lyrs=FALSE, unit="ha", transform=TRUE)
# 
# #Create new rasters showing the hectares in each land cover type
# for (i in c(1:7)) {
#   cover_rast_ha <-  base_year_frac_rasts[[i]] * grid_cell_area
#   out_name <- paste0(baseyr_lc_frac_list[i])
#   out_name <- gsub('fraction.tif', 'ha.tif', out_name)
#   writeRaster(cover_rast_ha, filename = paste0(out_name), overwrite = TRUE)
# }

## ----eval = FALSE-------------------------------------------------------------
# lapply( baseyr_lc_ha_file_names, function(.r){
#     ## (.r <- baseyr_lc_ha_file_names[[5]])
#     r <- terra::rast(.r)
#     filename <- tools::file_path_sans_ext(basename(.r))
#     names(r) <-sub("_ha$", "", filename)
#     ## Ensure raster has the default resolution, extent, and
#     ## coordinate reference system:
#     r <- apply_global_raster_properties(input.raster = r, global.raster = gr)
#     r <- as.data.frame(r, xy=TRUE)
#     ## Keep only positive values to decrease file size:
#     c <- colnames(r)[3]
#     r1 <- r %>% dplyr::filter(!!sym(c)>0)
#     ## Round up geographic coordinates to ensure compatibility with
#     ## the country-geography raster used for aggregation:
#     r1 <- round_up_coordinates(raster.df=r1)
# 
#     assign(filename,r1)
#     supplementary.data.path <- system.file("land_cover_2000", package = "gtapshape")
#     save(list=filename, file= file.path(supplementary.data.path, paste(filename,".rda",sep="")), compress = "xz",
#          compression_level = 9)
# }
# )

