#' Urban land cover
#'
#' Reading in the urban cover raster - Schneider, A., Friedl, M. A., & Potere,
#' D. (2010). Mapping global urban areas using MODIS 500-m data
#'
#' @param bip_file Path to the bip file containing urban land cover data
#' @param hdr_file Path to the hdr file for the urban land cover data
#' @param cropland_file Path to the file containing cropland data
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Creates urban cover raster
#' @export
urban_land_cover <- function(bip_file, hdr_file, cropland_file, tmp_dir=getwd()) {
  #Read in the urban land cover raster
    urban_rast <- terra::rast(bip_file)
    #Check the header file
    hdr_file <- readLines(con = hdr_file)
    #Correct the extent
    ext(urban_rast) <- c(-180, 180, -89.979167, 90)

  #Reclassify so urban areas have a value of 1, all other areas are 0
    rcl_mat <- matrix(c(0, 12, 0,
                        12, 13, 1,
                        13, 255, 0), nrow = 3, ncol = 3, byrow = T)
    urban_rast <- terra::classify(urban_rast, rcl_mat, right = T)

  #Replace NA values with zeroes
    urban_rast <- terra::classify(urban_rast, cbind(NA, 0))

  #Load in the cropland_cover raster from by Ramankutty et al (2008) and store
  #its resolution and crs. We will use it to create a new urban land cover raster
  #with the same resolution and crs (projection).
    cropland_rast <- terra::rast(cropland_file)
    cropland_res <- res(cropland_rast)
    #Project the urban cover raster into the new CRS, remove cropland_rast object
    urban_rast_wgs1984 <- terra::project(urban_rast, cropland_rast)
    rm(cropland_rast)
    gc()

  #Now change the resolution so it matches the other rasters by aggregating it to 0.083333 degrees
    aggr_factor <- cropland_res[1]/res(urban_rast_wgs1984)[1]
    aggr_factor <- ceiling(aggr_factor)

  #Now aggregate it to the 5 arc minute resolution
    urban_5min_res <- terra::aggregate(urban_rast_wgs1984, aggr_factor, fun = mean)
    terra::writeRaster(urban_5min_res, filename = file.path(tmp_dir, 'tmp/rasters/urban_cover.tif'), overwrite = TRUE)

  #Cleanup
    rm(list=ls())
    gc()
  }
