#' Make a Global Raster
#'
#'This empty raster with has a set of properties that are imposed on
#' all the rasters in the package in order to ensure that they align
#' once they are converted to dataframes. This could be more directly
#' achieved using the package raster, but because the sheer number of
#' raster files (and their sheer sizes) working with the rasters
#' directly is impractical.
make.global.raster <- function(){
    terra::rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90,
                resolution = c(0.08333333, 0.08333333),
                crs = "EPSG:4326")
}
