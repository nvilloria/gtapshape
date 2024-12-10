#' Apply Global Raster Properties
#'
#' This function applies the properties of a global raster template to an input raster.
#'
#'@param input.raster a SpatRaster object
#'@param global.raster a SpatRaster object with a set of properties
#'     that are imposed on each raster to ensure alignment once
#'     rasters are converted to dataframes.
#'
#'@return a SpatRaster object with the following properties: extent
#'     xmn = -180, xmx = 180, ymn = -90, ymx = 90: resolution =
#'     c(0.08333333, 0.08333333), coordinate reference system: crs =
#'     "+proj=longlat +datum=WGS84 +no_defs"
#' @seealso \link{global.raster} for details on the global raster template used.
#' @seealso \link{round_up_coordinates} to ensure that rasters can be merged with other rasters after converting them to dataframes.
#'
#' @examples
#' \dontrun{
#' anyraster <- raster(matrix(runif(100), 10, 10))
#' goodraster <- apply_raster_properties(anyraster, global.raster=make.global.raster())
#' }
#' @export
apply_global_raster_properties <- function(input.raster, global.raster){
    # Check if input raster is categorical (relevant for GADM_BIOME_rast)
    is_categorical <- terra::is.factor(input.raster)
    ## Ensure the input raster has the correct CRS
    input.raster <- terra::project(input.raster, global.raster)
    ## Resample the input raster to match the template's resolution
    ## resampling_method <- if(is_categorical) "nearest" else "bilinear"
    resampled <- terra::resample(input.raster, global.raster)#, method = "bilinear")
    ## Crop the resampled raster to match the template's extent
    final.raster <- terra::crop(resampled, terra::ext(global.raster))
     # Restore the categories of the raster  (relevant for GADM_BIOME_rast)
    ## if (is_categorical) {
    ##     levels(final.raster) <- levels(input.raster)[[1]]
    ## }
    return(final.raster)
}
