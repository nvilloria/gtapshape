#' Calculate hectares in each land cover
#'
#' Calculates hectares using the rasters for each of the 7 land cover types
#' created by the combine_land_covers function
#'
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Creates 7 rasters, one for each land cover type, containing hectares
#'   in each grid cell
#' @export
calc_land_cover_hectares <- function(tmp_dir=getwd()) {
  #Load in the land cover rasters containing the fraction of each cell
  baseyr_lc_frac_list <- list.files(path = file.path(tmp_dir, 'tmp/rasters/'),
                                    pattern = "fraction.tif",
                                    full.names = TRUE)
  base_year_frac_rasts <- lapply(baseyr_lc_frac_list, terra::rast)

  #Get the area of grid cells using the terra package
  grid_cell_area <- terra::cellSize(base_year_frac_rasts[[1]], mask=FALSE, lyrs=FALSE, unit="ha", transform=TRUE)

  #Create new rasters showing the hectares in each land cover type
  lapply(1:7, function(x) {
    cover_rast_ha <-  base_year_frac_rasts[[x]] * grid_cell_area
    out_name <- paste0(baseyr_lc_frac_list[x])
    out_name <- gsub('fraction.tif', 'ha.tif', out_name)
    writeRaster(cover_rast_ha, filename = paste0(out_name), overwrite = TRUE)
  })

  #Cleanup
    rm(list=ls())
    gc()
}
