#' Calculate hectares in each land cover (Modified by NV)
#'
#' Calculates hectares using the rasters for each of the 7 land cover types
#' created by the combine_land_covers function
#'
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#'
#' @return Creates 7 rasters, one for each land cover type, containing hectares
#'   in each grid cell
#' @export
calc_land_cover_hectares2 <- function(workdir_dir=getwd()) {
  #Load in the land cover rasters containing the fraction of each cell
  baseyr_lc_frac_list <- list.files(path = file.path(workdir_dir, 'workdir/rasters/'),
                                    pattern = "fraction.tif",
                                    full.names = TRUE)
  base_year_frac_rasts <- lapply(baseyr_lc_frac_list, terra::rast)

  #Get the area of grid cells using the terra package
  grid_cell_area <- terra::cellSize(base_year_frac_rasts[[1]], mask=FALSE, lyrs=FALSE, unit="ha", transform=TRUE)

  #Create new rasters showing the hectares in each land cover type
    lapply(1:7, function(x) {
        ## Vector of names as used in land_cover() by Micah:
        .n <- c('cropland_ha', 'forest_ha', 'grassland_ha', 'other_ha',
                'pasture_ha', 'shrubland_ha', 'urban_ha')
        cover_rast_ha <-  base_year_frac_rasts[[x]] * grid_cell_area
        ## Rename variable, as done in land_cover()
        names(cover_rast_ha) <- .n[x]
        ##out_name <- paste0(baseyr_lc_frac_list[x])
        ##out_name <- gsub('fraction.tif', 'ha.tif', out_name)
        out_name <- file.path(workdir_dir, 'workdir/rasters', paste(.n[x], ".tif", sep=""))
        writeRaster(cover_rast_ha, filename = paste0(out_name), overwrite = TRUE)
  })

  #Cleanup
    rm(list=ls())
    gc()
}

