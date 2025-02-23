#' Creates a Dataframe with Coordinates for Country-Geographies Boundaries
#'
#' The resulting dataframe is used to aggregated the gridded datasets
#'     on land use and land cover to the country-geographies of
#'     interest. The function is a wrapper of
#'     \link{make_subnatbound_raster} and
#'     \link{round_up_coordinates}.
#'
#' @param subnat_bound_file Simple feature object containing the
#'     subnational boundaries. The default subnational boundaries are
#'     Country by AEZ 18 (see [aez18]). [biomes14] is available for
#'     lazy load, e.g. subnat_bound_file='biome14'. Other sf objects
#'     with subnational boundaries can be passed on by using the full
#'     path of the file.
#'
#' @return A dataframe with the xy coordinates of each earth gridcell
#'     and a column with the categoris for the different
#'     countrry-geopgraphies defined in subnat_bound_file.
#'
#' @seealso [make_subnatbound_raster()] which produces the raster with
#'     country-subnational boundaries by intersecting polygon files
#'     with countries and subnational boundaries.
#'
#'@export
make_subnatbound_gridded_dataframe <- function(subnat_bound_file='aez18'){
    GADM_BIOME_rast <- make_subnatbound_raster(subnat_bound_file=subnat_bound_file)
    g <- as.data.frame(GADM_BIOME_rast, xy = TRUE)
    g <- round_up_coordinates(raster.df=g)
    g$iso3 = tolower(with(g,substr(GADM_subnatbound, 1, 3)))
    g$subnatbound = tolower(with(g, substr(GADM_subnatbound, 5, nchar(paste0(GADM_subnatbound)))))
    return(g)
    }

