#' Creates a Dataframe with Coordinates for Country-Geographies Boundaries
#'
#' The resulting dataframe is used to aggregated the gridded datasets
#'     on land use and land cover to the country-geographies of
#'     interest. The function is a wrapper of
#'     \link{make_country_biome_raster} and
#'     \link{round_up_coordinates}.
#'
#' @param subnat_bound_file Simple feature object containing the
#'     subnational boundaries
#'
#' @return A dataframe with the xy coordinates of each earth gridcell
#'     and a column with the categoris for the different
#'     countrry-geopgraphies defined in subnat_bound_file.
#'
#'@export
make_subnatbound_gridded_dataframe <- function(subnat_bound_file){
    GADM_BIOME_rast <- make_subnatbound_raster(subnat_bound_file)
    g <- as.data.frame(GADM_BIOME_rast, xy = TRUE)
    g <- round_up_coordinates(raster.df=g)
    g$iso3 = with(g,substr(GADM_BIO, 1, 3))
    g$bio = with(g, substr(GADM_BIO, 5, nchar(paste0(GADM_BIO))))
    return(g)
    }

