#' Annual Land Covers by Country (2011-2022)
#'
#' A dataframe containing annual cropland and pastures reported by
#' FAOSTAT, and estimated annual forests, grasslands, shrublands, and
#' other obtained by sharing out unclassified lands in faostat using
#' the data on land cover for 2000 (Micah).
#'
#' @format A data frame with 5 rows and 3 variables:
#' \describe{
#'   \item{iso3}{3-digit ISO country code.}
#'   \item{year}{Data is distributed for 2011-2022.}
#'   \item{gsc3}{cropland and pasture, both reported annually by FAOSTAT; urban is fixed over time;  forest, grassland, shrubland, and other are estimates obtained by sharing out unclassified land in FAOSTAT using gridded land cover data for 2000.}
#' \item{unit}{Physical unit.}
#' \item{value}{Value}
#' }
#'
#'
#' @source The step-by-step procedures to create this dataset can be found in the Country_level_landcover_ts vignette.
"iso.land.cover.2011.2022"
