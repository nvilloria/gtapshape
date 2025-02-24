#' Livestock heads by country, year, and category (2011-2022)
#'
#' This dataset provides the number of livestock heads for different categories
#' across countries from 2011 to 2022, based on FAO data.
#'
#' @format A tibble with 6,891 rows and 5 columns:
#' \describe{
#'   \item{iso3}{character. ISO 3166-1 alpha-3 country code}
#'   \item{year}{integer. Year of observation}
#'   \item{gsc3}{character. Livestock category code (ctl: cattle, rmk: milk animals, wol: wool animals)}
#'   \item{value}{double. Number of livestock heads}
#'   \item{unit}{character. Unit of measurement (1000heads)}
#' }
#'
#' @source Food and Agriculture Organization (FAO) of the United
#'     Nations (See vignette "Download FAO data" for details).
#'
#' @details
#' This dataset is derived from FAO statistics and provides information on
#' livestock populations across countries. The data collection and processing
#' procedures are detailed in the vignette "Download FAO data".
#'
#' The 'gsc3' column uses the following codes:
#' - ctl: cattle
#' - rmk: milk-producing animals (likely includes cattle, goats, and sheep)
#' - wol: wool-producing animals (likely includes sheep and possibly goats)
#'
"iso.gsc3lstk.heads.2011.2022"
