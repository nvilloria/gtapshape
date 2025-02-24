#' Livestock output value by country, year, and category (2011-2022)
#'
#' This dataset provides the output value of livestock products for different categories
#' across countries from 2011 to 2022, based on FAO data.
#'
#' @format A tibble with 5,697 rows and 5 columns:
#' \describe{
#'   \item{iso3}{character. ISO 3166-1 alpha-3 country code}
#'   \item{year}{integer. Year of observation}
#'   \item{gsc3}{character. Livestock category code (rmk: milk products, wol: wool products)}
#'   \item{unit}{character. Unit of measurement (USD1000)}
#'   \item{value}{double. Output value in thousands of US dollars}
#' }
#'
#' @source Food and Agriculture Organization (FAO) of the United Nations.
#'
#' @details
#' This dataset is derived from FAO statistics and provides information on
#' the economic output of livestock products across countries. The data collection
#' and processing procedures are detailed in the vignette "Download FAO data".
#'
#' The 'gsc3' column uses the following codes:
#' - rmk: milk products
#' - wol: wool products
#'
#' Note that unlike the livestock heads dataset, this output value dataset does not
#' include a 'ctl' (cattle) category, focusing instead on milk and wool products.
"iso.gsc3lstk.outputvalue.2011.2022"
