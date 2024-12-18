#' National Crop Production Data (2011-2022)
#'
#' A dataset containing crop production information for various countries from 2011 to 2022.
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{iso3}{character. ISO 3166-1 alpha-3 country code.}
#'   \item{year}{integer. Year of production, ranging from 2011 to 2022.}
#'   \item{item}{character. Name of the crop or agricultural product.}
#'   \item{item_code_cpc}{character. Central Product Classification (CPC) code for the item.}
#'   \item{item_code_fao}{integer. Food and Agriculture Organization (FAO) code for the item.}
#'   \item{element}{character. Type of data element (e.g., "production").}
#'   \item{unit}{character. Unit of measurement (e.g., "t" for tonnes).}
#'   \item{value}{numeric. Quantity of production or other measured element.}
#' }
#'
#' @details This dataset provides annual crop production data for various countries,
#'   identified by their ISO3 codes. It includes information on different agricultural
#'   products, their classification codes (CPC and FAO), and production quantities.
#'   The data covers the period from 2011 to 2022.
#'
#' @source Food and Agriculture Organization of the United Nations (FAO)
#'
#' @examples
#' data(iso.crop.production.2011.2022)
#' head(iso.crop.production.2011.2022)
#'
#' # Get production data for a specific country and year
#' subset(iso.crop.production.2011.2022, iso3 == "afg" & year == 2015)
#'
#' # Calculate total production of almonds for Afghanistan over all years
#' afghan_almonds <- subset(iso.crop.production.2011.2022,
#'                          iso3 == "afg" & item == "Almonds, in shell")
#' sum(afghan_almonds$value)
"iso.crop.production.2011.2022"
