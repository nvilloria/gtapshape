#' National Crop Harvested Area Data (2011-2022)
#'
#' A dataset containing information on harvested areas of various crops for different countries from 2011 to 2022.
#'
#' @format A data frame with 7 variables:
#' \describe{
#'   \item{iso3}{character. ISO 3166-1 alpha-3 country code.}
#'   \item{year}{integer. Year of data, ranging from 2011 to 2022.}
#'   \item{item}{character. Name of the crop or agricultural product.}
#'   \item{item_code_cpc}{character. Central Product Classification (CPC) code for the item.}
#'   \item{item_code_fao}{integer. Food and Agriculture Organization (FAO) code for the item.}
#'   \item{element}{character. Type of data element (e.g., "area_harvested").}
#'   \item{unit}{character. Unit of measurement (e.g., "ha" for hectares).}
#'   \item{value}{numeric. Size of harvested area or other measured element.}
#' }
#'
#' @details This dataset provides annual data on harvested areas for various crops across different countries,
#'   identified by their ISO3 codes. It includes information on different agricultural products,
#'   their classification codes (CPC and FAO), and the size of harvested areas. The data covers
#'   the period from 2011 to 2022.
#'
#' @source Food and Agriculture Organization of the United Nations (FAO)
#'
#' @examples
#' data(iso.crop.harea.2011.2022)
#' head(iso.crop.harea.2011.2022)
#'
#' # Get harvested area data for a specific country and year
#' subset(iso.crop.harea.2011.2022, iso3 == "afg" & year == 2015)
#'
#' # Calculate total harvested area of almonds for Afghanistan over all years
#' afghan_almonds_area <- subset(iso.crop.harea.2011.2022,
#'                               iso3 == "afg" & item == "Almonds, in shell")
#' sum(afghan_almonds_area$value)
"iso.crop.harea.2011.2022"
