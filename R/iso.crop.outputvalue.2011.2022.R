#' National Crop Output Value Data (2011-2022)
#'
#' A dataset containing information on the output value of various crops for different countries from 2011 to 2022.
#'
#' @format A tibble with 7 variables:
#' \describe{
#'   \item{iso3}{character. ISO 3166-1 alpha-3 country code.}
#'   \item{year}{integer. Year of data, ranging from 2011 to 2022.}
#'   \item{item}{character. Name of the crop or agricultural product.}
#'   \item{item_code_cpc}{character. Central Product Classification (CPC) code for the item.}
#'   \item{item_code_fao}{integer. Food and Agriculture Organization (FAO) code for the item.}
#'   \item{unit}{character. Unit of measurement (e.g., "USD1000" for thousands of US dollars).}
#'   \item{value}{numeric. Output value of the crop in the specified unit.}
#' }
#'
#' @details This dataset provides annual data on the output value of various crops across different countries,
#'   identified by their ISO3 codes. It includes information on different agricultural products,
#'   their classification codes (CPC and FAO), and their output values in thousands of US dollars.
#'   The data covers the period from 2011 to 2022. The dataset is grouped by year and item_code_cpc.
#'
#' @source Food and Agriculture Organization of the United Nations (FAO)
#'
#' @examples
#' data(iso.crop.outputvalue.2011.2022)
#' head(iso.crop.outputvalue.2011.2022)
#'
#' # Get output value data for a specific country and year
#' subset(iso.crop.outputvalue.2011.2022, iso3 == "afg" & year == 2015)
#'
#' # Calculate total output value of almonds for Afghanistan over all years
#' afghan_almonds_value <- subset(iso.crop.outputvalue.2011.2022,
#'                                iso3 == "afg" & item == "Almonds, in shell")
#' sum(afghan_almonds_value$value)
#'
#' # Average output value by year
#' library(dplyr)
#' iso.crop.outputvalue.2011.2022 %>%
#'   group_by(year) %>%
#'   summarise(avg_value = mean(value, na.rm = TRUE))
"iso.crop.outputvalue.2011.2022"
