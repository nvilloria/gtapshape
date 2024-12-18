#' Livestock Concordance Table
#'
#' A dataset providing concordance between different livestock classification systems.
#'
#' @format A data frame with 4 variables:
#' \describe{
#'   \item{use}{character. Common name or use of the livestock.}
#'   \item{item_code_fao}{integer. FAO (Food and Agriculture Organization) item code.}
#'   \item{item_code_cpc}{character. CPC (Central Product Classification) item code.}
#'   \item{gsc3}{character. GSC3 (Global Strategy to improve Agricultural and Rural Statistics) livestock group code.}
#' }
#'
#' @details This dataset provides a mapping between different livestock classification
#'   systems, including FAO codes, CPC codes, and GSC3 livestock group codes. It can be
#'   used to harmonize livestock data from different sources or to translate between
#'   classification systems.
#'
#' @source Various international agricultural classification systems (FAO, UN Statistics Division)
#'
#' @examples
#' data(livestock.concordance)
#' head(livestock.concordance)
#'
#' # Find the FAO code for a specific livestock
#' livestock.concordance[livestock.concordance$use == "Cattle", ]
#'
#' # List all livestock in the 'ctl' (presumably cattle) GSC3 group
#' subset(livestock.concordance, gsc3 == "ctl")
#'
#' # Count the number of livestock types in each GSC3 group
#' table(livestock.concordance$gsc3)
"livestock.concordance"
