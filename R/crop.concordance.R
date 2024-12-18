#' Crop Concordance Table
#'
#' A dataset providing concordance between different crop classification systems.
#'
#' @format A tibble with 175 rows and 4 columns:
#' \describe{
#'   \item{use}{character. Common name or use of the crop.}
#'   \item{item_code_fao}{integer. FAO (Food and Agriculture Organization) item code.}
#'   \item{item_code_cpc}{character. CPC (Central Product Classification) item code.}
#'   \item{gsc3}{character. GSC3 (Global Strategy to improve Agricultural and Rural Statistics) crop group code.}
#' }
#' @details This dataset provides a mapping between different crop classification
#'   systems, including FAO codes, CPC codes, and GSC3 crop group codes. It can be
#'   used to harmonize crop data from different sources or to translate between
#'   classification systems.
#'
#' @source Various international agricultural classification systems (FAO, UN Statistics Division, Global Strategy)
#'
#' @examples
#' data(crop.concordance)
#' head(crop.concordance)
#'
#' # Find the FAO code for a specific crop
#' crop.concordance[crop.concordance$use == "apple", ]
#'
#' # List all crops in the 'v_f' (presumably vegetables and fruits) GSC3 group
#' subset(crop.concordance, gsc3 == "v_f")
"crop.concordance"
