#' GTAP Regional Concordance
#'
#' A dataset providing concordance between ISO3 country codes and GTAP regions.
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{iso3}{character. ISO 3166-1 alpha-3 country code.}
#'   \item{reg}{character. GTAP region code.}
#' }
#'
#' @details This dataset maps ISO3 country codes to their corresponding GTAP regions.
#'   GTAP (Global Trade Analysis Project) uses a set of 160 regions in its economic modeling.
#'   This concordance allows for easy translation between standard country codes and GTAP's
#'   regional classification system.
#'
#' @source Global Trade Analysis Project (GTAP), Purdue University
#'
#' @examples
#' data(regional.concordance)
#' head(regional.concordance)
#'
#' # Find the GTAP region for a specific country
#' regional.concordance[regional.concordance$iso3 == "afg", ]
#'
#' # List all countries in a specific GTAP region
#' subset(regional.concordance, reg == "xcb")
#'
#' # Count the number of countries in each GTAP region
#' table(regional.concordance$reg)
"regional.concordance"
