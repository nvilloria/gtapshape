#' Concordance between gridded livestock data and GTAP sectors
#'
#' This dataset provides a concordance between the gridded livestock data (GSC3)
#' and their corresponding use in GTAP sectors.
#'
#' @format A data frame with 8 rows and 2 columns:
#' \describe{
#'   \item{gsc3}{character. The GSC3 code for the livestock category}
#'   \item{use}{character. The corresponding use or animal type in GTAP sectors}
#' }
#'
#' @source Data derived from GTAP and gridded livestock datasets.
#' For more details, see: https://github.com/nvilloria/gtapshape/tree/main/data-raw
#'
#' @details
#' The concordance includes the following mappings:
#' - 'ctl' (cattle) maps to both cattle and small ruminants (goats and sheep)
#' - 'rmk' (milk) maps to cattle, goats, and sheep
#' - 'wol' (wool) maps to goats and sheep
"gridded.livestock.concordance"
