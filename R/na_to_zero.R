#' NA to zero
#'
#' This function replaces NA values in a terra SpatRaster with zeroes
#'
#' @param a A terra SpatRaster
#' @return A terra SpatRaster where cells with NA values now have zeroes
#' @export
na_to_zero <- function(a) {
  d <- a
  d[is.na(d[])] <- 0
  return(d)
}
