#' Negative to zero
#'
#' Replace negative raster values with zeros.
#'
#' @param a A terra SpatRaster object
#' @return Returns the terra SpatRaster with negative values replaced by zeroes
#' @export
neg_to_zero <- function(a) {
  d <- a
  d[d[]<0] <- 0
  return(d)
}
