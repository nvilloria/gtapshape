#' Calculate the subnational boundary shares of land uses and covers `
#'
#' @param subnatbound.use Dataframe with the value of different uses
#'     by country (iso3), subnational bound, and use (either crops,
#'     livestock and land cover). This dataframe must have a target
#'     sector to which the uses are aggregated.
#'
#' @param sector The sector to which the uses are aggregated: CPC for
#'     the case of crops and GSC3 for livestock and land covers.
#'
#' @seealso [all_uses_by_subnatbound()] creates dataframes from
#'     gridded data aggregate to subnational boundes. These dataframes
#'     need to be merged with a sectoral concordance, eithr
#'     `crop.concordance`, `gridded.livestock.concordance` or
#'     land.cover.concordance in order to have a target sectors
#'
#' @export
subnatbound.shares <- function(subnatbound.use, sector) {
  # Create a formula dynamically
  formula_subnat <- as.formula(paste("subnatbound.value ~",
                                     paste("iso3", "subnatbound", sector, sep = " + ")))
  formula_national <- as.formula(paste("subnatbound.value ~",
                                       paste("iso3", sector, sep = " + ")))

  # Calculate subnatbound.value
  s <- aggregate(formula_subnat,
                 data = subnatbound.use,
                 FUN = sum,
                 na.rm = TRUE)

  # Calculate national.total
  national_totals <- aggregate(formula_national,
                               data = s,
                               FUN = sum,
                               na.rm = TRUE)
  colnames(national_totals)[ncol(national_totals)] <- "national.total"

  # Merge and calculate subnatbound.share
  s <- merge(s, national_totals, by = c("iso3", sector))
  s$subnatbound.share <- ifelse(s$national.total == 0, 0,
                                s$subnatbound.value / s$national.total)

  # Sort the result
  s <- s[do.call(order, s[c("iso3", sector, "subnatbound")]), ]
  rownames(s) <- NULL
  return(s)
}
