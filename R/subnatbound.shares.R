#' @export
subnatbound.shares <- function(subnatbound.levels, sector) {
  # Create a formula dynamically
  formula_subnat <- as.formula(paste("subnatbound.value ~",
                                     paste("iso3", "subnatbound", sector, sep = " + ")))
  formula_national <- as.formula(paste("subnatbound.value ~",
                                       paste("iso3", sector, sep = " + ")))

  # Calculate subnatbound.value
  s <- aggregate(formula_subnat,
                 data = subnatbound.levels,
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
