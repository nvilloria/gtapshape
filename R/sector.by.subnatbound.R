#' @export
sector.by.subnatbound <- function(iso.data, shares, units, sector) {
  # Create dynamic column names
  sector_sym <- sector

  # Merge data
  merged_data <- merge(shares, iso.data, by = c('iso3', sector_sym), all.x = TRUE)
  merged_data$value <- merged_data$value * merged_data$subnatbound.share

  merged_data <- merge(merged_data, regional.concordance, by = "iso3", all.x = TRUE)

  # Aggregate
  agg_formula <- as.formula(paste("value ~", paste("reg", "subnatbound", sector_sym, sep = " + ")))
  result <- aggregate(agg_formula, data = merged_data, sum, na.rm = TRUE)

  # Add units
  result$units <- units

  # Sort the result
  result <- result[do.call(order, result[c("reg", "subnatbound", sector_sym)]), ]
  rownames(result) <- NULL

  return(result)
}
