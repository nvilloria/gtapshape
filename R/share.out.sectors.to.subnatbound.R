#' Dissagregates national data by subantional boundary and aggregates to GTAP regions
#'
#' @param iso.data A dataframe with national data for a given
#'     year. The structure (column names) of these dataframes must be
#'     identical to the datasets iso.crop.production.2011.2022,
#'     iso.crop.outputvalue.2011.2022, iso.crop.harea.2011.2022,
#'     iso.gsc3lstk.heads.2011.2022,
#'     iso.gsc3lstk.outputvalue.2011.2022 and
#'     iso.land.cover.2011.2022, all distributed with `gtrapshape` and
#'     available for lazy loading.
#' @param shares A dataframe created by `subnatbound.shares()`,
#' @param units The units of the physical data. See vignette
#'     `build.gtapdabase.for.year.and.from.sf` or
#'     `build.dbase.from.sf()` for a programmatic way to get this from
#'     the iso.data.
#' @param sector For crops is 'item_code_cpc'; 'gsc3' for the
#'     rest. See vignette `build.gtapdabase.for.year.and.from.sf` or
#'     `build.dbase.from.sf()` for a programmatic way to get this from
#'     the iso.data.
#'
#' @export
share.out.sectors.to.subnatbound <- function(iso.data, shares, units, sector) {
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
