#' Write HAR file with physical land use and cover for 160 GTAP regions and subantional bounds
#'
#' @param gsc3.by.iso A list with the dataframes of physical land use
#'     and cover data by GTAP region and subnational boundaries. Each
#'     dataframe must have columns labeled 'reg', 'subnatbound`,
#'     `gsc3`, and value.  This function is a wrapper of the steps
#'     needed to write har files using `HARr::write_har()`.
#' @param file Name of the har file with the aggregated physical
#'     data. Should have an extension. Defaults to "gtaplulc.har"
#'
#'
#' @export
write.gtaplulc.har <- function(gsc3.by.iso, file = "gtaplulc.har") {
  ## Prepare data arrays (ordered following the GTAP convention) for
  ## writing har files:
    tohar <- lapply( gsc3.by.iso , function (.i) {
      da <- reshape::cast( .i, reg ~ subnatbound ~  gsc3, value = 'value' )
      da[is.na(da)] <- 0
      ## Assign attributes for har files:
      attr(da,'description') <- attr(.i,'description')
      return(da)
    }
    )
  ## devtools::install_git('https://github.com/USDA-ERS/MTED-HARr.git')
  HARr::write_har(tohar, file)
}
