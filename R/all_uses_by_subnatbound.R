#' Aggregate gridded land use and land cover to countries and geographic
#'     boundaries of interest
#'
#'@param GADM_subnatbound_df Dataframe mapping the centroids of
#'     (five-minute?) grid-cell coordinate to countries and geographic
#'     boundaries of interest. This file is created by
#'     \link{land_cover}
#'@param gridded.use.file.names a vector of character strings with
#'     the path to the dataframes with gridded information on land use
#'     (crop output and harvested area) or land cover. These
#'     dataframes need to have two columns labeled x and y with the
#'     coordinates of the centroids of each five minutes gridcell on
#'     earth, under the same projection GADM_subnatbound_df
#' @return A data frame with the columns of `file.to.aggregate` (other
#'     than x,y) aggregated to the countries and geographic boundaries
#'     defined by GADM_subnatbound_df.
#' @export
all_uses_by_subnatbound <- function(GADM_subnatbound_df = NULL, gridded.use.file.names = NULL) {
  # Pre-allocate the result list
  n_files <- length(gridded.use.file.names)
  result_list <- vector("list", n_files)

  # Use a for loop instead of lapply
  for (i in seq_len(n_files)) {
    result_list[[i]] <- aggregate_gridded_df_to_subnatbound(
      GADM_subnatbound_df = GADM_subnatbound_df,
      path.file.to.aggregate = gridded.use.file.names[i]
    )
  }

  # Combine results efficiently
  g <- do.call(rbind, result_list)

  return(g)
}
