#' Aggregate gridded land use and land cover to countries and geographic
#'     boundaries of interest
#'
#'@param GADM_BIOME_df Dataframe mapping the centroids of
#'     (five-minute?) grid-cell coordinate to countries and geographic
#'     boundaries of interest. This file is created by
#'     \link{land_cover}
#'@param gridded.use.file.names a vector of character strings with
#'     the path to the dataframes with gridded information on land use
#'     (crop output and harvested area) or land cover. These
#'     dataframes need to have two columns labeled x and y with the
#'     coordinates of the centroids of each five minutes gridcell on
#'     earth, under the same projection GADM_BIOME_df
#' @return A data frame with the columns of `file.to.aggregate` (other
#'     than x,y) aggregated to the countries and geographic boundaries
#'     defined by GADM_BIOME_df.
#' @export
all_uses_by_subnatbound <- function(GADM_BIOME_df= NULL, gridded.use.file.names=NULL){
    g <- lapply(gridded.use.file.names, function(.r){
        s <- aggregate_gridded_df_to_subnatbound(GADM_BIOME_df=GADM_BIOME_df,
                                                   path.file.to.aggregate=.r)
        return(s)
    })
    g <- do.call(rbind,g)
    return(g)
    }



