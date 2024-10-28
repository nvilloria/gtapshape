#' Aggregate gridded land use and land cover to countries and geographic
#'     boundaries of interest
#'
#'@param GADM_BIOME_df Dataframe mapping the centroids of
#'     (five-minute?) grid-cell coordinate to countries and geographic
#'     boundaries of interest. This file is created by
#'     \link{land_cover}
#'@param path.file.to.aggregate Character string with the path to a
#'     dataframe with gridded information on land use (crop output and
#'     harvested area) or land cover. These dataframes need to have
#'     two columns labeled x an y with the coordinates of the
#'     centroids of each five minutes gridcell on earth, under the
#'     same projection GADM_BIOME_df
#' @return A data frame with the columns of `file.to.aggregate` (other
#'     than x,y) aggregated to the countries and geographic boundaries
#'     defined by GADM_BIOME_df.
#' @export
aggregate_to_country_biome <- function(GADM_BIOME_df, path.file.to.aggregate){
    ## path.file.to.aggregate <- gridded.livestock.file.names[[1]]
    ## require(dplyr)
    ## Create a temporary environment within the function to hold the
    ## data to be aggregated:
    temp.env <- new.env()
    load(path.file.to.aggregate, envir = temp.env)
    loaded_name <- ls(temp.env)[1]
    data.to.aggregate <- temp.env[[loaded_name]]
    j <- right_join(GADM_BIOME_df, data.to.aggregate, by=c("x","y"))
    m <- j %>%
        dplyr::select(!c(x,y)) %>%
        dplyr::filter(!is.na(GADM_BIO)) %>%
        dplyr::group_by(GADM_BIO) %>%
        dplyr::summarise_all(sum, na.rm = T)
    return(m)
}



