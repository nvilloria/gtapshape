#' Extract Sets with Geographies of Interest from the sf Boundaries
#'
#' @param subnat_bound_file Simple feature object containing the subnational boundaries
#'
#' @return A character string with the names of the geographies of interest
#' @export
extract_bio_sets <- function(subnat_bound_file){
    ##Load in shapefile
    subnat_bound <- readRDS(subnat_bound_file)
    subnat_bound_key <- subnat_bound %>%
    sf::st_drop_geometry() %>%
    dplyr::select(subnat_name, subnat_num) %>%
    unique() %>%
    dplyr::mutate(subnat_name = as.character(subnat_name),
                  subnat_num = as.numeric(subnat_num)) %>%
    dplyr::arrange(subnat_num)
  #Make a Rest of World - ROW category for any area not covered by the shapes
  subnat_bound_key <- rbind(subnat_bound_key, data.frame(subnat_name = "ROW", subnat_num = (length(subnat_bound$subnat_name) + 1)))
  #Make a new column with the set names
  subnat_bound_key <- subnat_bound_key %>%
    dplyr::mutate(BIO = subnat_name,
                  BIO = toupper(BIO))
  #Store the names of the biomes to save as the set items
    set_BIO_list <- subnat_bound_key$BIO
    return(set_BIO_list)
    }
