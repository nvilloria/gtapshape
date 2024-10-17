#### Cleanup - Function to delete intermediate files ####
#Define function to remove intermediate files
#This deletes files in the "processed_data" folder
#' Function to remove intermediate files
#'
#' @param parent_dir Path to the parent directory
#'
#' @return Deletes files in the processed_data folder - NOTE - currently does
#'      not delete any files.
#' @export
#'
#' @examples
#' cleanup_intermediate_files("mydir/gtapfiles/")
cleanup_intermediate_files <- function(parent_dir) {
  #Set working directory to user specified direct
  setwd(paste0(parent_dir, "/GTAP_BIOMES/processed_data"))
  #delete files in each folder
  #unlink("sets/*")
  #unlink("gtap_ref_year/*")
  #unlink("concordances/*")
  #unlink("base_year/*")
  #unlink("shapefiles/*")
}

