#' Function
#'
#' @param newsets Set file with the ecoregions
#'@export
targetGTAPsets <- function(newsets = NULL){
    ## Sets with different geographies (e.g., AEZ
    ## vs Biome) which we could prepare for distribution of the
    ## package, thus sparing the user to download and process all the
    ## rasters---newsets need to be specified:
    if(is.null(newsets)){
        ns <- file.path(workdir_dir, paste('workdir/sets'), newsets="regbiosets.har")
    }else{
        ns <- file.path(workdir_dir, paste('workdir/sets'), newsets=newsets)
    }
    return(ns)
    }
