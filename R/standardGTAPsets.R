#' Title
#'
#' @param standardsets Regional and sectoral sets used by the standard GTAP data. The default is V10. [NV: Micah is this right?]
#'@export
standardGTAPsets <- function(standardsets=NULL){
    ## For the standard GTAP sets we can allow for different
    ## specifications too (e.g., V10, V11, etc.. Micah's documentation
    ## says that he was using GTAP V10. which are distributed with
    ## gtapshape. We could also distribute V11, which would have to be
    ## declared as standardsets. If we want to make this more flexible
    ## we would need to have an additional parameter asking whether an
    ## included set is going to be used.
    if(is.null(standardsets)){
        gs <- system.file("har/gtapsets2.har", package = "gtapshape")
    }else{
        gs <- system.file(paste("har/",standardsets,".har",sep=""), package = "gtapshape")
    }
    return(gs)
    }
