#' Setup gtapshape
#'@export
setup_gtapshape<- function() {
    packageStartupMessage("\n\nWelcome to gtapshape!\n")

    if (!dir.exists('raw_data')){
        ## Use this for testing:
        ## getrawdata(url="https://www.gtap.agecon.purdue.edu/uploads/temp/FlexSpatialAggTest.zip")
        ## Use this for the actual data:
        getrawdata()
    }else{
        cat("A folder named raw_data exists. This suggests that the raw data needed to construct \nthe GIS layers to split the national land use and land cover data is present. If \nfor any reason this is not the case and these data are really needed, run the \nfunction getrawdata().\n\n")
    }
}
