#' Setup gtapshape
#'@export
setup_gtapshape<- function() {
    packageStartupMessage("\n\nWelcome to gtapshape!\n")

    if (!dir.exists('raw_data')){
        ## Use this for testing:
        ## getrawdata(url="https://www.gtap.agecon.purdue.edu/uploads/temp/FlexSpatialAggTest.zip")
        ## Use this for the actual data:
        getrawdata(url="https://www.gtap.agecon.purdue.edu/uploads/temp/FlexSpatialAgg.zip")
        cat("\n\nDone downloading and unzipping the necessary data.\n")
    }else{
        cat("A folder named raw_data exists. This suggests that the raw data needed to construct \nthe GIS layers to split the national land use and land cover data is present. If \nfor any reason this is not the case and these data are really needed, run the \nfunction getrawdata().\n\n")
    }

    if (!dir.exists('workdir')){
        cat("\n\nCreating working directory...\n")
        make_workdir()
        cat("\n\nDone creating working directory (Eventually we want to distribute much of these data within the package).\n")
    }else{
        cat("\n\nA folder named workdir exists. This suggests that the [minimum] data needed used to split the national land use and land cover data is present. If for any reason this is not the case and it is highly recommended to, run the function \nfunction setup_gtapshape().\n")
                                                   }
    cat("\n\nChecking that the GEMPACK programs used by gtapshape are properly installed...\n")

    mf <- check_exe_files()

    ## cat("Check that the needed GEMPACK programs are installed.\n")

    ## mf <- check_exe_files()

    if (length(mf$missing)>0) {
        u <- readline(
            prompt = "\n\nDo you want to compile them? (yes/no): ")
        if (tolower(u) == "yes") {
            cat("C\n\n opying and compiling tablo files...\n")
            copy_missing_tab_files(missing_exe_files= mf$missing)
            compile_tab_files()
        }else{
            cat("\n\nSkipping compilation of missing GEMPACK programs.\n")
        return(invisible(NULL))
        }
    }else{
        cat("These are all the GEMPACK programs required by gtapshape.\n")
        }
}
