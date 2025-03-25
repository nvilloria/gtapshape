#' Downloads GIS layers and FAO data
#'
#' @description The function downloads a large zipped data (~8GB) with
#'     all the rasters for land cover, land use and ecological and
#'     political boundaries and the FAO data that are needed to split
#'     national land use, land cover and land rents. These data
#'     (including sources) are documented in the companion
#'     article. The data is stored in the GTAP servers.
#'
#' This function is ran automatically when the package is installed
#'     for the first time and can be ran by itself if needed. The
#'     function uses \link{download_with_progress} to handle the
#'     download of the file and showing a progress bar.
#'
#' @param url url from where to download the zipped data.
#'
#' @returns A ~28GB folder in the current working directory named 'raw_data'.
#'
#' @examples
#' \dontrun{getrawdata(url="https://www.gtap.agecon.purdue.edu/uploads/temp/FlexSpatialAgg.zip")}
#'
#' @seealso \link{.onAttach} for details on actions taken when the package is loaded.
#'
#' @export
getrawdata <- function(url) {
  cat("Building the database from GIS layers requires downloading a very large
    compressed file (~3.5 GB zipped, 16.6 GB unzipped). This may take in excess
    of an hour, and it is probably unpractical with a slow internet connection.
    The file has the underlying rasters and shape files used to split national
    land markets. These data are necessary only if there is a need to change a
    GIS layer. Otherwise, these data are not needed. Please refer to the
    package vignettes for documentation of how the underlying rasters and shapefiles
    were processed. \n")

  user_choice <- readline(prompt = "Do you want to download the data? (yes/no): ")

  if (tolower(user_choice) != "yes") {
    ##cat("Download cancelled. Exiting function.\n")
    return(invisible(NULL))
  }

  ## url <- "https://www.gtap.agecon.purdue.edu/uploads/temp/FlexSpatialAggTest.zip"
  ## url <- "https://www.gtap.agecon.purdue.edu/uploads/temp/FlexSpatialAgg.zip"
  zip_file <- "raw_data.zip"

  cat("Downloading the zipped data from the GTAP servers...\n")
  options(timeout = 3600) # Set timeout to 1 hour
  ## utils::download.file(url=url, destfile=zip_file, method="libcurl", quiet = FALSE, mode = "wb", force=TRUE)
  ## cat("Download complete.\n")
  download_with_progress(url, destfile=zip_file)
  cat("Unzipping file...\n")
  unzip(zip_file, exdir = ".")
  cat("Unzip complete.\n")
  # Remove the zip file after extraction
  file.remove(zip_file)
  cat("Zip file removed.\n")
  cat("\n\nDone downloading and unzipping the necessary data.\n")
}
