#' Downloads a file and shows a progress bar
#'
#' @param url url from where to download the zipped data
#' @param destfile file to which the zipped data is written on disk
#'
#' @seealso \link{getrawdata} for details on actions taken when the package is loaded.
download_with_progress <- function(url, destfile) {
    require(httr)
    response <- GET(url, write_disk(destfile, overwrite = TRUE), progress())
    return(response$status_code)
}
