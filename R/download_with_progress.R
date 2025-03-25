#' Downloads a file and shows a progress bar
#'
#' @param url url from where to download the zipped data
#' @param destfile file to which the zipped data is written on disk
#'
#' @seealso \link{getrawdata} for details on actions taken when the package is loaded.
#' @export
download_with_progress <- function(url, destfile) {
    response <- httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::progress())
    return(response$status_code)
}

