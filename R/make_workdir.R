#' Create worning directories
#'
#' This function creates directories in the user-specified locations.
#'
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @return Creates workdir directory along with its sub-directories
#' @export
make_workdir <- function(workdir_dir=getwd()) {
  #workdir directory
  dir.create(path = paste0(workdir_dir, '/workdir'))
    #workdir sub-directories
    dir.create(path = paste0(workdir_dir, '/workdir/rasters'))
    dir.create(path = paste0(workdir_dir, '/workdir/base_year'))
    dir.create(path = paste0(workdir_dir, '/workdir/sets'))
    dir.create(path = paste0(workdir_dir, '/workdir/gtap_ref_year'))
    dir.create(path = paste0(workdir_dir, '/workdir/output_data'))
    dir.create(path = paste0(workdir_dir, '/workdir/fao_data'))
}
