#' Creates
#'
#' @param workdir_dir Location of the workdir dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @return Outputs ....
#' @export
build_har_sets <- function(workdir_dir=getwd()){
    setwd(file.path(workdir_dir, 'workdir/sets/'))
    ## The STI files are distributed with the R package
    system(paste("modhar -sti", system.file("./sti/create_sets.sti", package = "gtapshape")),ignore.stdout=TRUE)
    setwd(workdir_dir)
}
