#' Load a Subnational Boundary Simple Features Object
#'
#' Resolves the `subnat_bound_file` argument used throughout the package: either
#' the name of one of the boundary sets distributed with gtapshape, or the path
#' to an rds file holding a simple features object of your own.
#'
#' @param subnat_bound_file Either `"aez18"`, `"biomes14"`, or the path to an
#'     rds file. A custom object must have `subnat_name` and `subnat_num`
#'     columns; `subnat_num` fixes the order of the SUBN set.
#'
#' @return A simple features object.
#'
#' @keywords internal
#' @noRd
read.subnat.bound.sf <- function(subnat_bound_file = "aez18") {
    if (subnat_bound_file %in% c("aez18", "biomes14")) {
        utils::data(list = subnat_bound_file, package = "gtapshape",
                    envir = environment())
        get(subnat_bound_file, envir = environment())
    } else if (file.exists(subnat_bound_file)) {
        readRDS(subnat_bound_file)
    } else {
        stop("Invalid subnat_bound_file. Use 'aez18', 'biomes14', or a valid ",
             "file path.")
    }
}
