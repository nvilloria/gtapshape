#' Save LULC final outputs with aggregation .txt file
#'
#' Define function to create new folder and move output files there.
#' Function takes 2 arguments: the parent directory containing the
#' GTAP_BIOMES folder, and the desired name of the folder which will
#' contain the 3 output files.
#' For example:
#' parent_dir <- "C:/Users/Micah/Dropbox/CGE_BIOMES_Carbon_2024/"
#' out_folder_name <- "GTAP_BRAZD7_beefandsoy75"
#'
#' @param gempack_rentcalc_dir Location of the directory where inputs and outputs
#' @param out_folder_name Desired name of folder containing final outputs
#' @param out_folder_dir Location where the output folder should be saved
#' @return Outputs a folder containing 4 files: BIOMEmod.txt (aggregation file),
#'    gtaplulcaez.har (land use and land cover dataset), gtaplulcset.har (file
#'    containing the sets used by GTAP-AEZ model).
#' @export
save_3final_outputs <- function(gempack_rentcalc_dir, out_folder_name, out_folder_dir) {
  #Create a new directory with the user specified name in the user-specified location
  dir.create(file.path(out_folder_dir, out_folder_name), showWarnings = FALSE)
  #List the final output files
  final_outputs <- list.files(path = file.path(gempack_rentcalc_dir, 'out/'), full.names = T)
  #Copy files over
  file.copy(from=final_outputs, to=file.path(out_folder_dir, out_folder_name),
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
}
