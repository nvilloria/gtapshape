#' Create gtaplulcaez.har file
#'
#' This function uses the user defined sets to create a .har file using MODHAR,
#' then it reads in the gempack text files created by gpack_txt
#'
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @param gempack_lulc_dir Location of the directory where inputs and outputs
#'     to LULC GEMPACK executables are stored.
#' @param gempack_timber_dir Location of the directory where inputs and outputs
#'    for the GEMPACK timberland rents executables are
#' @param gempack_rentcalc_dir Location of the directory where inputs and outputs
#'    for the GEMPACK rentcalc executables are
#' @return Outputs gtaplulcaez.har file.
#' @export
gtaplulc <- function(tmp_dir=getwd(), gempack_lulc_dir, gempack_timber_dir, gempack_rentcalc_dir) {
  #Copy the sets we defined in Step 1 to where the GEMPACK code runs
  set_files <- list.files(path = file.path(tmp_dir, 'tmp/sets/'),
                          pattern = '.txt',
                          full.names = T)
  #GEMPACK gtaplulc input folder
  gempack_folder <- file.path(gempack_lulc_dir, 'in/')
  #Copy files over
  file.copy(from=set_files, to=gempack_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  #Run the .BAT file which runs modhar using the .sti file to create a .HAR
  #file containing the user defined sets. The file is regbiosets.har
  shell.exec(file.path(gempack_lulc_dir, "create_sets.bat"))
  #Wait for 5 seconds so the executable finishes running
  Sys.sleep(5)

  #Copy the regbiosets.har file over to the "timber" folder where we will use it
  #to calculate the regional level timberland rents, and then share these rents
  #out by BIOME.
  #Copy file over
  regbioset_har <- file.path(gempack_lulc_dir, 'out/regbiosets.har')
  timber_folder <- file.path(gempack_timber_dir, 'in/')
  file.copy(from=regbioset_har, to=timber_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  #Run the .BAT file which creates regionally aggregated timberland
  #rents
  shell.exec(file.path(gempack_timber_dir, "regional_timber_rents.bat"))
  #Wait for 5 seconds so the executable finishes running
  Sys.sleep(5)

  #Run the .BAT file which runs the TABLO executable "create_gtaplulc_biomes.exe"
  #The TABLO executable loads in the sets defined in Step 1, along with the
  #land cover sets and crop categories from the original gtap database. It then
  #imports the BIOME level data from the .txt files output in Step8.
  #Copy the .txt files from output_data folder where the input folder of the GEMPACK
  #code.
  input_files <- list.files(path = file.path(tmp_dir, 'output_data/'),
                            pattern = '_data.txt',
                            full.names = T)
  #GEMPACK input folder
  gempack_input_folder <- file.path(gempack_lulc_dir, 'in/')
  #Copy files over
  file.copy(from=input_files, to=gempack_input_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)

  #Run the .BAT file which imports the text data and then exports a har file
  shell.exec(file.path(gempack_lulc_dir, "create_gtaplulc_biomes.bat"))
  #Wait for 5 seconds so the executable finishes running
  Sys.sleep(5)

  #Share out land rents
  #Copy regbiosets.har file over
  regbioset_har <- file.path(gempack_lulc_dir, 'out/regbiosets.har')
  rentcalc_folder <- file.path(gempack_rentcalc_dir, 'in/')
  file.copy(from=regbioset_har, to=rentcalc_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  #Copy r_forestdata.har
  r_forestdata_har <- file.path(gempack_timber_dir, 'out/r_forestdata.har')
  file.copy(from=r_forestdata_har, to=rentcalc_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  #Copy over gtaplulc_biomes.har
  gtaplulc_biomes_har <- file.path(gempack_lulc_dir, 'out/gtaplulc_biomes.har')
  file.copy(from=gtaplulc_biomes_har, to=rentcalc_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  # #Run the BAT file
  # shell.exec(file.path(parent_dir, "GTAP_BIOMES/gempack/rentcalc/03_rentcalc.bat"))
  #Run the 03_rentcalc.exe executable using the cmf file
  curwd <- getwd()
  setwd(file.path(gempack_rentcalc_dir))
  system(paste0("03_rentcalc.exe -CMF 03_rentcalc.cmf"))
  Sys.sleep(5)
  setwd(curwd)
}
