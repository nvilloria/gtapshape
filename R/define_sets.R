#' Define sets
#'
#' This function defines the sets used to generate agricultural
#' production data and split out land rents at a subnational level.There
#' are three sets used by the GTAP model which will be defined by the choices
#' made in this script:
#'    1. The region (REG) set elements will be defined.
#'    2. A set of subnational boundaries (BIOMES) that sub-divide regions need
#'       to be named and defined. For example, these biophysical boundaries may
#'       be the 14 terrestrial BIOMES defined by Olson & Dinerstein (1998) for
#'       the W.W.F. / Global 200 or they could be the 18 Agro-ecological zones
#'       used in the GTAP-AEZ database.
#'    3. The set of endowment commodities which includes countries' land endowments
#'       by biophysical area, skilled labor, unskilled labor, capital, and
#'       natural resources. The naming convention for this set is to use the
#'       3 character name assigned to the set of biophysical boundaries followed
#'       by "_ENDW".
#'
#' The function has two arguments:
#'     1. The file name of a .rds file containing the set of regions (with names
#'      in column REG) and their numbers (in column REG_NUM).
#'     2. The file name of an .rds file containing a simple features (sf) object
#'      defining the sub-national boundaries and their names. The simple features
#'      object containing the subnational boundaries needs to have a column named
#'       "subnat_name" containing the names of the subnational areas,
#'       another named "subnat_num" containing unique numbers for each,
#'        and a "geometry" column that can be read by the 'sf' package.
#'
#' @param reg_set_file Data frame containing the set REG of GTAP regions.
#' @param subnat_bound_file Simple feature object containing the subnational boundaries (NV: Add a link to the function that creates this----would it be a good idea to have few of this included as defaults?)
#' @param workdir_dir Location of the workdir dir create using gtap_setup function. The default is the current working directory set by getwd()
#' @return Sets for the final set of endowments (BIO_ENDW), 9 Crop commodity,
#'         aggregates (CRP9) and 7 land cover types (LCOV) (NV: Add more details of what the ouput is and where the output is at. E.g., five text files with the sets ... and two Rds files with the sets ... in workdir/sets/ Also add links to the functions that use these sets to facilitate followint the logic of the building process)
#'
#'
#' @export
define_sets <- function(reg_set_file, subnat_bound_file, workdir_dir=getwd()) {

  #### Specify Regions ####
  #Specify the set of GTAP regions (countries)
  set_reg <- readRDS(reg_set_file)
  #Make a list to save as GEMPACK text file
  set_REG_list <- set_reg$REG
  #Save this set list for formatting data later
  saveRDS(set_REG_list, file = file.path(workdir_dir, 'workdir/sets/set_REG.rds'))
  #Output the set as a GEMPACK txt file
  hmd <- c(paste0(length(set_REG_list), ' Strings Length 3 Header "REG" LongName "Set REG regions" ;'))
  filename <- paste0(workdir_dir, '/workdir/sets/reg_set.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(set_REG_list)) {
    write.table(set_REG_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### Specify BIOMES ####
  #Specify the set of subnational boundaries (such as AEZs or BIOMES) and the
  #by providing the shapefile used to delineate them.
  #NOTE - This set should have the same naming convention as the
  #shapefile containing the boundaries. For example, if the set will
  #be AEZ1, AEZ2, etc., then the corresponding rows in the sf object
  #containing the shapes should be AEZ1, AEZ2, etc.

  #Load in shapefile
  subnat_bound <- readRDS(subnat_bound_file)
  #Make a key linking subnational area names and numbers
  subnat_bound_key <- subnat_bound %>%
    sf::st_drop_geometry() %>%
    dplyr::select(subnat_name, subnat_num) %>%
    unique() %>%
    dplyr::mutate(subnat_name = as.character(subnat_name),
                  subnat_num = as.numeric(subnat_num)) %>%
    dplyr::arrange(subnat_num)
  #Make a Rest of World - ROW category for any area not covered by the shapes
  subnat_bound_key <- rbind(subnat_bound_key, data.frame(subnat_name = "ROW", subnat_num = (length(subnat_bound$subnat_name) + 1)))
  saveRDS(subnat_bound_key, file = file.path(workdir_dir, 'workdir/subnat_name_key.rds'))
  #Make a new column with the set names
  subnat_bound_key <- subnat_bound_key %>%
    dplyr::mutate(BIO = subnat_name,
                  BIO = toupper(BIO))
  #Store the names of the biomes to save as the set items
  set_BIO_list <- subnat_bound_key$BIO
  #Save
  saveRDS(set_BIO_list, file = file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
  #Output the set as a GEMPACK txt file
  hmd <- c(paste0(length(set_BIO_list), ' Strings Length 10 Header "BIO" LongName "Set BIO subnational areas" ;'))
  filename <- paste0(workdir_dir, '/workdir/sets/biome_set.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(set_BIO_list)) {
    write.table(set_BIO_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #### Save other sets ####
  # Sets are the final set of endowments (BIO_ENDW), 9 Crop commodity
  # aggregates (CRP9) and 7 land cover types (LCOV)
  #BIO_ENDW set has 4 plus number of elements in set_BIO_list
  set_bioendw_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  hmd <- c(paste0(length(set_bioendw_list), ' Strings Length 7 Header "ENDW" LongName "Set of endowments including land in biomes" ;'))
  filename <- paste0(workdir_dir, '/workdir/sets/biome_endw_set.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(set_bioendw_list)) {
    write.table(set_bioendw_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #Create the set of 9 crops, CRP9, as a GEMPACK txt file
  set_CRP9_list <- c('pdr', 'wht', 'gro', 'v_f', 'osd', 'c_b', 'pfb', 'ocr', 'frs')
  hmd <- c(paste0(length(set_CRP9_list), ' Strings Length 3 Header "CRP9" LongName "Set of 9 crop aggregates" ;'))
  filename <- paste0(workdir_dir, '/workdir/sets/crp9_set.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(set_CRP9_list)) {
    write.table(set_CRP9_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  #Create the set of 7 land covers, LCOV, as a GEMPACK txt file
  set_LCOV_list <- c('Forest', 'SavnGrasslnd', 'Shrubland', 'Cropland', 'Pastureland', 'Builtupland', 'Otherland')
  hmd <- c(paste0(length(set_LCOV_list), ' Strings Length 13 Header "LCOV" LongName "Set of 7 land covers" ;'))
  filename <- paste0(workdir_dir, '/workdir/sets/lcov_set.txt')
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE)
  for (i in 1:length(set_LCOV_list)) {
    write.table(set_LCOV_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
}
