#' Create aggregation input file
#'
#' This function creates an input file for aggregating the data. It modifies the
#' end of the BIOMEmod_base.txt file to add in the sub-national boundary sets.
#'
#' @param tmp_dir Location of the tmp dir created using gtap_setup function. The default is the current working directory set by getwd()
#' @param gempack_rentcalc_dir Location of the directory where inputs and outputs
#' @param input_aggr_file Aggregation file (.txt) with mapping for regions and
#'    commodity groups.
#' @param input_aggr_file_dir Location of the directory containing the input file
#'    used to aggregate LULC databse to desired regions, commodity groups, etc.
#' @return Outputs BIOMEmod.txt aggregation file.
#' @export
input_file <- function(tmp_dir=getwd(), gempack_rentcalc_dir, input_aggr_file, input_aggr_file_dir) {
  #Make a copy of the BIOMEmod_base.txt file
  BIOMEmod_base <- file.path(input_aggr_file_dir, input_aggr_file)
  output_data_folder <- file.path(gempack_rentcalc_dir, 'out/')
  file.copy(from=BIOMEmod_base, to=output_data_folder,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)
  file.rename(file.path(gempack_rentcalc_dir, 'out/', input_aggr_file),
              file.path(gempack_rentcalc_dir, 'out/BIOMEmod.txt'))

  #Specify that we're modifying the BIOMEmod.txt file
  filename <- file.path(gempack_rentcalc_dir, 'out/BIOMEmod.txt')
  #Make a new line object
  eol <- c("\n")
  #Add a blank line before we start editing
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #Load in BIOME sets
  set_BIO_list <- readRDS(file.path(tmp_dir, 'tmp/sets/set_BIO.rds'))

  #AEND Header
  #Set has 4 plus number of elements in set_BIO_list
  set_AEND_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  hmd <- c(paste0(length(set_AEND_list), ' Strings Length 12 Header "H6" LongName "AEND Aggregate endowment" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_AEND_list)) {
    write.table(set_AEND_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #ASEN header
  #Set has the sluggish endowments, meaning the land endowments
  set_ASEN_list <- c(set_BIO_list)
  hmd <- c(paste0(length(set_ASEN_list), ' Strings Length 12 Header "ASEN" LongName "ASEN Aggregate sluggish endowment" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_ASEN_list)) {
    write.table(set_ASEN_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #DEND header
  #Set has the land endowments plus 7 others
  set_DEND_list <- c(set_BIO_list, "SkLab", "UnSkLab", "UnSkLab", "SkLab", "UnSkLab", "Capital", "NatRes")
  hmd <- c(paste0(length(set_DEND_list), ' Strings Length 12 Header "DEND" LongName "Endowment aggregation mapping" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_DEND_list)) {
    write.table(set_DEND_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #MSEN header
  #Set has the sluggish endowments, meaning the land endowments
  set_MSEN_list <- c(set_BIO_list)
  hmd <- c(paste0(length(set_MSEN_list), ' Strings Length 12 Header "MSEN" LongName "AMEN Aggregate sluggish endowment mapping" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_MSEN_list)) {
    write.table(set_MSEN_list[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #ETRE Header
  #Set has 4 plus number of elements in set_BIO_list
  set_ETRE_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  set_ETRE_vals <- c(rep(-1, length(set_BIO_list)), -2, -2, -2, 0)
  hmd <- c(paste0(length(set_ETRE_list), ' real header "ETRE" LongName "Value of ETRAE for endowments" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_ETRE_list)) {
    write.table(set_ETRE_vals[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #ESMS Header
  #Set has 2 plus number of elements in set_BIO_list
  set_ESMS_list <- c("SkLab", "UnSkLab", set_BIO_list)
  set_ESMS_vals <- c(rep(0.5, length(set_ESMS_list)))
  hmd <- c(paste0(length(set_ESMS_list), ' real header "ESMS" LongName "Value of ESMS for mobile and sluggish endowments (except capital)" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_ESMS_vals)) {
    write.table(set_ESMS_vals[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #SLUG Header
  #Set has 4 plus number of elements in set_BIO_list
  set_SLUG_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  set_SLUG_vals <- c(rep(1, length(set_BIO_list)), 0, 0, 0, 1)
  hmd <- c(paste0(length(set_SLUG_list), ' integer header "SLUG" LongName "Sluggish (1) or mobile (0) endowments" ;'))
  write.table(hmd, file=filename, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_SLUG_vals)) {
    write.table(set_SLUG_vals[[i]], file=filename, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
} #end function
