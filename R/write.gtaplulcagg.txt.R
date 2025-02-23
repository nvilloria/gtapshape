#' Create aggregation textfile based on subnatbound choice
#'
#' @param subnat_bound_file SF file with subnational
#'     boundaries. Defaults to the 18 Agroecological Zones (Micah:
#'     Version AND perhaps refer to vignette?)#'
#' @param base_aggr_file Text file containing how commodities will are mapped to
#'    larger categories, how regions will be aggregated, etc. NOTE - This file
#'    will be overwritten when this function runs.
#' @param file Name of the output aggregation file. Defaults to 'gtaplulcagg.txt'.
#'
write.gtaplulcagg.txt <- function(subnat_bound_file="aez18",
                                  base_aggr_file =
                                      system.file("agg_templates",
                                                  "gtapv11c_aggr_base_file.txt",
                                                  package = "gtapshape") ,
                                  textfilename = 'gtaplulcagg.txt'){

  ## Copy the GTAP database aggregation template to the temporary
    ## folder for modifications: I AM HERE!!
    tempaggfile <- file.path( tempdir(), "agg.template.tmp.txt")

    file.copy(
        from = base_aggr_file,
        to = tempaggfile
    )
  #Make a new line object
  eol <- c("\n")
  #Add a blank line before we start editing
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #Load in BIOME sets
  #Get the order of the subnational boundaries from the chosen subnatbound file
  if (subnat_bound_file %in% c('aez18', 'biomes14')) {
    # Load the lazy-loaded data
    data(list = subnat_bound_file, package = "gtapshape")
    subnat_bound.sf <- get(subnat_bound_file)
  } else if (file.exists(subnat_bound_file)) {
    # Load external file
    subnat_bound.sf <- readRDS(subnat_bound_file)
  } else {
    stop("Invalid subnat_bound_file. Use 'aez18', 'biomes14', or a valid file path.")
  }
  ## subnatbound_order <- subset(subnat_bound.sf %>% sf::st_drop_geometry(), select = c(subnat_name, subnat_num))
  ## subnatbound_order <- subnatbound_order %>%
  ##   dplyr::mutate(subnatbound = tolower(subnat_name)) %>%
  ##   dplyr::rename(subnatbound_num = subnat_num) %>%
  ##   dplyr::select(-subnat_name) %>%
  ##   dplyr::arrange(subnatbound_num)
    ## set_BIO_list <- subnatbound_order$subnatbound

    ## Changed all this to base R:
    subnatbound_order <- subnat_bound.sf[, c("subnat_name", "subnat_num")]
    subnatbound_order <- as.data.frame(subnatbound_order)
    subnatbound_order$subnatbound <- tolower(subnatbound_order$subnat_name)
    names(subnatbound_order)[names(subnatbound_order) == "subnat_num"] <- "subnatbound_num"
    subnatbound_order <- subnatbound_order[, c("subnatbound", "subnatbound_num")]
    subnatbound_order <- subnatbound_order[order(subnatbound_order$subnatbound_num), ]
    set_BIO_list <- subnatbound_order$subnatbound

  #AEND Header
  #Set has 4 plus number of elements in set_BIO_list
  set_AEND_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  hmd <- c(paste0(length(set_AEND_list), ' Strings Length 12 Header "H6" LongName "AEND Aggregate endowment" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_AEND_list)) {
    write.table(set_AEND_list[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #ASEN header
  #Set has the sluggish endowments, meaning the land endowments
  set_ASEN_list <- c(set_BIO_list)
  hmd <- c(paste0(length(set_ASEN_list), ' Strings Length 12 Header "ASEN" LongName "ASEN Aggregate sluggish endowment" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_ASEN_list)) {
    write.table(set_ASEN_list[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #DEND header
  #Set has the land endowments plus 7 others
  set_DEND_list <- c(set_BIO_list, "SkLab", "UnSkLab", "UnSkLab", "SkLab", "UnSkLab", "Capital", "NatRes")
  hmd <- c(paste0(length(set_DEND_list), ' Strings Length 12 Header "DEND" LongName "Endowment aggregation mapping" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_DEND_list)) {
    write.table(set_DEND_list[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #MSEN header
  #Set has the sluggish endowments, meaning the land endowments
  set_MSEN_list <- c(set_BIO_list)
  hmd <- c(paste0(length(set_MSEN_list), ' Strings Length 12 Header "MSEN" LongName "AMEN Aggregate sluggish endowment mapping" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_MSEN_list)) {
    write.table(set_MSEN_list[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #ETRE Header
  #Set has 4 plus number of elements in set_BIO_list
  set_ETRE_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  set_ETRE_vals <- c(rep(-1, length(set_BIO_list)), -2, -2, -2, 0)
  hmd <- c(paste0(length(set_ETRE_list), ' real header "ETRE" LongName "Value of ETRAE for endowments" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_ETRE_list)) {
    write.table(set_ETRE_vals[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #ESMS Header
  #Set has 2 plus number of elements in set_BIO_list
  set_ESMS_list <- c("SkLab", "UnSkLab", set_BIO_list)
  set_ESMS_vals <- c(rep(0.5, length(set_ESMS_list)))
  hmd <- c(paste0(length(set_ESMS_list), ' real header "ESMS" LongName "Value of ESMS for mobile and sluggish endowments (except capital)" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_ESMS_vals)) {
    write.table(set_ESMS_vals[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }
  write.table(eol, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)

  #SLUG Header
  #Set has 4 plus number of elements in set_BIO_list
  set_SLUG_list <- c(set_BIO_list, "UnSkLab", "SkLab", "Capital", "NatRes")
  set_SLUG_vals <- c(rep(1, length(set_BIO_list)), 0, 0, 0, 1)
  hmd <- c(paste0(length(set_SLUG_list), ' integer header "SLUG" LongName "Sluggish (1) or mobile (0) endowments" ;'))
  write.table(hmd, file=tempaggfile, row.names=FALSE, col.names=FALSE, quote = FALSE, append = TRUE)
  for (i in 1:length(set_SLUG_vals)) {
    write.table(set_SLUG_vals[[i]], file=tempaggfile, quote = FALSE, row.names=FALSE, col.names=FALSE, append = TRUE)
  }

  #Rename the file
  file.rename(tempaggfile,
              textfilename)
}
