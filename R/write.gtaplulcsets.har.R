#' Write HAR file containing the sets needed to read in the land use and 
#' land cover data. These need to be defined in order so they are 
#' consistent with the GTAP base database.
#'

#' @param gtap_basedatasets_file Base GTAP database set file 
#' @param subnat_bound_file SF file with subnational
#'     boundaries. Defaults to the 18 Agroecological Zones (Micah:
#'     Version AND perhaps refer to vignette?)#'
#' @param file Name of the output HAR file. Defaults to 'gtaplulcsets.har'. 
#'
write.gtaplulcsets.har <- function(gtap_basedatasets_file, subnat_bound_file="aez18", file = 'gtaplulcsets.har'){
  #Get the order of regions and commodities from the GTAP database set file
  gtap_basedatasets <- HARr::read_har(gtap_basedatasets_file)
  reg_order <- data.frame(reg = gtap_basedatasets[['reg']], reg_num = seq(1:length(gtap_basedatasets[['reg']])))
  gsc3_order <- data.frame(gsc3 = gtap_basedatasets[['comm']], gsc3_num = seq(1:length(gtap_basedatasets[['comm']])))
  
  #Define the order of the land cover types
  lcov_order <- data.frame(gsc3 = c('Forest', 'SavnGrasslnd', 'Shrubland', 'Cropland', 'Pastureland', 'Builtupland', 'Otherland'),
                           gsc3_num = c(seq(1, 7)))
  
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
  subnatbound_order <- subset(subnat_bound.sf %>% sf::st_drop_geometry(), select = c(subnat_name, subnat_num)) 
  subnatbound_order <- subnatbound_order %>%
    dplyr::mutate(subnatbound = tolower(subnat_name)) %>%
    dplyr::rename(subnatbound_num = subnat_num) %>%
    dplyr::select(-subnat_name)
  
#Create the list that is output using the HARr package
gtap.lulc.sets <- list(
  REG = reg_order$reg,
  SUBN = subnatbound_order$subnatbound,
  CRP8 = gsc3_order$gsc3[1:8],
  CRP9 = c(gsc3_order$gsc3[1:8], 'frs'),
  LCOV = lcov_order$gsc3)
attr(gtap.lulc.sets[[1]], 'description') <- "GTAP region"
attr(gtap.lulc.sets[[2]], 'description') <- "Subnational boundary"
attr(gtap.lulc.sets[[3]], 'description') <- "8 GTAP crop categories"
attr(gtap.lulc.sets[[4]], 'description') <- "8 GTAP crop categories and forest resources 'frs'"
attr(gtap.lulc.sets[[5]], 'description') <- "Land cover categories"

#Write the HAR file
write_har(gtap.lulc.sets, filename = file)
}