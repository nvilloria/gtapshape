#' Orders gsc3.by.iso output to be consistent with set order from GTAP
#' base data
#'
#' @param gsc3.by.iso List containing shared out land use and land cover data
#' @param gtap_basedatasets_file Base GTAP database set file
#' @param subnat_bound_file SF file with subnational
#'     boundaries. Defaults to the 18 Agroecological Zones used in version 11 
#'     of the GTAP-AEZ LULC database by \href{https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=7407}{Baldoz and Corong (2025)}.
#'     The `vignette("create.18.aez.shapefile", package = "gtapshape")` contains
#'     the code which creates the 18 AEZ shapefile. 
#'
order.gsc3.by.iso <- function(gsc3.by.iso, gtap_basedatasets_file, subnat_bound_file="aez18"){
  #Get the order of regions and commodities from the GTAP database set file
  gtap_basedatasets <- HARr::read_har(gtap_basedatasets_file)
  reg_order <- data.frame(reg = gtap_basedatasets[['reg']], reg_num = seq(1:length(gtap_basedatasets[['reg']])))
  gsc3_order <- data.frame(gsc3 = gtap_basedatasets[['comm']], gsc3_num = seq(1:length(gtap_basedatasets[['comm']])))

  #Add the order of the land cover types
  lcov_order <- data.frame(gsc3 = c('Forest', 'SavnGrasslnd', 'Shrubland', 'Cropland', 'Pastureland', 'Builtupland', 'Otherland'),
                           gsc3_num = c(seq(1, 7)))
  gsc3_order <- rbind(gsc3_order, lcov_order)

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

  #Go through the list of headers in the gsc3.by.iso object
    #Store header names
    hnames <- names(gsc3.by.iso)
  tohar <- lapply(seq_along(gsc3.by.iso), function (i) {
      x <- gsc3.by.iso[[i]]
      #Assign region levels
      x$reg <- factor(x$reg, 
                      levels = reg_order$reg,
                      labels = reg_order$reg)
      #Assign subnational bound levels
      x$subnatbound <- factor(x$subnatbound, 
                              levels = subnatbound_order$subnatbound,
                              labels = subnatbound_order$subnatbound)
      #Now assign the commodity and other levels 
      x$gsc3 <- factor(x$gsc3, 
                       levels = subset(gsc3_order, gsc3 %in% unique(x$gsc3))$gsc3,
                       labels = subset(gsc3_order, gsc3 %in% unique(x$gsc3))$gsc3)
      return(x)
    }
  )
  names(tohar) <- hnames
  tohar
}
