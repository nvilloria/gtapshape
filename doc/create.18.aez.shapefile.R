## ----load, include = FALSE----------------------------------------------------
#Load in require packages 
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  terra,
  sf,
  dplyr)

## ----eval = FALSE-------------------------------------------------------------
# #Load in length of growing season data
#     lgd <- terra::rast("./raw_data/Total number of growing period days_1981 to 2010/lgd_CRUTS32_Hist_8110.tif")
#     #Check RCS - WGS1984, EPSG 4326
#     crs(lgd)
#     #Check resolution - It's 0.083 degrees which is equivalent to 5 arc minutes
#     res(lgd)
#     #Check unique values
#     unique(lgd)
#     plot(lgd)

## ----eval = FALSE-------------------------------------------------------------
#     #Recode so there are 36 categories defined by 10 degree intervals
#       lgd_int_low <- c(-1, seq(from = 9, to = 349, by = 10))
#       lgd_int_high <- c(seq(from = 9, to = 349, by = 10), 366)
#       lgd_int_values <- c(seq(from = 1, to = 36, by = 1))
#       lgd_int_df <- data.frame(lgd_int_low, lgd_int_high, lgd_int_values)
#       lgd_mat <- data.matrix(lgd_int_df)
#       lgd_36cat <- classify(lgd, lgd_mat)
#       unique(lgd_36cat)
#       #Plot to check
#       plot(lgd_36cat)
#       #Check projection - WGS1984 - EPSG 4326
#       crs(lgd_36cat)
#       #Check resolution - 0.83 degrees = 5 arc minutes
#       res(lgd_36cat)
# 
#       #Clean up the environment
#       rm(lgd, lgd_int_df, lgd_int_low, lgd_int_high, lgd_int_values)
#       gc()

## ----eval = FALSE-------------------------------------------------------------
#   #Thermal Zones
#     mc2 <- terra::rast("./raw_data/Thermal zones/mc2_CRUTS32_Hist_8110.tif")
#     #Make a key for the raster
#     mc2_rast_values <- c(seq(from = 1, to = 13, by = 1))
#     mc2_value_labels <- c("Tropics, warm", "Tropics, cool/cold/very cold",
#                           "Subtropics, warm/mod. cool", "Subtropics, cool",
#                           "Subtropics, cold", "Subtropics, very cold",
#                           "Temperate, cool", "Temperate, cold",
#                           "Temperate, very cold", "Boreal, cold",
#                           "Boreal, very cold", "Artic", "NA")
#     mc2_label_key <- data.frame(mc2_rast_values, mc2_value_labels)
# 
#     #Reclassify Thermal Climates to only 3 categories
#     mc2_3cat <- reclassify(mc2, c(0, 1, 1,
#                                   1, 7, 2,
#                                   7, 12, 3))
#     mc2_3cat_values <- c(1,2,3)
#     mc2_3cat_labels <- c("Tropical", "Temperate", "Boreal")
#     mc2_3cat_label_key <- data.frame(mc2_3cat_values, mc2_3cat_labels)
#     mc2_3cat <- subs(mc2_3cat, mc2_3cat_label_key, by = 1, which = 2)
#     #Plot to double check
#     plot(mc2_3cat)
#     #Check labels
#     levels(mc2_3cat)
#     #Check projection - WGS1984 - EPSG 9122
#     crs(mc2_3cat)
#     #Check resolution - 0.83 degrees = 5 arc minutes
#     res(mc2_3cat)
# 
#     #Clean up environment
#     rm(mc2, mc2_rast_values, mc2_value_labels, mc2_label_key, mc2_3cat_values, mc2_3cat_labels, mc2_3cat_label_key)
#     gc()

## ----setup,  eval = FALSE-----------------------------------------------------
# 
# #Multiply the values for mc2_3cat by 100
#   aez_108 <- mc2_3cat * 100 + lgd_36cat
#   aez_cur_values <- c(unique(aez_108))
#   aez_final_values <- c(seq(1, 108, by = 1))
#   aez_key <- data.frame(aez_cur_values, aez_final_values)
#   aez_mat <- data.matrix(aez_key)
#   aez_108 <- reclassify(aez_108, aez_mat)
#   unique(aez_108)
#   plot(aez_108)
#   aez_108
#   names(aez_108) <- c("aez_108")
#   writeRaster(aez_108, filename = file.path(parent_dir, "Processed data/aez_108.tif"), overwrite = TRUE)
# 
# #Make another raster with the 18 aez categories instead
#   aez_18_values <- c(rep(1:18, each = 6))
#   aez_18_key <- data.frame(aez_final_values, aez_18_values)
#   aez_18_mat <- data.matrix(aez_18_key)
#   aez_18 <- reclassify(aez_108, aez_18_mat)
#   unique(aez_18)
#   names(aez_18) <- c("aez_18")
#   plot(aez_18)

## ----eval = FALSE-------------------------------------------------------------
# #Convert raster to simple features (sf) object
#   aez18_sf <- sf::st_as_sf(aez_18)
# #Group geometries by AEZ and combine them to get one geometry per AEZ
#   aez18_sf <- aez18_sf %>%
#     dplyr::rename(aez = aez_18.tif)
#   aez18_sf_out <- aez18_sf %>%
#     group_by(aez) %>%
#     summarise(geometry = st_union(geometry))
# #Make columns indicating the AEZ number
#   aez18_sf_out <- aez18_sf_out %>%
#     dplyr::mutate(subnat_num = aez,
#                   subnat_name = paste0("AEZ", subnat_num)) %>%
#     dplyr::select(subnat_name, subnat_num, geometry)
# #Save to the gtapshape package data
#   usethis::use_data(aez18, overwrite = TRUE)

