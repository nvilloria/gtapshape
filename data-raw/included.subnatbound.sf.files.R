biomes14 <- readRDS("./raw_data/TerrestrialEcos/wwf14_subnatbound_sf.rds")
aez18 <-    readRDS("./raw_data/aez18_subnatbound_sf.rds")


usethis::use_data(biomes14, overwrite = TRUE)
## usethis::use_r("biomes14.R")


usethis::use_data(aez18, overwrite = TRUE)
## usethis::use_r("aez18.R")
