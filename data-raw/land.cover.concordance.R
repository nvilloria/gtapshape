\land.cover.concordance <- data.frame(
    gsc3 = c("Forest", "SavnGrasslnd", "Shrubland", "Cropland", "Pastureland", "Builtupland", "Otherland"),
    use = c("forest", "grassland", "shrubland",  "cropland", "pasture", "urban", "other")
)

usethis::use_data(land.cover.concordance, overwrite = TRUE)
## usethis::use_r("land.cover.concordance.R")
