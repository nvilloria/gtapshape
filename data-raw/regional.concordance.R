regional.concordance_file = "./raw_data/iso3_gtap160_concordance.csv"
require(usethis)
require(dplyr)

regional.concordance <- read.csv(regional.concordance_file, header = TRUE) %>%
    dplyr::select(-name) %>%
    dplyr::rename(reg = GTAP_160, iso3 = ISO3)


use_data(regional.concordance, overwrite = TRUE)
##use_r("regional.concordance.R")
