regional.concordance_file = "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/iso3_gtap160_concordance.csv"
require(usethis)
require(dplyr)

regional.concordance <- read.csv(regional.concordance_file, header = TRUE) %>%
    dplyr::select(-name) %>%
    dplyr::rename(reg = GTAP_160, iso3 = ISO3)

regional.concordance$iso3 <- toupper(regional.concordance$iso3)

regional.concordance$reg <- toupper(regional.concordance$reg)

use_data(regional.concordance, overwrite = TRUE)
##use_r("regional.concordance.R")
