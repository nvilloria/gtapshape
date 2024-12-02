## Micah, add a brief description of these prices including where they
## are coming from, it seems to me they are coming from a GTAP
## database, i am not sure which version/year. Please clarify.
require(tidyr)
lvstk_prices_4spec_file = "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/gtaplulc18/lvstk_prices_4spec.csv"

GTAP.ruminant.prices <- read.csv(lvstk_prices_4spec_file, header = T) %>%
    dplyr::rename("reg" = "QLIVE_18",
                  "Buffalo" = "X1.BUFFALOES",
                  "Cattle" = "X2.CATTLE",
                  "Goats" = "X3.GOATS",
                  "Sheep" = "X4.SHEEP") %>%
    mutate(reg=substr( reg, nchar(reg) - 2, nchar(reg))) %>%
    pivot_longer(cols = c("Buffalo", "Cattle", "Goats","Sheep" ),
                 values_to='value',
                 names_to='use') %>%
    ## Micah, check units here please---is this per head?:
    mutate(unit='USD.head',
           value=as.numeric(value)) %>%
    ## Use an average price (over all countries) when the prices are
    ## missing:
    group_by(use) %>%
    mutate( value = ifelse( is.na(value),
                           mean(value[use==use],
                                na.rm = TRUE), value )
           ) %>%
    ## Map the 160 GTAP regions to 251 iso codes for consistency
    ## across provided datasets (all at the iso3 level):
    right_join(.,regional.concordance,
               by=c('reg'),
               relationship='many-to-many') %>%
    select( iso3, use, unit, value)

usethis::use_data(GTAP.ruminant.prices, overwrite = TRUE)
##usethis::use_r("GTAP.ruminant.prices.R")



