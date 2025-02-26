## These livestock prices are taken from the Land Use and Land Cover (LULC) Data
## associated with the AEZ version 11c GTAP database. The construction of the
## AEZ version 11 GTAP LULC database is described in Baldos and Corong (2025) at
## https://doi.org/10.21642/RM39. To recover prices, we sum up the value of 
## livestock production (in 1,000 USD) and quantity of livestock production
## (in 1,000 head) from the AEZ v11 GTAP LULC database by GTAP region.
## Then we divide the region-level value of production by the quantity of
## production to recover the price. The .csv file loaded in contains the
## resulting prices in $ USD/head. 
require(tidyr)
lvstk_prices_4spec_file = "./raw_data/gtaplulc18/lvstk_prices_4spec.csv"

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
    ## Units are in USD/head as we divide total value of production in 1000USD
    ## by the quantity of livestock in 1000 head.
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



