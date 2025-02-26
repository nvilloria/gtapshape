## ----eval = FALSE-------------------------------------------------------------
# #Load required packages
# require(dplyr)
# devtools::document('..') ## This must be replaced with
#                                 ## 'library("gtapshape")' before
#                                 ## distribution!
# #Download data from FAO
# fao_production.bulk <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder =   tempdir())

## ----eval = FALSE-------------------------------------------------------------
# ## Rename and reformat columns
# fao_production <- fao_production.bulk %>%
#   dplyr::filter(year_code > 2010) %>%
#   dplyr::rename(area_code_m49 = area_code__m49_,
#                 item_code_cpc = item_code__cpc_) %>%
#   dplyr::mutate(element_code = as.numeric(element_code),
#                 area_code_m49 = substr(area_code_m49, 2, 4),
#                 area_code_m49 = as.numeric(area_code_m49),
#                 item_code_cpc = gsub("'", "", item_code_cpc),
#                 item_code_fao = item_code) %>%
#     ## Select only production (5510), harvested area (5312), and
#     ## animal stocks (5111):
#     dplyr::filter(element_code %in% c(5510, 5111, 5312))  %>%
#     ## Add iso3 codes
#     dplyr::mutate(iso3 = tolower(countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))) %>%
#     ## The bulk download includes total for the world and various regions,
#     ## so we drop all areas where the ISO3 code is missing
#     dplyr::filter(!is.na(iso3)) %>%
#     dplyr::select(iso3, year_code, item, item_code_cpc, item_code_fao, element, unit, value) %>%
#     dplyr::rename(year=year_code)

## ----eval = FALSE-------------------------------------------------------------
# iso.crop.production.2011.2022 <- fao_production %>%
#     dplyr::filter(element == 'production')
# 
# usethis::use_data(iso.crop.production.2011.2022, overwrite = TRUE)
# ##usethis::use_r("iso.crop.production.2011.2022.R")
# 
# iso.crop.harea.2011.2022 <- fao_production %>%
#     dplyr::filter(element == 'area_harvested')
# 
# usethis::use_data(iso.crop.harea.2011.2022, overwrite = TRUE)
# ##usethis::use_r("iso.crop.harea.2011.2022.R")

## ----eval = FALSE-------------------------------------------------------------
# producer_prices.bulk <- FAOSTAT::get_faostat_bulk(code = "PP", data_folder = tempdir())
# ## Format columns, select price in USD/tonne, choose years
# producer_prices <- producer_prices.bulk %>%
#       dplyr::filter(year_code > 2010) %>%
#   dplyr::rename(area_code_m49 = area_code__m49_,
#                 item_code_cpc = item_code__cpc_) %>%
#   dplyr::mutate(element_code = as.numeric(element_code),
#                 area_code_m49 = substr(area_code_m49, 2, 4),
#                 area_code_m49 = as.numeric(area_code_m49),
#                 item_code_cpc = gsub("'", "", item_code_cpc),
#                 item_code_fao = item_code) %>%
#   dplyr::filter(element_code == 5532)
# 
# ##Make a new column showing the iso3 code
# producer_prices <- producer_prices %>%
#   dplyr::mutate(iso3 = tolower(countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c')))
# #The bulk download includes total for the world and regions, so we
# #drop all areas where the ISO3 code is missing
# producer_prices <- producer_prices %>%
#   dplyr::filter(!is.na(iso3))
# #Select necessary columns
# producer_prices <- producer_prices %>%
#   dplyr::select(iso3, year_code, item, item_code_cpc, item_code_fao, element, unit, value)
# #Save
# producer_prices <- producer_prices %>%
#     dplyr::rename(year=year_code)  %>%
#     mutate(element="price",
#            unit="USD.t") %>%
#     select(iso3, year, item_code_cpc, item_code_fao, element, unit, value)

## ----eval = FALSE-------------------------------------------------------------
# iso.crop.outputvalue.2011.2022 <- left_join(iso.crop.production.2011.2022 %>%
#                                  dplyr::rename(qty=value),
#                                  producer_prices[,c("iso3", "year", "item_code_cpc", "value")] %>%
#                                  dplyr::rename(iso3_price=value),
#                                  by = c('iso3', 'year', 'item_code_cpc')
#                                  ) %>%
#     ## Make a world price (country-average) for when the prices are missing
#     group_by(year, item_code_cpc) %>%
#     mutate(price = ifelse(is.na(iso3_price), mean(iso3_price, na.rm=TRUE), iso3_price),
#            unit="USD1000",
#            value=price*qty/1000
#            ) %>%
#     select("iso3",
#            "year",
#            "item",
#            "item_code_cpc",
#            "item_code_fao",
#            "unit",
#            "value")
# 
# usethis::use_data(iso.crop.outputvalue.2011.2022, overwrite = TRUE)
# ##usethis::use_r("iso.crop.outputvalue.2011.2022.R")

## ----eval = FALSE-------------------------------------------------------------
# ## Number of livestock animals data - just need ruminants (ctl, rmk, wol GTAP categories)
# fao_livestock_numanimals <- fao_production %>%
#     dplyr::filter(element == "stocks") %>%
#     ## Drop categories with multiple items and ones we don't need
#     dplyr::filter(item != "Cattle and Buffaloes" &
#                   item != "Sheep and Goats" &
#                   item != "Swine / pigs")
# 
# ## Define the list of animals in each RUMINANT GSC3 Category
# ctl <- c('Cattle', 'Buffalo', 'Camels', 'Other camelids', 'Sheep', 'Goats',
#          'Horses', 'Asses', 'Mules and hinnies')
# rmk <- c('Cattle', 'Buffalo', 'Camels', 'Other camelids', 'Sheep', 'Goats')
# wol <- c('Sheep', 'Goats')
# 
# ## Mapping of FAO items to GTAP sectors:
# gsc3.to.item <- as.data.frame(
#     rbind(
#         cbind(gsc3='ctl',item=ctl),
#         cbind(gsc3='rmk',item=rmk),
#         cbind(gsc3='wol',item=wol)
#     ))
# 
# ## Aggregate to GTAP sectors:
# iso.gsc3lstk.heads.2011.2022 <- full_join(fao_livestock_numanimals,
#                 gsc3.to.item,
#                 by = c("item"),
#                 relationship="many-to-many") %>%
#     group_by(iso3,year,gsc3) %>%
#     summarise(value = sum(value, na.rm = TRUE)/1000) %>%
#     mutate(unit="1000heads")
# 
# usethis::use_data(iso.gsc3lstk.heads.2011.2022, overwrite = TRUE)
# ## usethis::use_r("iso.gsc3lstk.heads.2011.2022.R")

## ----eval = FALSE-------------------------------------------------------------
# iso.rmk.and.wol.outputvalue.2011.2022 <-  fao_production %>%
#     ## Use the livestock.concordance to select animal products for
#     ## which prices are available (these do not include standing
#     ## animal heads, which are considered below):
#     filter(item_code_cpc %in%
#            livestock.concordance$item_code_cpc[livestock.concordance$gsc3 %in% c("wol","rmk")]) %>%
#     dplyr::rename(qty=value) %>%
#     left_join(.,
#               producer_prices[, c("iso3", "year", "item_code_cpc", "value")] %>%
#               dplyr::rename(iso3_price=value),
#               by = c('iso3', 'year', 'item_code_cpc')
#               ) %>%
#     ## Make a world price (country-average) for when the prices are missing
#     group_by(year, item_code_cpc) %>%
#     mutate( price = ifelse(is.na(iso3_price), mean(iso3_price, na.rm=TRUE), iso3_price),
#            unit="USD1000",
#            value=price*qty/1000) %>%
#     select("iso3", "year", "item", "item_code_cpc", "item_code_fao", "unit", "value") %>%
#     left_join(.,
#               livestock.concordance[,c("item_code_cpc", "gsc3")],
#               by = "item_code_cpc") %>%
#     group_by(iso3, year, gsc3, unit ) %>%
#     summarise(value=sum(value, na.rm=TRUE)) %>%
#     ungroup()

## ----eval = FALSE-------------------------------------------------------------
# ## For the CTL sector, value is taken over the heads of ruminants
# ## (buffalo, cattle, goats and sheep) using prices from the GTAP
# ## database:
# iso.ctl.outputvalue.2011.2022 <- fao_livestock_numanimals %>%
#     filter( item %in% c('Buffalo', 'Cattle', 'Goats', 'Sheep') ) %>%
#     group_by(iso3, year, item) %>%
#     summarise( value = sum(value, na.rm=TRUE)) %>%
#     dplyr::rename( use = item)  %>%
#     mutate(unit="heads") %>%
#     left_join(.,GTAP.ruminant.prices %>%
#              dplyr::rename(price = value),
#              by=c("iso3","use")) %>%
#     group_by(iso3, year) %>%
#     summarise(value = sum(price*value/1000),
#               gsc3='ctl',
#               unit='USD1000') %>%
#     dplyr::select(iso3, year,gsc3, unit, value)

## ----eval = FALSE-------------------------------------------------------------
# iso.gsc3lstk.outputvalue.2011.2022 <- rbind(
#     iso.rmk.and.wol.outputvalue.2011.2022,
#     iso.ctl.outputvalue.2011.2022)
# 
# usethis::use_data(iso.gsc3lstk.outputvalue.2011.2022, overwrite = TRUE)
# ## usethis::use_r("iso.gsc3lstk.outputvalue.2011.2022.R")

