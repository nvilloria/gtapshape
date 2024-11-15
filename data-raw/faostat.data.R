require(dplyr)
## FAOSTAT data 2011-2022
fao_production.bulk <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder =   tempdir())
producer_prices.bulk <- FAOSTAT::get_faostat_bulk(code = "PP", data_folder = tempdir())
fao_val_production.bulk <- FAOSTAT::get_faostat_bulk(code = "QV", data_folder = tempdir() )


## CROP PRODUCTION

## rename and reformat columns
fao_production <- fao_production.bulk %>%
  dplyr::filter(year_code > 2010) %>%
  dplyr::rename(area_code_m49 = area_code__m49_,
                item_code_cpc = item_code__cpc_) %>%
  dplyr::mutate(element_code = as.numeric(element_code),
                area_code_m49 = substr(area_code_m49, 2, 4),
                area_code_m49 = as.numeric(area_code_m49),
                item_code_cpc = gsub("'", "", item_code_cpc),
                item_code_fao = item_code)
  #Select only production and harvested area data
prod_harvarea <- fao_production %>%
  dplyr::filter(element_code == 5510 | element_code == 5312)  %>%
  dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
# #Show areas missing ISO3 codes
# missing_iso <- prod_harvarea %>%
#   dplyr::filter(is.na(ISO3)) %>%
#   dplyr::select(area, area_code_m49, area_code) %>%
#   unique()
#The bulk download includes total for the world and various regions, so we
#drop all areas where the ISO3 code is missing
prod_harvarea <- prod_harvarea %>%
   dplyr::filter(!is.na(ISO3))
 #Select necessary columns
prod_harvarea <- prod_harvarea %>%
    dplyr::select(ISO3, year_code, item, item_code_cpc, item_code_fao, element, unit, value) %>%
    dplyr::rename(iso3 = ISO3, year=year_code)

iso.crop.production.2011.2022 <- prod_harvarea %>%
    dplyr::filter(element == 'production')

head(iso.crop.production.2011.2022)
## supplementary.data.path <- system.file("faostat", package = "gtapshape")

## save(prod_harvarea,
##      file= file.path(supplementary.data.path, "prod_harvarea.rda"),
##      compress = "xz",
##      compression_level = 9)

usethis::use_data(iso.crop.production.2011.2022, overwrite = TRUE)
##usethis::use_r("iso.crop.production.2011.2022.R")

## LIVESTOCK

## Number of livestock animals data - just need ruminants (ctl, rmk, wol GTAP categories)
livestock_numanimals <- fao_production %>%
  dplyr::filter(element_code == 5111)
## Make a new column showing the iso3 code
livestock_numanimals <- livestock_numanimals %>%
  dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
## The bulk download includes total for the world and various regions, so we
#drop all areas where the ISO3 code is missing
livestock_numanimals <- livestock_numanimals %>%
  dplyr::filter(!is.na(ISO3))
## Select necessary columns
livestock_numanimals <- livestock_numanimals %>%
    dplyr::select(ISO3, year_code, item, item_code_cpc, item_code_fao, element, unit, value)

## Drop categories with multiple items and ones we don't need
fao_livestock_numanimals <- livestock_numanimals %>%
  dplyr::filter(item != "Cattle and Buffaloes" &
                  item != "Sheep and Goats" &
                  item != "Swine / pigs")
#Define the list of animals in each RUMINANT Category
ctl_list <- c('Cattle', 'Buffalo', 'Camels', 'Other camelids', 'Sheep', 'Goats', 'Horses', 'Asses', 'Mules and hinnies')
rmk_list <- c('Cattle', 'Buffalo', 'Camels', 'Other camelids', 'Sheep', 'Goats')
wol_list <- c('Sheep', 'Goats')
#Add up animals in each category by region and year
ctl_fao_livestock_numanimals <- fao_livestock_numanimals %>%
  dplyr::filter(item %in% ctl_list) %>%
  dplyr::group_by(ISO3,year_code) %>%
  dplyr::summarise(value = sum(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GSC3 = 'ctl')
rmk_fao_livestock_numanimals <- fao_livestock_numanimals %>%
  dplyr::filter(item %in% rmk_list) %>%
  dplyr::group_by(ISO3,year_code) %>%
  dplyr::summarise(value = sum(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GSC3 = 'rmk')
wol_fao_livestock_numanimals <- fao_livestock_numanimals %>%
  dplyr::filter(item %in% wol_list) %>%
  dplyr::group_by(ISO3,year_code) %>%
  dplyr::summarise(value = sum(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(GSC3 = 'wol')
#Combine the three into one dataframe
fao_livestock_numanimals <- rbind(ctl_fao_livestock_numanimals, rmk_fao_livestock_numanimals, wol_fao_livestock_numanimals)
#Divide number of animals by 1000
fao_livestock_numanimals <- fao_livestock_numanimals %>%
  dplyr::mutate(head1000 = value/1000) %>%
  dplyr::select(-value)

iso.animal.production.2011.2022 <- fao_livestock_numanimals %>%
    dplyr::rename("iso3"="ISO3",
                  "year"="year_code",
                  "gsc3"="GSC3",
                  "value"="head1000") %>%
    mutate(unit="1000heads")  %>%
    dplyr::select(iso3, year, gsc3, unit, value)

head(iso.animal.production.2011.2022)

usethis::use_data(iso.animal.production.2011.2022, overwrite = TRUE)
##usethis::use_r("iso.animal.production.2011.2022.R")

