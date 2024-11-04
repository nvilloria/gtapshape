## FAOSTAT data 2011-2022
fao_production.bulk <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder =   tempdir())
producer_prices.bulk <- FAOSTAT::get_faostat_bulk(code = "PP", data_folder = tempdir())
fao_val_production.bulk <- FAOSTAT::get_faostat_bulk(code = "QV", data_folder = tempdir() )
land_cover.bulk <- FAOSTAT::get_faostat_bulk(code = "RL", data_folder = tempdir())


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

## LAND COVER
  #### Land cover ####
  # Bulk download land cover data - 1000 hectares in arable land (item code 6621),
  #permanent crops (6650), and permanent meadows (6655), and total land area (6601)
  # item_list <- c("Arable land","Permanent crops", "Permanent meadows and pastures")
  item_code_list <- c(6621, 6650, 6655, 6601)
  #Format columns, select the three item codes, choose years
  land_cover <- land_cover.bulk %>%
#    dplyr::filter(year_code == gtap_ref_year) %>%
    dplyr::rename(area_code_m49 = area_code__m49_) %>%
    dplyr::mutate(element_code = as.numeric(element_code),
                  area_code_m49 = substr(area_code_m49, 2, 4),
                  area_code_m49 = as.numeric(area_code_m49),
                  item_code_fao = item_code) %>%
    dplyr::filter(element_code == 5110) %>%
    dplyr::filter(item_code %in% item_code_list)
  #Make a new column showing the iso3 code
  land_cover <- land_cover %>%
    dplyr::mutate(ISO3 = countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c'))
  #The bulk download includes total for the world and regions, so we
  #drop all areas where the ISO3 code is missing
  land_cover <- land_cover %>%
    dplyr::filter(!is.na(ISO3))
  #Pivot wider so we can make columns for cropland and pastureland
  land_cover_wide <- land_cover %>%
    dplyr::select(ISO3, year_code, item, value) %>%
    tidyr::pivot_wider(id_cols = c(ISO3, year_code),
                       names_from = item,
                       values_from =  value)
  #Rename categories, replace NA values with zeroes
  land_cover_wide <- land_cover_wide %>%
    dplyr::rename(arable = `Arable land`,
                  permanent_crop = `Permanent crops`,
                  pasture_ha = `Permanent meadows and pastures`,
                  land_area_ha = `Land area`) %>%
    dplyr::mutate(arable = ifelse(is.na(arable), 0, arable),
                  permanent_crop = ifelse(is.na(permanent_crop), 0, permanent_crop),
                  pasture_ha = ifelse(is.na(pasture_ha), 0, pasture_ha),
                  land_area_ha = ifelse(is.na(land_area_ha), 0, land_area_ha))
  #Now add up the categories to get the values for cropland and pasture.
  #Values are in 1000 ha, convert to ha
  land_cover_wide <- land_cover_wide %>%
    dplyr::mutate(cropland_ha = 1000*(arable + permanent_crop),
                  pasture_ha = 1000*pasture_ha,
                  land_area_ha = 1000*land_area_ha) %>%
    dplyr::select(ISO3, year_code, cropland_ha, pasture_ha, land_area_ha)
head(land_cover_wide)
## saveRDS(land_cover_wide, file.path(workdir_dir, 'workdir/gtap_ref_year/fao_land_cover.rds'))
