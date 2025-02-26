## ----eval=FALSE---------------------------------------------------------------
# require(dplyr)
# require(tidyr)
# ## Bulk download land cover data - 1000 hectares in arable land (item
# ## code 6621), permanent crops (6650), and permanent meadows (6655),
# ## and total land area (6601)
# land_cover.bulk <- FAOSTAT::get_faostat_bulk(code = "RL", data_folder = tempdir())
# item_list <- c("Arable land","Permanent crops", "Permanent meadows and pastures")
# item_code_list <- c(6621, 6650, 6655, 6601)
# 
# ## Format columns, select the three item codes, choose years
# land_cover <- land_cover.bulk %>%
#       dplyr::filter(year_code > 2010) %>%
# #    dplyr::filter(year_code == gtap_ref_year) %>%
#     dplyr::rename(area_code_m49 = area_code__m49_) %>%
#     dplyr::mutate(element_code = as.numeric(element_code),
#                   area_code_m49 = substr(area_code_m49, 2, 4),
#                   area_code_m49 = as.numeric(area_code_m49),
#                   item_code_fao = item_code) %>%
#     dplyr::filter(element_code == 5110) %>%
#     dplyr::filter(item_code %in% item_code_list)
#   #Make a new column showing the iso3 code
#   land_cover <- land_cover %>%
#     dplyr::mutate(ISO3 = tolower(countrycode::countrycode(area_code_m49, 'iso3n', 'iso3c')))
#   #The bulk download includes total for the world and regions, so we
#   #drop all areas where the ISO3 code is missing
#   land_cover <- land_cover %>%
#     dplyr::filter(!is.na(ISO3))
#   #Pivot wider so we can make columns for cropland and pastureland
#   land_cover_wide <- land_cover %>%
#     dplyr::select(ISO3, year_code, item, value) %>%
#     tidyr::pivot_wider(id_cols = c(ISO3, year_code),
#                        names_from = item,
#                        values_from =  value)
#   #Rename categories, replace NA values with zeroes
#   land_cover_wide <- land_cover_wide %>%
#     dplyr::rename(arable = `Arable land`,
#                   permanent_crop = `Permanent crops`,
#                   pasture = `Permanent meadows and pastures`,
#                   land_area = `Land area`) %>%
#     dplyr::mutate(arable = ifelse(is.na(arable), 0, arable),
#                   permanent_crop = ifelse(is.na(permanent_crop), 0, permanent_crop),
#                   pasture = ifelse(is.na(pasture), 0, pasture),
#                   land_area = ifelse(is.na(land_area), 0, land_area))
#   #Now add up the categories to get the values for cropland and pasture.
#   #Values are in 1000 ha, convert to ha
#   land_cover_wide <- land_cover_wide %>%
#     dplyr::mutate(cropland = 1000*(arable + permanent_crop),
#                   pasture = 1000*pasture,
#                   land_area = 1000*land_area) %>%
#     dplyr::select(ISO3, year_code, cropland, pasture, land_area)
# 
# iso.land.covers.2011.2022 <- land_cover_wide
# iso.land.covers.2011.2022 <- gdata::rename.vars(iso.land.covers.2011.2022, from=c("ISO3", "year_code"), to=c("iso3", "year"))
# 
# iso.land.covers.2011.2022 <- reshape2::melt(iso.land.covers.2011.2022,
#                      id.vars=c("iso3","year"),
#                      measure.vars=c("cropland","pasture","land_area"),
#                      variable.name='use',
#                      value.name='value')
# 
# iso.land.covers.2011.2022$unit <- "ha"

## ----eval = FALSE-------------------------------------------------------------
# ## Read file names of gridded land cover data for year 2000 distributed in gtapshape:
# devtools::document("..") ## Loads the gtapshape package
# gridded.landcover.file.names <- list.files(system.file("land_cover_2000", package = "gtapshape"),
#                         pattern = "\\.rda$", full.names = TRUE)
# ##summary(aez18)
# ##class(aez18)
# 
# ## Make a gridded dataframe from the SF file with subnational
# ## boundaries (uses the aez18 raster of subnational bounds which is
# ## the default, but could use any other because here we will get rid
# ## of the subnational bounds and focus on country level aggregates):
# g <- make_subnatbound_gridded_dataframe()
# 
# ha.by.iso3.6covers <- all_uses_by_subnatbound(
#     GADM_subnatbound_df= g,
#     gridded.use.file.names=gridded.landcover.file.names) %>%
#     group_by(use,iso3) %>%
#     summarise(iso3.by.use.area = sum(subnatbound.value, na.rm=TRUE) ) %>%
#     group_by(iso3) %>%
#     mutate(iso3.area = sum(iso3.by.use.area, na.rm=TRUE),
#            use.share.by.iso3 =  iso3.by.use.area/iso3.area)
# 
# urban <- ha.by.iso3.6covers %>%
#     dplyr::filter(use ==  "urban")  %>%
#     select(iso3,iso3.by.use.area) %>%
#     unique()
# 
# ## The problem to be solved is to create areas other thea cropland and
# ## pasture for each year in the FAO database:
# iso.land.covers.2011.2022.w <- reshape2::dcast(
#                                              iso.land.covers.2011.2022,
#                                              iso3+year~use,
#                                              value.var="value")
# 

## ----eval=FALSE---------------------------------------------------------------
# ## Start by getting the filling ha:
# unasigned.land.area.by.year <- left_join(iso.land.covers.2011.2022.w,
#                   ha.by.iso3.6covers %>%
#                   dplyr::filter(use ==  "urban")  %>%
#                   select(iso3,iso3.by.use.area) %>%
#                   unique(),
#                   by=c('iso3')) %>%
#     dplyr::rename("urban"="iso3.by.use.area") %>%
#     mutate(unasigned.ha = land_area - cropland - pasture - urban)

## ----eval=FALSE---------------------------------------------------------------
# ## Corrected forest, grassland, shrubland, and other lands shares (the correction keep urban area constant.)
# 
# c.shr <- reshape2::dcast(ha.by.iso3.6covers, iso3 ~ use, value.var = "use.share.by.iso3")
# c.shr$noncroppasturban.ha <- with(c.shr, forest + grassland + other + pasture + shrubland)
# c.shr$forest.s <- with(c.shr, forest/noncroppasturban.ha)
# c.shr$other.s <- with(c.shr, other/noncroppasturban.ha)
# c.shr$grassland.s <- with(c.shr, grassland/noncroppasturban.ha)
# c.shr$shrubland.s <- with(c.shr, shrubland/noncroppasturban.ha)
# 
# iso.land.cover.2011.2022 <- left_join(unasigned.land.area.by.year,
#                   c.shr[,c("iso3", "forest.s",
#                            "other.s", "grassland.s", "shrubland.s" )],
#                   by = c("iso3")) %>%
#     mutate(forest = unasigned.ha*forest.s,
#            other =  unasigned.ha*other.s,
#            grassland = unasigned.ha*grassland.s,
#            shrubland = unasigned.ha*shrubland.s) %>%
#     select(iso3, year, cropland, pasture, urban, forest,
#            other, grassland, shrubland) %>%
#     dplyr::rename(Forest = forest,
#                   SavnGrasslnd = grassland,
#                   Shrubland = shrubland,
#                   Cropland = cropland,
#                   Pastureland = pasture,
#                   Builtupland = urban,
#                   Otherland = other) %>%
#     pivot_longer(
#         cols = c('Forest', 'SavnGrasslnd', 'Shrubland', 'Cropland', 'Pastureland', 'Builtupland', 'Otherland'),
#         names_to = "gsc3",
#         values_to = "value")
# 
# iso.land.cover.2011.2022$unit <- "ha"

## ----eval=FALSE---------------------------------------------------------------
# usethis::use_data(iso.land.cover.2011.2022, overwrite = TRUE)
# ##usethis::use_r("iso.land.cover.2011.2022.R")

## ----eval=FALSE---------------------------------------------------------------
# ## The gridded land cover data for 2000 covers 185 countries while
# ## FAOSTAT's annual country-level data covers 236 countries. There are
# ## 171 countries with data in both datasets, which are the ones for
# ## which this update work:
# countries.in.both.datasets <- intersect(unique(ha.by.iso3.6covers$iso3), unique(iso.land.covers.2011.2022$iso3))
# countries.that.cant.be.updated <- setdiff(unique(iso.land.covers.2011.2022$iso3), countries.in.both.datasets)
# by.reg <- left_join(data.frame(iso3=countries.that.cant.be.updated),regional.concordance, by = "iso3")
# table(by.reg$reg)

