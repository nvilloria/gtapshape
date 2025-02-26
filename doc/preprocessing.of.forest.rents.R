## ----eval = FALSE-------------------------------------------------------------
# devtools::document("gtapshape")
# require(dplyr)
# require(HARr)

## ----eval = FALSE-------------------------------------------------------------
# ## Read the data on forestry provided by the GTAP center:
# data = read_har(system.file("har", "forestdata.har", package = "gtapshape"))
# 
# ## Transform data arrays into dataframes:
# aa <- lapply( list(
#     ## Timberland accessible area, unit: hectare:
#     tarea=data[['tmha']],
#     ## Timberland marginal land rent, 2000 US$/ha yr (with negative rents)
#     trent=data[['tmrn']]), function(.x){
#     ## Transform arrays into dataframes:
#     as.data.frame.table(.x, responseName="value", stringsAsFactors=FALSE)
# })
# 
# 
# ## Convert ha to thousand ha:
# aa[['tarea']]$ha.1000 <- aa[['tarea']]$value/1000
# ## Convert US$/ha to  Million US$/ha:
# aa[['trent']]$US.M.per.ha <- aa[['trent']]$value/1000000
# 
# head(aa[['trent']])
# head(aa[['tarea']])
# 
# timber.rents.by.iso.2000 <- left_join(
#     aa[['trent']][,c("treemgmt", "ctry", "US.M.per.ha")],
#     aa[['tarea']], by = c("treemgmt", "ctry") ) %>%
#     mutate(trenttot = ha.1000*US.M.per.ha)   %>%
#     group_by(ctry) %>%
#     summarise(value = sum(trenttot, na.rm = TRUE)) %>%
#     mutate(use="timber",
#            gsc3="Forest",
#            unit = "USD.M",
#            year=2000
#            ) %>%
#     dplyr::rename(iso3=ctry)
# 
# 
# usethis::use_data(timber.rents.by.iso.2000, overwrite = TRUE)
# ## usethis::use_r("timber.rents.by.iso.2000.R")

