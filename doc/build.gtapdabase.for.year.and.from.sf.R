## ----eval = FALSE-------------------------------------------------------------
# devtools::document("../../gtapshape")
# require(dplyr)

## ----eval = FALSE-------------------------------------------------------------
# 
# ##  user  system elapsed
# ## 24.51    2.92  103.28
# system.time(aez18 <- build.dbase.from.sf())
# system.time(biomes14 <- build.dbase.from.sf(subnat_bound_file='biomes14', file="gtaplulc.biome14.har"))
# 

## ----eval = FALSE-------------------------------------------------------------
# ## Make a gridded dataframe from the SF file with subnational
# ## boundaries:
# g <- make_subnatbound_gridded_dataframe() ## Default is aez18

## ----eval = FALSE-------------------------------------------------------------
# file.names.list <- list(
#     crops = gridded.output.file.names <- list.files(system.file("monfreda", package = "gtapshape"),
#                                             pattern = "\\Production.rda$", full.names = TRUE
#                                             ),
#     lstck = gridded.livestock.file.names <- c(
#         system.file("fao_lstck_2005", "cattle.rda", package = "gtapshape"),
#         system.file("fao_lstck_2005", "goats.rda", package = "gtapshape"),
#         system.file("fao_lstck_2005", "sheep.rda", package = "gtapshape")),
#     cover = gridded.landcover.file.names <- list.files(
#         system.file("land_cover_2000", package = "gtapshape"),
#         pattern = "\\.rda$", full.names = TRUE)
# )
# 
# ## Aggregate gridded land use and land cover to countries and subnational boundaries of interest
# 
# system.time( ## user: 78.25   system: 7.35  elapsed 85.84
#     subnatbound.use.list <- lapply( file.names.list, function(.f){
#         ha.by.iso.subnatbound.6covers <- all_uses_by_subnatbound(
#             GADM_subnatbound_df= g,
#             gridded.use.file.names= .f)
#     }
#     )
# )

## ----eval = FALSE-------------------------------------------------------------
# 
# ## Add concordances to map FAOSTAST crops into CPC codes (aggregation
# ## to GSC3 sectors occurs after sharing out the national anual FAOSTAT
# ## data):
# subnatbound.use.list[['crops']] <- merge(subnatbound.use.list[['crops']],
#                                          unique(crop.concordance[, c("use", "item_code_cpc")]),
#                                          by = "use",
#                                          all.x = TRUE
#                                          )
# 
# ## Add concordances to map FAOSTAST livestock sectors to GSC3 sectors:
# subnatbound.use.list[['lstck']] <- full_join(subnatbound.use.list[['lstck']],
#                                             gridded.livestock.concordance,
#                                             by = c("use"),
#                                             relationship="many-to-many")
# 
# 
# ## Add concordances to rename land cover uses to the names use by the
# ## GTAP Center, labeled here as GSC3 sectors:
# subnatbound.use.list[['cover']] <- merge(subnatbound.use.list[['cover']],
#                                          land.cover.concordance,
#                                          by = "use",
#                                          all.x = TRUE)
# 
# system.time(
#     shares <- lapply( subnatbound.use.list, function(.f){
#         s <- subnatbound.shares(
#             subnatbound.use= .f,
#             sector=if(any(grepl("item_code_cpc", names(.f)))){"item_code_cpc"}else{"gsc3"}
#         )
#     }
#     )
# )

## ----eval = FALSE-------------------------------------------------------------
# ## Annual national data, from FAOSTAT (4-letters data labels are used
# ## later for compatibility with the har file):
# year = "2017"
# iso.data <- list(
#     QCR8 = iso.crop.production.2011.2022[
#         iso.crop.production.2011.2022$year==year,],
#     VCR8 = iso.crop.outputvalue.2011.2022[
#         iso.crop.outputvalue.2011.2022$year==year,],
#     HARV = iso.crop.harea.2011.2022[
#         iso.crop.harea.2011.2022$year==year,],
#     QLV3 = iso.gsc3lstk.heads.2011.2022[
#         iso.gsc3lstk.heads.2011.2022$year==year,],
#     VLV3 =iso.gsc3lstk.outputvalue.2011.2022[
#         iso.gsc3lstk.outputvalue.2011.2022$year==year,],
#     LAND = iso.land.cover.2011.2022[iso.land.cover.2011.2022$year==year,],
#     RTMB = timber.rents.by.iso.2000
# )
# 
# ## Description of each dataset, used later when producing the har file:
# attr(iso.data$QCR8, 'description')= 'Crop production (MT) for the 8 gtap crops'
# attr(iso.data$VCR8, 'description')= 'Value of crop production (1000 USD) for the 8 gtap crops'
# attr(iso.data$HARV, 'description')= 'Harvested area (ha) for the 8 gtap crops'
# attr(iso.data$QLV3, 'description')= 'Livestock production (heads) for the 3 gtap livestock sectors'
# attr(iso.data$VLV3, 'description')= 'Livestock output value (1000 USD) for the 3 gtap livestock sectors'
# attr(iso.data$LAND, 'description')= 'Land cover areas (ha)'
# attr(iso.data$RTMB, 'description')= 'Timber land rents (USD Million)'
# 
# ## Dissagregates national data by subantional boundary and aggregates to GTAP regions
# shares.o <- c(rep("crops",3), rep("lstck", 2), rep("cover",2) ) ## Index to select subnational boundaries shares
# system.time( ## user: 5.27   system: 0.05   elapsed:  5.31
#     gsc3.by.iso <- lapply(c(1:7), function(.n){
#         ## .n <- 1
#         .i <- iso.data[[.n]]
#         .s <- shares.o[[.n]]
#         .l <- share.out.sectors.to.subnatbound(
#             iso.data = .i,
#             shares = shares[[.s]],
#             units = unique(.i$unit),
#             sector = if(any(grepl("item_code_cpc", names(.i)))){"item_code_cpc"}else{"gsc3"}
#         )
#         ## For the crop variables (production, output value and area
#         ## harvested), further aggregation from CPC to GSC3 is needed:
#         if( grepl('crops', .s ) ){
#             ## Merge and aggregate
#             merged <- merge(.l,
#                         unique(crop.concordance[, c("item_code_cpc", "gsc3")]),
#                         by = "item_code_cpc", all.x = TRUE
#                         )
#             result <- aggregate(value ~ reg + subnatbound + gsc3,
#                                 data = merged, sum, na.rm = TRUE)
#             ## Add unit
#             result$unit <- unique(.l$unit)
#             ## Sort the result
#             result <- result[order(result$reg, result$subnatbound, result$gsc3), ]
#             rownames(result) <- NULL
#             .l <- result
#         }
#         ## Assign attributes for har files:
#         attr(.l,'description') <- attr(.i,'description')
#         return(.l)
#     }
#     )
# )
# 
# names(gsc3.by.iso) <- names(iso.data)

## ----eval = FALSE-------------------------------------------------------------
# lapply(gsc3.by.iso, function(x) attr(x,'description'))
# 
# ## Verify number of regions, not tackled.
# lapply( gsc3.by.iso, function(.x){
#     length(unique(.x$reg))
#     setdiff(regional.concordance$reg,unique(.x$reg))
# })
# 
# lapply( gsc3.by.iso, function(.x){
#     length(unique(.x$subnatbound))
# })

## ----eval = FALSE-------------------------------------------------------------
# #Use command 'order.gsc3.by.iso' to assign ordered levels to the character
# #variables (like regions) so when they are output it is consistent with
# #the GTAP base dataset
# gsc3.by.iso.ordered <- order.gsc3.by.iso(gsc3.by.iso,
#                                          gtap_basedatasets_file = system.file("har", "gsdgset11cMV6.har", package = "gtapshape")
#                                          )

## ----eval = FALSE-------------------------------------------------------------
# write.gtaplulc.har(gsc3.by.iso.ordered, file="./gtaplulc.har")

## ----eval = FALSE-------------------------------------------------------------
# write.gtaplulcsets.har(gtap_basedatasets_file =
#                            system.file("har", "gsdgset11cMV6.har", package = "gtapshape"),
#                        file = './gtaplulcsets.har')

## ----eval = FALSE-------------------------------------------------------------
# write.gtaplulcagg.txt(
#                       base_aggr_file = "./BIOMEmod_base.txt",
#                       file = './gtaplulcagg.txt')

