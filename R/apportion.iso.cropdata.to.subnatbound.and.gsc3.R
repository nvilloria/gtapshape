#' Make the Raster with Country-Subnational Boundaries
#'
#' @param GADM_subnatbound_df A data frame with country (iso3 codes)
#'     and subnational bounds created by
#'     [make_subnatbound_gridded_dataframe()].
#'
#' @param year Reference year of the database (defaults to 2017).
#'
#' @details ....
#'
#' @return A list of threee dataframes with crop quantity, value and
#'     harvested area by GTAP region, subnational bound (e.g., 18
#'     AEZs) and GTAP sector (gsc3).
#'
#' @seealso [make_subnatbound_gridded_dataframe()]
#'
#' @export
apportion.iso.cropdata.to.subnatbound.and.gsc3 <- function(GADM_subnatbound_df, year=2017){
    ## Gridded output by crop (172 crops, all the data distributed
    ## with the package in the folder inst/monfreda, one compressed
    ## file by crop)
    gridded.output.file.names <- list.files(system.file("monfreda", package = "gtapshape"),
                                            pattern = "\\Production.rda$", full.names = TRUE
                                            )
    ## Aggregate gridded output by crop (172 crops, all the data
    ## distributed with the package in the folder inst/monfreda, one
    ## compressed file by crop) to the country biomes using the
    ## dataframexs `g`:
    tons.by.iso.subnatbound.172crops <- all_uses_by_subnatbound(
        GADM_subnatbound_df= g,
        gridded.use.file.names=gridded.output.file.names
    )
    ## Use the gridded data to calculate the share in country level
    ## production of each subnational bound, and then share country
    ## level production for year `year` using the subnational bound
    ## shares:
    tons.by.iso.subnatbound.172crops <- merge(tons.by.iso.subnatbound.172crops,
                                              unique(crop.concordance[, c("use", "item_code_cpc")]),
                                              by = "use",
                                              all.x = TRUE
                                              )

    shares <- subnatbound.shares(subnatbound.levels = tons.by.iso.subnatbound.172crops,
                                 sector = 'item_code_cpc')

    ## Data included in the package, not lazy-loaded, depends on
    ## argument year:
    faostat.iso.crop.data <- list(
        prod.tons = iso.crop.production.2011.2022[iso.crop.production.2011.2022$year==year,],
        qval.usd1000 = iso.crop.outputvalue.2011.2022[iso.crop.outputvalue.2011.2022$year==year,],
        area.ha = iso.crop.harea.2011.2022[iso.crop.harea.2011.2022$year==year,]
    )

    ## Share out faostat data on crop production, value, and harvested area:
    list.cropdata.by.iso.subnatbound.8gsc3crops <- lapply(faostat.iso.crop.data, function(.d){
        ## .d <- faostat.iso.crop.data[[1]]

        a <- sector.by.subnatbound(iso.data=.d,
                                   shares=shares,
                                   units=unique(.d$unit),
                                   sector='item_code_cpc'
                                   )
        ## Prepare the crop concordance
        cc <- unique(crop.concordance[, c("item_code_cpc", "gsc3")])
        ## Merge and aggregate
        merged <- merge(a, cc, by = "item_code_cpc", all.x = TRUE)
        result <- aggregate(value ~ reg + subnatbound + gsc3, data = merged, sum, na.rm = TRUE)
        ## Add unit
        result$unit <- unique(.d$unit)
        ## Sort the result
        result <- result[order(result$reg, result$subnatbound, result$gsc3), ]
        rownames(result) <- NULL
        result
    }
    )
    return(list.cropdata.by.iso.subnatbound.8gsc3crops)
}
