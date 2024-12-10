#' Builds a land use GTAP-compatible land cover database from gridded and national data
#'
#' @param subnat_bound_file SF file with subnational
#'     boundaries. Defaults to the 18 Agroecological Zones (Micah:
#'     Version>?)#'
#' @param year Base year for the database. The FAOSTAT database
#'     distributed with the package is preprocessed and available for
#'     2011-2022
#' @param file Name of the har file with the aggregated physical
#'     data. Should have an extension. Defaults to "gtaplulc.har"
#'
build.dbase.from.sf <- function(subnat_bound_file="aez18", year="2017", file = "gtaplulc.har"){
    ## Make a gridded dataframe from the SF file with subnational boundaries:
    g <- make_subnatbound_gridded_dataframe(subnat_bound_file)

    ## Subnational bound shares of ag. production or and land cover used to share:

    file.names.list <- list(
    crops = gridded.output.file.names <- list.files(system.file("monfreda", package = "gtapshape"),
                                            pattern = "\\Production.rda$", full.names = TRUE
                                            ),
    lstck = gridded.livestock.file.names <- c(
        system.file("fao_lstck_2005", "cattle.rda", package = "gtapshape"),
        system.file("fao_lstck_2005", "goats.rda", package = "gtapshape"),
        system.file("fao_lstck_2005", "sheep.rda", package = "gtapshape")),
    cover = gridded.landcover.file.names <- list.files(
        system.file("land_cover_2000", package = "gtapshape"),
        pattern = "\\.rda$", full.names = TRUE)
    )

    ## Aggregate gridded land use and land cover to countries and subnational boundaries of interest

    subnatbound.use.list <- lapply( file.names.list, function(.f){
        ha.by.iso.subnatbound.6covers <- all_uses_by_subnatbound(
            GADM_subnatbound_df= g,
            gridded.use.file.names= .f)
    }
    )

    ## Add concordances to map FAOSTAST crops into CPC codes
    ## (aggregation to GSC3 sectors occurs after sharing out the
    ## national anual FAOSTAT data):
    subnatbound.use.list[['crops']] <- merge(subnatbound.use.list[['crops']],
                                             unique(crop.concordance[, c("use", "item_code_cpc")]),
                                             by = "use",
                                             all.x = TRUE
                                             )
    ## Add concordances to map FAOSTAST livestock sectors to GSC3 sectors:
    subnatbound.use.list[['lstck']] <- full_join(subnatbound.use.list[['lstck']],
                                                 gridded.livestock.concordance,
                                                 by = c("use"),
                                                 relationship="many-to-many")
    ## Add concordances to rename land cover uses to the names use by the
    ## GTAP Center, labeled here as GSC3 sectors:
    subnatbound.use.list[['cover']] <- merge(subnatbound.use.list[['cover']],
                                             land.cover.concordance,
                                             by = "use",
                                             all.x = TRUE)

    shares <- lapply( subnatbound.use.list, function(.f){
        s <- subnatbound.shares(
            subnatbound.use= .f,
            sector=if(any(grepl("item_code_cpc", names(.f)))){"item_code_cpc"}else{"gsc3"}
        )
    }
    )
        ## Annual national data, from FAOSTAT (4-letters data labels are used
    ## later for compatibility with the har file):
    year = "2017"
    iso.data <- list(
        QCR8 = iso.crop.production.2011.2022[
            iso.crop.production.2011.2022$year==year,],
        VCR8 = iso.crop.outputvalue.2011.2022[
            iso.crop.outputvalue.2011.2022$year==year,],
        HARV = iso.crop.harea.2011.2022[
            iso.crop.harea.2011.2022$year==year,],
        QLV3 = iso.gsc3lstk.heads.2011.2022[
            iso.gsc3lstk.heads.2011.2022$year==year,],
        VLV3 =iso.gsc3lstk.outputvalue.2011.2022[
            iso.gsc3lstk.outputvalue.2011.2022$year==year,],
        LAND = iso.land.cover.2011.2022[iso.land.cover.2011.2022$year==year,]
    )

    ## Description of each dataset, used later when producing the har file:
    attr(iso.data$QCR8, 'description')= 'Crop production (MT) for the 8 gtap crops'
    attr(iso.data$VCR8, 'description')= 'Value of crop production (1000 USD) for the 8 gtap crops'
    attr(iso.data$HARV, 'description')= 'Harvested area (ha) for the 8 gtap crops'
    attr(iso.data$QLV3, 'description')= 'Livestock production (heads) for the 3 gtap livestock sectors'
    attr(iso.data$VLV3, 'description')= 'Livestock output value (1000 USD) for the 3 gtap livestock sectors'
    attr(iso.data$LAND, 'description')= 'Land cover areas (ha)'

    ## Dissagregates national data by subantional boundary and aggregates to GTAP regions
    shares.o <- c(rep("crops",3), rep("lstck", 2), "cover")
    gsc3.by.iso <- lapply(c(1:6), function(.n){
        .i <- iso.data[[.n]]
        .s <- shares.o[[.n]]
        .l <- share.out.sectors.to.subnatbound(
            iso.data = .i,
            shares = shares[[.s]],
            units = unique(.i$unit),
            sector = if(any(grepl("item_code_cpc", names(.i)))){"item_code_cpc"}else{"gsc3"}
        )
        ## For the crop variables (production, output value and area
        ## harvested), further aggregation from CPC to GSC3 is needed:
        if( grepl('crops', .s ) ){
            ## Merge and aggregate
            merged <- merge(.l,
                            unique(crop.concordance[, c("item_code_cpc", "gsc3")]),
                            by = "item_code_cpc", all.x = TRUE
                            )
            result <- aggregate(value ~ reg + subnatbound + gsc3,
                                data = merged, sum, na.rm = TRUE)
            ## Add unit
            result$unit <- unique(.l$unit)
            ## Sort the result
            result <- result[order(result$reg, result$subnatbound, result$gsc3), ]
            rownames(result) <- NULL
            .l <- result
        }
        ## Assign attributes for har files:
        attr(.l,'description') <- attr(.i,'description')
        return(.l)
    }
    )
    names(gsc3.by.iso) <- names(iso.data)

    write.gtaplulc.har(gsc3.by.iso=gsc3.by.iso, file = file)

    return(gsc3.by.iso)
}


