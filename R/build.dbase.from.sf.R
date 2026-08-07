#' Builds a land use GTAP-compatible land cover database from gridded and national data
#'
#' @param subnat_bound_file SF file with subnational
#'     boundaries. Defaults to the 18 Agroecological Zones from the version 11
#'     GTAP Land Use and Land Cover database described in 
#'     \href{https://www.gtap.agecon.purdue.edu/resources/res_display.asp?RecordID=7407}{Baldoz and Corong (2025)}. The 18 AEZs are created using
#'     publicly available spatial data on the length of the growing period (LGP)
#'     and the thermal climate. Both of these datasets are available as rasters 
#'     from the \href{https://gaez.fao.org/pages/data-viewer}{GAEZ data viewer}. The code creating the shapefile
#'     is provided in `vignette("create.18.aez.shapefile", package = "gtapshape")` 
#' @param year Base year for the database. The FAOSTAT database
#'     distributed with the package is preprocessed and available for
#'     2011-2022
#' @param crop_rasters Source of the gridded crop production data. Either
#'     `"monfreda"`, the default, from
#'     \href{https://doi.org/10.1029/2007GB002947}{Monfreda et al. (2008)}, or
#'     `"cropgrids"` from \href{https://doi.org/10.1038/s41597-024-03247-7}{Tang
#'     et al. (2024)}. Both are distributed with the package.
#' @param grid Optional gridded dataframe from
#'     [make_subnatbound_gridded_dataframe()]. `NULL`, the default, builds it
#'     from `subnat_bound_file`, which takes a minute or two because it
#'     rasterizes GADM against the boundary file.
#'
#'     Supply it when another function needs the same grid, so it is built once
#'     rather than twice. `gtapluco2::build.co2dbase.from.sf()` is the usual
#'     case. When supplied, its subnational boundaries are checked against
#'     `subnat_bound_file`, which still determines the order of the SUBN set.
#' @param dir Directory the output files are written to, relative to the working
#'     directory. Defaults to `"."`. Created if it does not exist. Point it at
#'     the same directory `gtapshapeagg::splitlr()` will use, so one
#'     configuration keeps its files together.
#' @param file Name of the har file with the aggregated physical
#'     data. A basename, not a path -- use `dir` for the location. Should have
#'     an extension. Defaults to "gtaplulc.har". The names of the sets file and
#'     the aggregation template are derived from its stem.
#'
#' @details
#' This builds the *disaggregate* database: every GTAP region, split across the
#' subnational boundaries you supply. It does not aggregate regions or
#' commodities. That is `gtapshapeagg::createdat()`, driven by the aggregation
#' template written here once you have edited it.
#'
#' National totals come from the FAOSTAT data distributed with the package, for
#' the `year` requested. The gridded data decides only how those totals are
#' distributed across subnational boundaries within each country.
#'
#' @return Invisibly, a named list of the data frames written, one per header.
#'     Three files are written to `dir`:
#' \describe{
#'   \item{`file`}{Physical land use and land cover, defaulting to
#'     `gtaplulc.har`. Eight headers: QCR8, VCR8, HARV, QLV3, VLV3, LAND, RTMB.}
#'   \item{`<stem>-sets.<ext>`}{Sets: REG, SUBN, CRP8, CRP9, LCOV. Needed to
#'     read the data file.}
#'   \item{`<stem>-map.txt`}{A GTAP aggregation template with the subnational
#'     boundaries filled in. Edit the region and commodity sections by hand,
#'     then pass it to `gtapshapeagg::createdat()`.}
#' }
#'
#' @seealso
#' [make_subnatbound_gridded_dataframe()] builds the `grid`.
#' `gtapshapeagg::splitlr()` takes the data and sets files and splits national
#' land rents across the subnational boundaries.
#' `gtapshapeagg::createdat()` then aggregates to the target regions and
#' commodities using the map file.
#' `gtapluco2::build.co2dbase.from.sf()` produces matching carbon stocks.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## Defaults: 18 AEZs, 2017, Monfreda, written to the working directory.
#' build.dbase.from.sf()
#'
#' ## Into a directory of its own, sharing one grid with the carbon database:
#' g <- make_subnatbound_gridded_dataframe(subnat_bound_file = "aez18")
#' build.dbase.from.sf(subnat_bound_file = "aez18", year = "2017",
#'                     grid = g, dir = "AEZ18v11c")
#' gtapluco2::build.co2dbase.from.sf(subnat_bound_file = "aez18",
#'                                   grid = g, dir = "AEZ18v11c")
#'
#' ## The 14 WWF biomes instead, with CROPGRIDS:
#' build.dbase.from.sf(subnat_bound_file = "biomes14", crop_rasters = "cropgrids",
#'                     dir = "biomes14v11c", file = "gtaplulc.har")
#' }
build.dbase.from.sf <- function(
                                subnat_bound_file="aez18",
                                year="2017",
                                crop_rasters = "monfreda",
                                grid = NULL,
                                dir = ".",
                                file = "gtaplulc.har"){

    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

    ## Make a gridded dataframe from the SF file with subnational boundaries,
    ## unless the caller already has one. subnat_bound_file still fixes the
    ## order of the SUBN set, so a grid built from other boundaries would give
    ## data and sets that disagree with nothing to signal it.
    if (is.null(grid)) {
        g <- make_subnatbound_gridded_dataframe(subnat_bound_file=subnat_bound_file)
    } else {
        g <- grid
        subnat_bound.sf <- read.subnat.bound.sf(subnat_bound_file)
        extra <- setdiff(unique(g$subnatbound), tolower(subnat_bound.sf$subnat_name))
        if (length(extra))
            stop("`grid` holds subnational boundaries that are not in ",
                 "`subnat_bound_file`: ", paste(utils::head(extra, 5), collapse = ", "),
                 ". Both must describe the same boundaries.")
    }

    ## Subnational bound shares of ag. production or and land cover used to share:

      ## ADDED BY MVCH 12/20
      ## If using Monfreda crop rasters, or cropgrids
      if (crop_rasters == "monfreda") {
        crops = gridded.output.file.names <- list.files(system.file("monfreda", package = "gtapshape"),
                                                        pattern = "\\Production.rda$", full.names = TRUE)
      } else if (crop_rasters == "cropgrids") {
        crops = gridded.output.file.names <- list.files(system.file("cropgrids", package = "gtapshape"),
                                                        pattern = "\\CROPGRIDSv1.08", full.names = TRUE)
      } else {
        crops = gridded.output.file.names <- list.files(system.file("monfreda", package = "gtapshape"),
                                                        pattern = "\\Production.rda$", full.names = TRUE)
      }
      ##END ADDED SECTION

    #Make one list combining crop production, livestock production, and land cover files
    file.names.list <- list(
    crops = crops, ## Also edited this line - MVCH 12/20
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
    subnatbound.use.list[['lstck']] <- dplyr::full_join(subnatbound.use.list[['lstck']],
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
    #year = "2017"
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
        LAND = iso.land.cover.2011.2022[iso.land.cover.2011.2022$year==year,],
        RTMB = timber.rents.by.iso.2000
    )

    ## Description of each dataset, used later when producing the har file:
    attr(iso.data$QCR8, 'description')= 'Crop production (MT) for the 8 gtap crops'
    attr(iso.data$VCR8, 'description')= 'Value of crop production (1000 USD) for the 8 gtap crops'
    attr(iso.data$HARV, 'description')= 'Harvested area (ha) for the 8 gtap crops'
    attr(iso.data$QLV3, 'description')= 'Livestock production (heads) for the 3 gtap livestock sectors'
    attr(iso.data$VLV3, 'description')= 'Livestock output value (1000 USD) for the 3 gtap livestock sectors'
    attr(iso.data$LAND, 'description')= 'Land cover areas (ha)'
    attr(iso.data$RTMB, 'description')= 'Timber land rents (USD Million)'

    ## Dissagregates national data by subantional boundary and aggregates to GTAP regions
    shares.o <- c(rep("crops",3), rep("lstck", 2), rep("cover",2) )
    gsc3.by.iso <- lapply(c(1:length(iso.data)), function(.n){
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

    ## Order  regions and products:
    gsc3.by.iso <- order.gsc3.by.iso(gsc3.by.iso,
                                     gtap_basedatasets_file =
                                         system.file("har", "gsdgset11cMV6.har", package = "gtapshape"),
                                     subnat_bound_file=subnat_bound_file
                                     )

    ## All three outputs go to `dir`. The sets file and the aggregation template
    ## take their names from the stem of `file`.
    data.file.name <- strsplit(file, "\\.")[[1]]
    set.file.name <- paste0(data.file.name[1], "-sets.", data.file.name[2])
    map.file.name <- paste0(data.file.name[1], "-map.txt")

    write.gtaplulc.har(gsc3.by.iso=gsc3.by.iso, file = file.path(dir, file))

    write.gtaplulcsets.har(gtap_basedatasets_file =
                               system.file("har", "gsdgset11cMV6.har", package = "gtapshape"),
                           subnat_bound_file = subnat_bound_file,
                           file = file.path(dir, set.file.name))

    write.gtaplulcagg.txt(
        subnat_bound_file= subnat_bound_file,
        base_aggr_file = system.file("mappings",
                                     "gtapv11c_aggr_base_file.txt",
                                     package = "gtapshape"),
        textfilename = file.path(dir, map.file.name))

    invisible(gsc3.by.iso)
}
