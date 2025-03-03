#' Make the Raster with Country-Subnational Boundaries
#'
#' @param subnat_bound_file Simple feature object containing the
#'     subnational boundaries. The default is [aez18]. [biomes14] is
#'     available for lazy load,
#'     e.g. subnat_bound_file='biomes14'. Other sf objects with
#'     subnational boundaries can be passed on by using the full path
#'     of the file.
#'
#' @return A SpatRaster object with boundaries for countries and the
#'     geographies of interest.
#'
#' @seealso [make_subnatbound_gridded_dataframe()] which invokes this
#'     function before creating a dataframe with XY coordinates and
#'     the combinations of countries and subnational boundaries.
#'
#' @export
make_subnatbound_raster <- function(subnat_bound_file='aez18'){
    ## Global raster to enforce raster settings:
    gr <- make.global.raster()
    country.raster.path <-
        system.file("GADM", "gadm_rast.tif", package = "gtapshape")
    gadm_rast <- terra::rast(country.raster.path)
    gadm_rast <- apply_global_raster_properties(input.raster = gadm_rast, global.raster = gr)
    ## names(gadm_rast) <- "iso3"
    ## Load in rds file containing the sf object for subnational areas
    if (subnat_bound_file %in% c('aez18', 'biomes14')) {
        # Load the lazy-loaded data
        data(list = subnat_bound_file, package = "gtapshape")
        subnat_bound.sf <- get(subnat_bound_file)
    } else if (file.exists(subnat_bound_file)) {
        # Load external file
        subnat_bound.sf <- readRDS(subnat_bound_file)
    } else {
        stop("Invalid subnat_bound_file. Use 'aez18', 'biomes14', or a valid file path.")
    }
    ##Make a spatial vector of subnational bounds:
    subnat_bound_vect <- terra::vect(subnat_bound.sf)
    ## Define the levels for the raster
    subnatbounds.set <- c(subnat_bound_vect$subnat_name)
    subnat_bound_cats <- data.frame(ID=1:length(subnatbounds.set), subnatbound=subnatbounds.set)
    ## Rasterize the subnational boundaries spatial vector and assign its levels
    subnat_bound_rast <- terra::rasterize(subnat_bound_vect, gr, 'subnat_num')
    levels(subnat_bound_rast) <- subnat_bound_cats
    subnat_bound_rast <- apply_global_raster_properties(
        input.raster = subnat_bound_rast, global.raster = gr)
    ## Concatenate two categorical rasters to get one that shows the
    ## combinations of their levels REG and subnat_boundS
    GADM_subnat_bound_rast <- terra::concats(gadm_rast, subnat_bound_rast)
    return(GADM_subnat_bound_rast)
    }
