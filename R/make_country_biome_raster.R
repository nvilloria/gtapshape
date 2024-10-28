#' Make the Raster with Country-Geographies Boundaries
#'
#' @param subnat_bound_file Simple feature object containing the subnational boundaries
#'
#' @return A SpatRaster object with boundaries for countries and the geographies of interest.
#' @export
make_country_biome_raster <- function(subnat_bound_file){
    ## Global raster to enforce raster settings:
    gr <- make.global.raster()
    ## Load the raster with countries (seems to me tha can come into
    ## the data directory, ask Micah)
    country.raster.path <-
        system.file("GADM", "gadm_rast.tif", package = "gtapshape")
    gadm_rast <- terra::rast(country.raster.path)
    gadm_rast <- apply_global_raster_properties(input.raster = gadm_rast, global.raster = gr)
    ## Load in rds file containing the sf object for subnational areas
    BIOME_vect <- readRDS(subnat_bound_file) %>%
        dplyr::rename(biome_num = subnat_num,
                  biome_name = subnat_name)
    BIOME_vect <- sf::st_as_sf(BIOME_vect)
    BIOME_vect <- terra::vect(BIOME_vect)
    ## Define the levels for the raster
    set_BIO_list <- extract_bio_sets(subnat_bound_file) ## readRDS(file.path(workdir_dir, 'workdir/sets/set_BIO.rds'))
    BIOME_cats <- data.frame(ID=1:length(set_BIO_list), BIO=set_BIO_list)
    ## Make a rest of world category with value equal to the final number in specified biome list
    restofworld_num <- as.numeric(length(set_BIO_list))
    BIOME_rast <- terra::rasterize(BIOME_vect, gr, 'biome_num', background = restofworld_num)
    ## Replace NA values with "7" for the rest of the world
    levels(BIOME_rast) <- BIOME_cats
    ## Concatenate two categorical rasters to get one that shows the
    ## combinations of their levels REG and BIOMES
    GADM_BIOME_rast <- terra::concats(gadm_rast, BIOME_rast)
    return(GADM_BIOME_rast)
    }
