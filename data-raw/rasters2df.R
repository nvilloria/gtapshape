devtools::document("..")
#devtools::load_all("gtapshape")

crop_production_rast_dir = "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/HarvestedAreaYield175Crops_Geotiff/GeoTiff"
crop_area_rast_dir =       "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/HarvestedAreaYield175Crops_Geotiff/GeoTiff"
livestock_density_rast_dir = "C:/Users/nvill/Dropbox/papers/Current/GTAPBIOMES_shared/GTAP_BIOMES/raw_data/Gridded livestock_FAO/2005"
workdir_dir <-  "c:/Users/nvill/Dropbox/REPOS/"

output.file.names <- list.files(crop_production_rast_dir,
                                        pattern = '_Production.tif$',
                                        recursive = TRUE,
                                        full.names = TRUE)

harea.file.names <- list.files(crop_area_rast_dir,
                                           pattern = '_HarvestedAreaHectares.tif$',
                                           recursive = TRUE,
                                           full.names = TRUE)

monfreda.file.names <- c(output.file.names, harea.file.names)

dir_paths <- dir(path = livestock_density_rast_dir, recursive = FALSE, full.names = TRUE)

livestock_rast_file_names <- lapply(dir_paths, FUN = list.files,
                                   pattern = ".tif$", full.names = T)

baseyr_lc_ha_file_names<- list.files(path = file.path(workdir_dir, 'workdir/rasters/'),
                                    pattern = "ha.tif",
                                    full.names = TRUE)


## Read the rasters, convert them to dataframes and store the files to inst/monfreda:

## This is an empty raster used to ensure that all the rasters have
## the same resolution, extent, and geographic coordinate system
## before converting them to dataframes:
gr <- make.global.raster()

lapply( monfreda.file.names, function(.r){
    ## (.r <- monfreda.file.names[[1]])
    r <- terra::rast(.r)
    names(r) <- gsub(pattern = '_Production', replacement = '', names(r))
    ## Ensure raster has the default resolution, extent, and
    ## coordinate reference system:
    r <- apply_global_raster_properties(input.raster = r, global.raster = gr)
    r <- as.data.frame(r, xy=TRUE)
    ## Keep only positive values to decrease file size:
    c <- colnames(r)[3]
    r1 <- r %>% dplyr::filter(!!sym(c)>0)
    ## Round up geographic coordinates to ensure compatibility with
    ## the country-geography raster used for aggregation:
    r1 <- round_up_coordinates(raster.df=r1)
    filename <- tools::file_path_sans_ext(basename(.r))
    assign(filename,r1)
    supplementary.data.path <- system.file("monfreda", package = "gtapshape")
    save(list=filename, file= file.path(supplementary.data.path, paste(filename,".rda",sep="")), compress = "xz",
         compression_level = 9)
}
)

lapply( livestock_rast_file_names, function(.r){
    require(dplyr)
    ## (.r <- livestock_rast_file_names[[6]])
    r <- terra::rast(.r)
    r <- neg_to_zero(r)
    ## multiply density by area of grid cells to get quantity of animals
    grid_cell_area <- terra::cellSize(r, mask=FALSE, lyrs=FALSE, unit="km", transform=TRUE)
    r <-  r * grid_cell_area
    ## Aggregate the rasters because they're at a finer resolution (specific to 2005 global livestock of the world.)
    r <- terra::aggregate(r, fact = 10, fun = "sum")
    r <- na_to_zero(r)
    ## Ensure raster has the default resolution, extent, and
    ## coordinate reference system:
    r <- apply_global_raster_properties(input.raster=r, global.raster = gr)
    ## Convert to data-frame
    rdf <- as.data.frame(r, xy=TRUE)
    rm("r","grid_cell_area")
    gc()
    ## Extract species names:
    species_names <- gsub(livestock_density_rast_dir, "", .r)
    species_names <- substr(species_names, 2, regexpr("_", species_names) - 1)
    colnames(rdf)[3] <- species_names
    ## Eliminate zero-valued gridcells to reduce file size
    rdf1  <- rdf %>% filter(!!sym(species_names)>0)

    #rdf1 <- rdf
    ## Round up geographic coordinates to ensure compatibility with
    ## the country-geography raster used for aggregation:
    rdf1 <- round_up_coordinates(raster.df=rdf1)
    filename <- species_names
    assign(filename,rdf1)
    supplementary.data.path <- system.file("fao_lstck_2005", package = "gtapshape")
    save(list=filename,file= file.path(supplementary.data.path, paste(filename,".rda",sep="")), compress = "xz",
         compression_level = 9)
}
)

lapply( baseyr_lc_ha_file_names, function(.r){
    ## (.r <- baseyr_lc_ha_file_names[[5]])
    r <- terra::rast(.r)
    filename <- tools::file_path_sans_ext(basename(.r))
    names(r) <-sub("_ha$", "", filename)
    ## Ensure raster has the default resolution, extent, and
    ## coordinate reference system:
    r <- apply_global_raster_properties(input.raster = r, global.raster = gr)
    r <- as.data.frame(r, xy=TRUE)
    ## Keep only positive values to decrease file size:
    c <- colnames(r)[3]
    r1 <- r %>% dplyr::filter(!!sym(c)>0)
    ## Round up geographic coordinates to ensure compatibility with
    ## the country-geography raster used for aggregation:
    r1 <- round_up_coordinates(raster.df=r1)

    assign(filename,r1)
    supplementary.data.path <- system.file("land_cover_2000", package = "gtapshape")
    save(list=filename, file= file.path(supplementary.data.path, paste(filename,".rda",sep="")), compress = "xz",
         compression_level = 9)
}
)
