## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
# #Load in one crop to check .nc file types are effectively loaded by terra package.
#   abaca_2020 <- terra::rast("./raw_data/CROPGRIDSv1.08_NC_maps/CROPGRIDSv1.08_abaca.nc")
#   abaca_2020

## ----eval = FALSE-------------------------------------------------------------
# #plot just the harvested area variable
#   abaca_2020[names(abaca_2020)[2]]
#   terra::plot(abaca_2020[[2]])

## ----eval = FALSE-------------------------------------------------------------
# #Load in 'gtapshape' commands
# library(gtapshape)
# 
# #Now make a list of all the files, make sure they have some properties as current data, and save as dataframes in .rda format.
#   #List files
#   CROPGRIDS.file.list <- list.files(path = "./raw_data/CROPGRIDSv1.08_NC_maps/",
#                                      pattern = ".nc",
#                                      full.names = TRUE)
# 
#   #Make sure we just do the ones for individual crops
#   CROPGRIDS.file.list
# 
#   #Make a global raster with the intended CRS, resolution, and extent
#   gr <- make.global.raster()
# 
#   #Use lapply to work through all of them
#   lapply( CROPGRIDS.file.list, function(.r){
#     # (.r <- CROPGRIDS.file.list[[1]]) #REMOVE BEFORE RUNNING FUNCTION
#     r <- terra::rast(.r)
#     r <- r[["harvarea"]]
#     #Get the file name without the directory so we can store the crop name
#     cropname <- gsub(pattern = 'CROPGRIDSv1.08_', replacement = '', basename(.r))
#     cropname <- gsub(pattern = '.nc', replacement = '', cropname)
#     names(r) <- cropname
#     ## Ensure raster has the default resolution, extent, and
#     ## coordinate reference system:
#     r <- apply_global_raster_properties(input.raster = r, global.raster = gr)
#     r <- as.data.frame(r, xy=TRUE)
#     ## Keep only positive values to decrease file size:
#     c <- colnames(r)[3]
#     r1 <- r %>% dplyr::filter(!!sym(c)>0)
#     ## Round up geographic coordinates to ensure compatibility with
#     ## the country-geography raster used for aggregation:
#     r1 <- round_up_coordinates(raster.df=r1)
#     filename <- tools::file_path_sans_ext(basename(.r))
#     assign(filename,r1)
#     supplementary.data.path <- system.file("cropgrids", package = "gtapshape")
#     save(list=filename, file= file.path(supplementary.data.path, paste(filename,".rda",sep="")), compress = "xz",
#          compression_level = 9)
# }
# )
# 
# ##Clean-up environment
# rm(list=ls()[! ls() %in% c("gr")])
# gc()

