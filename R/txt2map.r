## Reads aggregation mappings in the aggregation input file and
## creates gmap.har [called by aggcheck()]:
txt2map <- function(mapfile){
    ## a <- substring(mapfile,1,nchar(mapfile)-4)o
    ## Prepare STI file for Modhar
    system(paste("txt2map", mapfile, sep =" "))
}
