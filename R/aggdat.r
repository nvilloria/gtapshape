aggdat <- function(mapfile,
                           setfile,
                   datfile,
                   dir ## where to put the data
                   ){
    ## aggcheck() reads aggregation mappings in the aggregation input file and
    ## creates gmap.har:, and checks the consistency between the sets
    ## in the input file and those in the data to be aggregated:
    aggcheck(mapfile,setfile)
    ## Aggregates the data to the sectors, regions and endowments in
    ## gmap.har:
    if(file.exists(dir)==FALSE){
        dir.create(dir)}
    fileConn<-file("aggdat.cmf")
    writeLines(
        c(paste("!Input files:!"),
          paste("FILE DDATA=",datfile,";!Disaggregate GTAP data"),
          paste("FILE DSETS=",setfile,";!Sets for disaggregate GTAP data",sep=""),
          paste("FILE ASETS=gmap.har; !Set mapping for aggregating data"),
          paste("!Output files:!"),
          paste("FILE ADATA=.\\",dir,"\\basedata.har; !Aggregated GTAP data",sep=""),
          paste("FILE GSETS=.\\",dir,"\\sets.har; !Aggregated GTAP sets",sep="")
          ),
        fileConn)
    close(fileConn)
    system(paste("aggdat -cmf aggdat.cmf"), ignore.stdout=TRUE)
}
