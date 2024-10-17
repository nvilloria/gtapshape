## Aggregates parameters [called by lusepar()}:
aggpar <- function(mapfile,
                   setfile,
                   datfile,
                   stdprm,
                   dir){
    ## Reads aggregation mappings in the aggregation input file and
    ## creates gmap.har:, and checks the consistency between the sets
    ## in the input file and those in the data to be aggregated:
    aggcheck(mapfile,setfile)
    if(file.exists(dir)==FALSE){
        dir.create(dir)}
    fileConn<-file("aggpar.cmf")
    writeLines(
        c(paste("!Input files:!"),
          paste("FILE DDATA=",datfile,";!disaggregate data in GTAP notation"),
          paste("FILE DPARAM=",stdprm,";!disaggregate parameters"),
          paste("FILE EPARAM=gmap.har; !user-supplied parameters from input file"),
          paste("FILE DSETS=", setfile,";!set specification for disaggregate data"),
          paste("FILE ASETS=gmap.har; !set specification for aggregate data"),
          paste("!Output files:!"),
          paste("FILE PARAM=.\\",dir,"\\aggpar.har; !aggregated CDE, CES and CET elasticities",sep="")
          ),
        fileConn)
    close(fileConn)
    system(paste("aggpar -cmf aggpar.cmf"),ignore.stdout=TRUE)
    }
