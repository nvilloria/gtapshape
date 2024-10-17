## Checks the consistency between the sets in the input file and those
## in the data to be aggregated:
aggcheck <-function(mapfile,setfile){
    txt2map(mapfile)
    fileConn<-file("aggcheck.cmf")
    writeLines(
        c(paste("!Input files:!"),
          paste("FILE DSETS=",setfile,";!Sets for disaggregate GTAP data",sep=""),
          paste("FILE ASETS=gmap.har; !Set mapping for aggregating data"),
          paste("!Output files:!"),
          paste("FILE checkmap=aggcheck.har;")
          ),
        fileConn)
    close(fileConn)
    system("aggcheck -cmf aggcheck.cmf", ignore.stdout=TRUE)
    file.remove("aggcheck.har")
    }
