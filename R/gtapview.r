## lusepar(mapfile="../ADDLU2GTAP/gtaplulc18mod.txt",
##         dir="gtaplulc18mod")

gtapview <- function(dir){
    fileConn<-file("gtapview.cmf")
    writeLines(
        c(paste("!Input files:!"),
          paste("FILE GTAPDATA=.\\",dir,"\\basedata.har; !Aggregated GTAP data",sep=""),
          paste("FILE GTAPSETS=.\\",dir,"\\sets.har; !Aggregated GTAP sets",sep=""),
          paste("FILE GTAPPARM=.\\",dir,"\\default.prm; !Sets of the aggregated data, now with physical data",sep=""),
          paste("!Output files:!"),
          paste("FILE GTAPVIEW=.\\",dir,"\\baseview.har; !File with headers for viewinh",sep=""),
          paste("FILE TAXRATES=.\\",dir,"\\baserate.har; !File with headers for viewinh",sep="")),
        fileConn)
    close(fileConn)
    system(paste("gtapview -cmf gtapview.cmf"), ignore.stdout=TRUE)
    }
## gtapview(dir="gtaplulc18mod")
