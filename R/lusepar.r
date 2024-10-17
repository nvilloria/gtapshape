## Adds land use parameters to the parameter file . . .
lusepar <- function(mapfile,setfile,datfile,stdprm,dir){
    aggpar(mapfile,setfile,datfile,stdprm,dir)
    fileConn<-file("lusepar.cmf")
    writeLines(
        c(paste("!Input files:!"),
          paste("FILE GTAPSETS=.\\",dir,"\\sets.har; !Sets of the aggregated data, now with physical data",sep=""),
          paste("File ALLPARAM=.\\",dir,"\\aggpar.har; !Sets of all the data",sep=""),
          paste("!Output files:!"),
          paste("FILE NEWPARM=.\\",dir,"\\default.prm; !Sets of the aggregated data, now with physical data",sep="")
          ),
        fileConn)
    close(fileConn)
    system(paste("lusepar -cmf lusepar.cmf"), ignore.stdout=TRUE)
    file.remove(paste("./", dir,"/aggpar.har",sep=""))
     }
