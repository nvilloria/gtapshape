createdat <- function(mapfile, setfile, datfile, stdprm, dir){
    aggdat(mapfile, setfile, datfile, dir)
    lusepar(mapfile, setfile, datfile, stdprm, dir)
    gtapview(dir)
    inputfiles <- c("aggcheck.cmf","aggdat.cmf","aggpar.cmf",
                    "lusepar.cmf","gtapview.cmf","gmap.har")
    zip(zipfile=paste("./",dir,"/inputs.zip",sep=""),files=inputfiles)
    file.remove(c(inputfiles,"gmap.sti","gmap.log"))
    }
