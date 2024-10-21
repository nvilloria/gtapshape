#'@export
run_gtap_program <- function(program, cmf){
     system(paste(program, "-cmf", cmf),ignore.stdout=FALSE)
 }
