gridded.livestock.concordance <- data.frame(
    gsc3 = c(rep("ctl",3), rep("rmk",3), rep("wol",2)),
    use =  c(rep(c("cattle", "goats", "sheep"),2),
             c("goats", "sheep"))
    )

usethis::use_data(gridded.livestock.concordance, overwrite = TRUE)
## usethis::use_r("gridded.livestock.concordance.R")
