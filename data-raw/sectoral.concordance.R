## code to prepare `crop.concordances` dataset goes here
##########################################################################################
fao_production <- FAOSTAT::get_faostat_bulk(code = "QCL", data_folder =   tempdir())

require(dplyr)
fao_production.0 <- fao_production %>%
    dplyr::rename(area_code_m49 = area_code__m49_,
                  item_code_cpc = item_code__cpc_) %>%
    dplyr::mutate(element_code = as.numeric(element_code),
                  area_code_m49 = substr(area_code_m49, 2, 4),
                  area_code_m49 = as.numeric(area_code_m49),
                  item_code_cpc = gsub("'", "", item_code_cpc),
                  item_code_fao = item_code)

head(fao_production.0)

crop_fao_item_codes <- fao_production.0 %>% dplyr::select(item, item_code_fao, item_code_cpc) %>% unique

fao.to.cpc_file = "./raw_data/monfreda_fao_cropname_concordance.xlsx"

fao.to.cpc <- readxl::read_excel(fao.to.cpc_file)

fao.to.cpc <- fao.to.cpc %>%
    dplyr::rename(item = monfreda_detailed, crop = crop_raster_name) %>%
    dplyr:: mutate(crop = gsub(' ', '', crop))

fao.to.cpc <- dplyr::left_join(fao.to.cpc, crop_fao_item_codes, by = c('item'))

## Make new entry for cotton lint
  ## First is the cottonseed oil
  cottonlint <- data.frame(crop = "cotton", item = "Cotton lint, ginned", item_code_fao = 767, item_code_cpc = "01921.02")

  ## Add the new rows using rbind()
  fao.to.cpc <- rbind(fao.to.cpc, cottonlint)
  
## Replace missing values with 0191 - Forage products
fao.to.cpc <- fao.to.cpc %>%
    mutate(item_code_cpc = ifelse(
               is.na(item_code_cpc), "0191", item_code_cpc))

##########################################################################################
cpc.to.gsc3_file = "./raw_data/gtap_GSC_CPC_concordance.csv"

cpc.to.gsc3 <- read.csv(cpc.to.gsc3_file,
                           header = T,
                           colClasses = c(cpc_code = "character"))

gsc3_list <- cpc.to.gsc3$GSC_code %>% unique()

gsc3_cpc_codes <- lapply(gsc3_list, function (x) {
    gsc3_vec <- cpc.to.gsc3 %>% dplyr::filter(GSC_code==x) %>% dplyr::select(cpc_code)
    gsc3_vec$cpc_code
  })

names(gsc3_cpc_codes) <- gsc3_list

##########################################################################################
## Crops:
crop.concordance <- fao.to.cpc %>%
    dplyr::mutate(## item_code_cpc_2 = substr(item_code_cpc, 1, 2),
               item_code_cpc_3 = substr(item_code_cpc, 1, 3),
               item_code_cpc_4 = substr(item_code_cpc, 1, 4)## ,
               ## item_code_cpc_5 = substr(item_code_cpc, 1, 5)
           ) %>%
    dplyr::mutate(gsc3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pdr'])), 'pdr',
                                ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wht'])), 'wht',
                                       ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['gro'])), 'gro',
                                              ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['v_f'])), 'v_f',
                                                     ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['osd'])), 'osd',
                                                            ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['c_b'])), 'c_b',
                                                                   ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['pfb'])), 'pfb',
                                                                          ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr',
                                                                          ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ocr'])), 'ocr', NA)))))))))) %>%
    select(crop, item, item_code_fao, item_code_cpc, gsc3)

crop.concordance$gsc3[crop.concordance$crop=="rapeseed"] <- "osd"

crop.concordance <- crop.concordance[,c("crop", "item_code_fao", "item_code_cpc", "gsc3")]
crop.concordance <- gdata::rename.vars(crop.concordance, from="crop",to="use")

head(crop.concordance)

usethis::use_data(crop.concordance, overwrite = TRUE)
## usethis::use_r("crop.concordance.R")

## Livestock:
livestock.concordance <- fao_production.0 %>%
    filter(year_code==2017) %>%
    select(item, item_code, item_code_cpc) %>%
    unique() %>%
    dplyr::mutate(## item_code_cpc_2 = substr(item_code_cpc, 1, 2),
               item_code_cpc_3 = substr(item_code_cpc, 1, 3),
               item_code_cpc_4 = substr(item_code_cpc, 1, 4),
               item_code_cpc_5 = substr(item_code_cpc, 1, 5)
           ) %>%
    dplyr::mutate(gsc3 = ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['ctl'])), 'ctl',
                                ifelse(item_code_cpc_5 %in% c(unlist(gsc3_cpc_codes['ctl'])), 'ctl',
                                       ifelse(item_code_cpc_3 %in% c(unlist(gsc3_cpc_codes['rmk'])), 'rmk',
                                       ifelse(item_code_cpc_4 %in% c(unlist(gsc3_cpc_codes['wol'])), 'wol', NA))))) %>%
    na.omit() %>%
    dplyr::rename(item_code_fao=item_code,
                  use=item) %>%
    select(use, item_code_fao, item_code_cpc, gsc3)

head(livestock.concordance)

usethis::use_data(livestock.concordance, overwrite = TRUE)
## usethis::use_r("livestock.concordance.R")
