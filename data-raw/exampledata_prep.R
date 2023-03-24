exampledata_prepare = function (filesamples = "./data-raw/HF_WS_abun_nut_chl_CTD.csv",
                             asv_tab_l = "./data-raw/example_asv_tab_long.txt",
                             metadata = "./data-raw/HFW_HFS_metadaata.csv") {

abund_data <- read.csv2(file = filesamples)
asv_tab_l <- read.delim(file = filesamplecodes)
metadata <- read.csv2(file = metadata)


}

usethis::use_data(exampledata, overwrite = TRUE)
#con esto se crea un dataset de ejemplo que se carga con el paquete
