exampledata_prepare = function (filesamples = "./data-raw/HF_WS_abun_nut_chl_CTD.csv",
                             filesamplecodes = "./data-raw/sample_codes_HF_MIAU.txt",
                             metadata = "./data-raw/HFW_HFS_metadaata.csv") {

samples <- read.csv2(file = filesamples)
codes <- read.delim(file = filesamplecodes)
metadata <- read.csv2(file = metadata)


}

usethis::use_data(exampledata, overwrite = TRUE)
#con esto se crea un dataset de ejemplo que se carga con el paquete
