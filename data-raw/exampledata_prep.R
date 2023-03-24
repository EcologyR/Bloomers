exampledata_prepare = function (abund_data = "./data-raw/example_abund_data.txt",
                             asv_tab_l = "./data-raw/example_asv_tab_long.txt",
                             metadata = "./data-raw/example_metadata.csv") {

abund_data <- read.csv2(file = filesamples)
asv_tab_l <- read.delim(file = asv_tab_l)
metadata <- read.csv2(file = metadata)

}

usethis::use_data(exampledata_prepare, overwrite = TRUE)
#con esto se crea un dataset de ejemplo que se carga con el paquete
