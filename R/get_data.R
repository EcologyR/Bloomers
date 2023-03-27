
metadata_HF <- read_delim("../MIAU2_HF/data/envdata/HFW_HFS_metadaata.csv", delim = ";") %>%
  as_tibble() %>%
  stopifnot()

abund_data <- read.table('data-raw/abund_data.txt', sep = '\t',
                         header = T) %>%
  as_tibble() %>%
  stopifnot()

asv_tab_l <- read.table('data-raw/asv_tab_HF_MIAU_subset_test.txt', sep = '\t') %>%
  as_tibble() %>%
  stopifnot()

get_example_data <- function(metadata, abundance_data, asv_tab_long){

metadata %>%
    as_tibble() %>%
    stopifnot()

  abund_data  %>%
    as_tibble() %>%
    stopifnot()

  asv_tab_l %>%
    as_tibble() %>%
    stopifnot()

}

get_data(metadata = metadata_HF, abundance_data = abund_data, asv_tab_long = asv_tab_l)
