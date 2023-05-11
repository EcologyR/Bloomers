#' Get example dataset
#'
#' @param abund_data a txt dataset with total abundance data and sample_id codes.
#' @param asv_tab_l a txt dataset with data from the ASV_tab in long format with reads per ASV and sample
#' @param metadata a txt dataset with all the metadata needed for this dataset
#'
#' @return
#' @export
#' @import tidyverse
#'
#' @examples
prepare_example_data <- function (abund_data = "./data-raw/example_abund_data.txt",
          asv_tab_l = "./data-raw/example_asv_tab_long.txt",
          metadata = "./data-raw/example_metadata.txt") {

  abund_data <- read.csv2(file = abund_data, sep = '\t') %>%
    as_tibble()
  asv_tab_l <- read.table(file = asv_tab_l, sep = '\t') %>%
    as_tibble()
  metadata <- read.table(file = metadata, sep = '\t') %>%
    as_tibble()
  print(head(abund_data))
  print(head(asv_tab_l))
  print(head(metadata))
  # return(abund_data)
  # return(asv_tab_l)
  # return(metadata)
  aa <- list(abund_data = abund_data, asv_tab_l = asv_tab_l, metadata = metadata)
  return(aa)
}
