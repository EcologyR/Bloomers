#' Example data prepartion
#'
#'
#' @return
#' @export
#'
#' @examples
#' Create a list of tibbles for the example dataset
#' example_data <- prepare_exampledata()

prepare_exampledata <-  function (abund_data = "./data-raw/example_abund_data.txt",
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

usethis::use_data(exampledata_prepare, overwrite = TRUE)

