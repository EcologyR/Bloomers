
#' Pseudoabundances calculation
#'
#' @param data a tidy tibble long with relative abundance and total abundance per sample as columns
#' @param abund_data a tibble with total abundance data
#' @param rel_abund abundance column with relative abundances (0-1) for each asv from the data tibble
#' @param total_abund column with total abundance data from abund_data tibble
#'
#' @return
#' @export
#' @import tidyverse
#' @import magrittr
#' @import dplyr
#'
#' @examples
calculate_pseudoabund <- function(data, abund_data, by_ = NULL, #sample_id1 = "col1", sample_id2 = "col2",
                                  rel_abund, total_abund){
  if("package:plyr" %in% search()) detach("package:plyr", unload=TRUE)
  pseudoabund_df <- data %>%
    ungroup %>%
    dplyr::left_join(y = abund_data, by = by_, multiple = 'all') %>% #c(!!sample_id1 = !!sample_id2)
    rowwise() %>%
    mutate(pseudoabundance = ({{rel_abund}}*{{total_abund}})) %>%
    as_tibble()
  print(head(pseudoabund_df$pseudoabundance))
  return(pseudoabund_df)
}


