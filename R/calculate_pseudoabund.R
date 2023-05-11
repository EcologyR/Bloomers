
#' Pseudoabundances calculation
#'
#' @param data a tidy tibble long with relative abundance and total abundance per sample as columns
#' @param abund_data a tibble with total abundance data
#' @param rel_abund abundance column with relative abundances (0-1) for each taxa from the data tibble
#' @param total_abund column with total abundance data from abund_data tibble
#' @param by_ columns used to merge dataframes (default is NULL)
#'
#' @return
#' @export
#' @import tidyverse
#' @import magrittr
#' @import dplyr
#'
#' @examples
#' taxa <- c("taxa1", "taxa2", "taxa3", "taxa4")
#' rel.abundances<-  c(runif(16, 0, 2000), runif(16, 0, 2000), runif(16, 0, 2000), runif(16, 0, 2000))
#' data <- data.frame(taxa = rep(taxa, 16), rel.abundances, event = sort(rep(1:16,4)))
#' abund_data <- data.frame(event = 1:16, tot.abund = runif(16, 2000, 5000))
#' calculate_pseudoabund(data, abund_data, rel_abund = rel.abundances, total_abund = tot.abund)

#'@author O. Deulofeu

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


