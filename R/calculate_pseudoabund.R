
#' Pseudoabundances calculation
#'
#' @param data a tidy tibble long with relative abundance and total abundance per sample as columns
#' @param grp a vector with all the metadata we want to keep for plotting the results and to operate by group
#' @param Relative abundance column with relative abundances (0-1) for each asv
#' @param total_abund column with total abundance data
#'
#' @return
#' @export
#' @import tidyverse
#' @import magrittr
#' @import dplyr
#'
#' @examples
pseudoabun_fc <- function(data, abund_data, by_ = NULL, #sample_id1 = "col1", sample_id2 = "col2",
                          rel_abund, total_abund){
  if("package:plyr" %in% search()) detach("package:plyr", unload=TRUE)
  ##unload library plyr (it doesn't work with this library uploaded) because
  ##plyr::summari[sz]e on a grouped tbl_df seems to destroy the grouping.
  # sample_id1 <- enquo(sample_id1)
  # sample_id2 <- enquo(sample_id2)
  #
  #by = set_names(quo_name(sample_id1), quo_name(sample_id2))
   pseudabund_df <- data %>%
     ungroup %>%
    dplyr::left_join(y = abund_data, by = by_, multiple = 'all') %>% #c(!!sample_id1 = !!sample_id2)
    #group_by_at(grp) %>%
    dplyr::mutate(pseudoabundance = {rel_abund}*{total_abund}) %>%
    # unnest(pseudoabundance) %>%
    # ungroup() %>%
    as_tibble()
  print(head(pseudabund_df))
  return(pseudabund_df)
}

pseudoabun_fc(data = asv_tab_rel, abund_data = abund_data, by = c('Sample_id_ed' = 'sample_id'),
              #sample_id1 = 'Sample_id_ed', #sample_id2 = 'sample_id',
              rel_abund = relative_abundance, total_abund = mean_total_bac)


abund_data <- read.table('data-raw/abund_data.txt', sep = '\t',
                      header = T) %>%
  as_tibble()
#mean_total_bacteria
asv_tab_rel  %>%
  colnames()
abund_data %>%
  colnames()

asv_tab_pseudoabund <- asv_tab_rel %>%
  left_join(abund_data, by = c('Sample_id_ed' = 'sample_id')) %>%
  mutate(pseudoabundance = relative_abundance*mean_total_bac)

asv_tab_rel %>%
  glimpse()

abund_data %>%
  glimpse()



asv_tab_pseudoabund_02 %>%
  colnames()

asv_tab_rel$Sample_id_ed
abund_data$sample_id
