#
#
# asv_tab <- read.table('data-raw/asv_tab_HF_MIAU_subset_test.txt', sep = '\t',
#                       header = T)
#
# asv_tab %>%
#   colnames()
# asv_tab_rel <- asv_tab %>%
#   group_by(Sample_id_ed, filter) %>%
#   mutate(total_reads = sum(reads)) %>%
#   mutate(realative_abundance = reads/total_reads)
# asv_tab_HF_MIAU_filt_rel <- apply(X = asv_tab_HF_MIAU_filt[,c(1:64)],
#                                   MARGIN = 2, function(x) {x / sum(x)})
relative.abundance <- function(data, groups){
  ##check col sums = 1
  new_data <- data %>%
    group_by(Sample_id_ed, filter) %>%
    mutate(total_reads = sum(reads)) %>%
    mutate(realative_abundance = reads/total_reads)
  return(new_data)
}

# asv_tab_rel <- asv_tab %>%
#   relative.abundance()
# relative.abundance(data, dim1, dim2){
#   row.names(data) <- data[,1]
#   apply(X = data[,c(dim1:dim2)],
#         MARGIN = 2, function(x) {x / sum(x)})
# }
