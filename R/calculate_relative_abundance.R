
#' Calculate relative abundances
#'
#' @param data a long tibble with reads as a column
#' @param groups columns to group by that identify exclusively each sample
#'
#' @return
#' @export
#'
#' @examples
calculate_rel_abund <- function(data, groups){
  ##check col sums = 1
  new_data <- data %>%
    group_by(Sample_id_ed, filter) %>%
    mutate(total_reads = sum(reads)) %>%
    mutate(relative_abundance = reads/total_reads)
  return(new_data)
}
