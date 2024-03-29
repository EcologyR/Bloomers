
#' Calculate relative abundances from a data with reads from each taxa.
#'
#' @param data a long tibble with reads as a column
#' @param group_cols columns to group by that identify exclusively each sample
#'
#' @return
#' @export
#'
#' @examples
#'
#'
calculate_rel_abund <- function(data, group_cols){
  ##check col sums = 1
  new_data <- data |>
    group_by({{group_cols}}) |>
    dplyr::mutate(total_reads = sum(reads)) |>
    dplyr::mutate(relative_abundance = reads/total_reads)
  return(new_data)
}


