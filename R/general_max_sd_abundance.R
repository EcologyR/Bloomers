#' Detect maximum standard deviations from the mean abundance in the microbial community studied
#'
#' @param data tidy dataset in long format with abundances or pseudoabundances for each taxa
#' @param group_var taxa column number (ASV numbers) to calculate the mean and sd for the each taxa
#' @param abundance_col column with the abundances or pseudoabundances for each taxa
#' @param x number of maximum standard deviations to return
#'
#' @return
#' @export
#' @import tidyverse
#'
#' @examples
#' general_max_changes(asv_tab_pseudoabund, group_var = asv_num, abundance_col = pseudoabundance, x = 5)
#'
general_max_sd_abundance <- function(data, group_var, abundance_col, x){
  z <- data %>%
    as_tibble() %>%
    mutate(abundance = as.numeric({{abundance_col}})) %>%
    group_by({{group_var}}) %>%
    dplyr::summarize(mean_abund = mean(abundance),
                     sd_abund = sd(abundance)) %>%
    slice_max(sd_abund, n = x)
  return(z)
}
