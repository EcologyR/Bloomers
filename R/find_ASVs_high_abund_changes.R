#' Find ASVs with highest changes in their abundances
#'
#' @param data long datset with rolling sd previously calculated
#' @param asv_col column with the names of each taxa
#' @param high_change sd value to be considered a high change in abundance (should be adapted to your dataset)
#'
#' @return
#' @export
#' @import dplyr
#' @import tidyverse
#'
#' @examples
#' high_abund_changes_asvs <- ASVs_high_abund_changes(data = asv_tab_pseudoabund_roll_mean_sd, asv_col = asv_num, high_change = 50000)
#'
ASVs_high_abund_changes <- function(data, asv_col, high_change){
  ASV_vector <- data %>%
    dplyr::filter(roll_sd > high_change) %>%
    dplyr::pull({{asv_col}}) %>%
    unique()
  return(ASV_vector)

}
