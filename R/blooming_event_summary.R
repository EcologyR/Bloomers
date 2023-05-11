#' Blooming event summary
#'
#' @param data a tibble for each ASV that presented a blooming event
#' @param anomaly_point sampling point number at which we have detected the anomaly
#' @param relative_abundance column with relative abundance of the ASV
#' @param range_percentage percentage of change of the relative abundance that at which we admit that the blooming event is mantained
#'
#' @return
#' @export
#'
#' @examples
blooming_summary <- function(data, anomaly_point, relative_abundance,  range_percentage){

  relative_abundance_anomaly <- data %>%
    dplyr::mutate(row_index := row_number()) %>%
    dplyr::filter(row_index == anomaly_point) %>%
    select(relative_abundance) %>%
    as.numeric()

  perc <-   relative_abundance_anomaly*range_percentage

  data_blooming_maintained  <- data %>%
    dplyr::mutate(row_index := row_number()) %>%
    dplyr::filter(row_index >= anomaly_point) %>%
    dplyr::mutate(mantaining_bloom = case_when(between(relative_abundance, relative_abundance_anomaly-perc, relative_abundance_anomaly+perc) ~ 'TRUE',
                                               #relative_abundance <  ~ 'TRUE',
                                               .default  = 'FALSE')) %>%
    dplyr::mutate(binomial_bloom = case_when(mantaining_bloom == 'TRUE' ~ 1,
                                             mantaining_bloom == 'FALSE' ~ 0)) %>%
    dplyr::mutate(anormal_abundance_points = sum(binomial_bloom)) %>%
    group_by(asv_num, grp = with(rle(binomial_bloom), rep(seq_along(lengths), lengths))) %>%
    mutate(consecutive_bloom = 1:n()) %>%
    ungroup() %>%
    dplyr::filter(grp == 1) %>%
    select(-grp, -binomial_bloom, -mantaining_bloom, -row_index, -relative_abundance) %>%
    slice_max(consecutive_bloom, n = 1) %>%
    cbind(relative_abundance_anomaly)
  return(data_blooming_maintained)
}
