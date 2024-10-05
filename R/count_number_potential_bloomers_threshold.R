#' Count number of potential bloomers at different relative abunance () threshold
#'
#' @param threshold relative abundance at which you would like to set the threshold to consider a blooming event
#' @param fraction size fraction at which you are working on
#' @param asv_tab_pseudo table with relative abundances
#' @param z_score_tb table with the z_scores for each asv and sample (columns needed asv_num and sample_id)
#'
#' @return
#' @export
#'
#' @examples
#' n_0.2_0 <- conunt_num_bloomers(threshold = 0, fraction = '0.2', asv_tab_pseudo =  asv_tab_10y_02_pseudo,
#' asv_tab_zclr = asv_tab_10y_02_zclr)
#'
#' # I use a loop for all the thresholds that I'm interested in
#' Define a vector of threshold values
#' threshold_values <- c(0, 0.0001, 0.00015, 0.00025, 0.0005, 0.00075,
#'                      0.001, 0.0015, 0.0025, 0.005, 0.0075,
#'                    0.01, 0.015, 0.0, 0.025, 0.05, 0.075,
#'                     0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6)
#'
#' # Create an empty list to store the results
#' datasets <- list()
#'
#' # Iterate over each threshold value and apply the function
#' for (threshold in threshold_values) {
#'   dataset_name <- paste0("n_0.2_", threshold * 100)  # Create dataset name
#'  dataset <- count_num_bloomers(threshold, 0.2, asv_tab_10y_02_pseudo, z_scores_tb = z_scores_02_red)
#'   datasets[[dataset_name]] <- dataset  # Store the dataset in the list
#' }
#'
#' # Combine all datasets into a single dataframe
#' result_dataset_02 <- bind_rows(datasets)
#'

count_num_bloomers <- function(threshold, fraction, asv_tab_pseudo, z_scores_tb) {

  result <- asv_tab_pseudo |>
    dplyr::left_join(z_scores_tb, by= c('sample_id', 'asv_num')) |>
    dplyr::filter(relative_abundance >= threshold &
                    z_score_ra >= 1.96) |>
    dplyr::distinct(asv_num) |>
    reframe( n = n()) |>
    ungroup() |>
    reframe(num = sum(n)) |>
    dplyr::mutate(threshold = threshold, fraction = fraction)

  return(result)
}
