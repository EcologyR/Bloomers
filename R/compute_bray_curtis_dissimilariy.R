#' Compute Bray-Curtis dissimilarity for adjacent samples
#'
#' @param data a tibble with relative abundances for each ASV in long format
#' @param sample_id_col column that identifies uniquely each sample consecutively
#'
#' @return
#' @export
#'
#' @examples dissimilarity_matrix(data = asv_tab_l_rel_abund, sample_id_col = sample_id)
dissimilarity_matrix <- function(data, sample_id_col) {

  # Extract rownames to mantain them at the output table
  sample_id_unique <- data %>%
    group_by({{sample_id_col}}) %>% ##sample id that identifies uniquely each sample
    dplyr::arrange(.by_group = TRUE)  %>% ## reorder so that it is ordered equally
    dplyr::distinct({{sample_id_col}})

  # Index samples to filter for only consecutive comparisons
  # check name of the columns
  #if ({{sample_id_col}} %in% colnames(data)==FALSE) {stop("There is no sample_id_col column in your data tibble")}

  samples_index <- data %>%
    dplyr::group_by({{sample_id_col}}) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    dplyr::select({{sample_id_col}}) %>%
    dplyr::distinct({{sample_id_col}}) %>%
    as_tibble() %>%
    dplyr::mutate('row_index_2' := dplyr::row_number()) %>%
    dplyr::select({{sample_id_col}}, row_index_2)

  # Compute pairwise Bray-Curtis distances between all rows of data

  dissim_mat <- data %>%
    pivot_wider(id_cols = {{sample_id_col}},  names_from = asv_num, values_from = relative_abundance) %>%
    group_by({{sample_id_col}}) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    column_to_rownames('sample_id') %>%
    vegan::vegdist(method = 'bray', upper = T) %>%
    as.matrix() %>%
    as_data_frame() %>%
    cbind(sample_id_unique) %>%
    dplyr::mutate(row_index := row_number()) %>%
    rowid_to_column() %>%
    as_tibble() %>%
    pivot_longer(cols = starts_with('HFW'), values_to = 'bray_curtis_result', names_to = 'samples') %>%
    left_join(samples_index, by = c('samples' = 'sample_id')) %>%
    dplyr::filter(row_index == (row_index_2-1))


  # # Chech that diagonal elements to zero (i.e., each sample is identical to itself)
  # diag(dissim_mat) <- 0

  # Return the dissimilarity matrix
  return(dissim_mat)
}
