#' Calculate a rolling mean and sd for the abundance of each ASV
#'
#' @param df a tidy dataset in a long format with a column with abundances
#' @param abundance_column a column with relative abundances or pseudoabundances for each ASV
#' @param group_var column with the names of the ASVs to calculate the means for each ASV independently
#' @param group_size number of values previous to consider for the rolling mean
#'
#' @return
#' @export
#' @import tidyverse
#' @import slider
#'
#' @examples
#' asv_tab_pseudoabund_roll_mean_sd <- mean_and_sd_for_group_tidy(
#' df = asv_tab_pseudoabund,
#' value_column = pseudoabundance,
#' group_var = asv_num,
#' group_size = 3)
#'
mean_and_sd_for_group_tidy <- function(df, abundance_column, group_var, group_size) {
  require(tidyverse)
  require(slider)

  # Nest data frame by group_var
  nested_df <- df %>%
    group_by({{group_var}}) %>%
    nest()

  # Define function to calculate mean and standard deviation using slide_mean and slide_sd
  calculate_mean_sd <- function(data) {
    data %>%
      mutate(
        roll_mean = slide_mean(x = {{abundance_column}}, before = group_size - 1, complete = TRUE),
        roll_sd = slide_dbl(.x = {{abundance_column}}, .f = sd, .before = group_size - 1, .complete = TRUE)
        # sd_column = ifelse(length(slide(.x = {{value_column}}, .f = sd, .before = group_size - 1, .complete = TRUE)) == 0, NA, unlist(slide(.x = {{value_column}}, .f = sd, .before = group_size - 1, .complete = TRUE)))
      )
  }

  # Apply function to each nested data frame using lapply
  nested_df$data <- lapply(nested_df$data, calculate_mean_sd)

  # Unnest nested data frame
  unnested_df <- nested_df %>%
    unnest(data)

  return(unnested_df)
}
