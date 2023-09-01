#' find_asv_with_anomalies
#'
#' @param anomalies_result the list with the results from get_anomalies function
#' @param anomaly_in1 column of anomalies detected from values that you want a take into account (relative abundances)
#' @param anomaly_in2 column of anomalies detected from other values that you want a take into account (pseudobundances)
#' @param logic1 filter only those ASVs that the anomaly_in1 is TRUE
#' @param logic2 filter only those ASVs that the anomaly_in2 is TRUE
#' @param asv_col column where you have the number/names of the ASVs/taxa.
#'
#' @return a vector with the names of ASVs/taxa that have an anomaly at some point
#' @export
#'
#' @examples
#' find_asv_with_anomalies(anomalies_result = z,
#'   anomaly_in1 = anomalies_ab,
#'   anomaly_in2 = anomalies_ra,
#'   logic1 = TRUE,
#'   logic2 = TRUE,
#'  asv_col = asv_num)
#'
find_asv_with_anomalies <- function(anomalies_result, anomaly_in1, anomaly_in2,
                                    anomaly_in3 = NULL, logic1 = TRUE,
                                    logic2 = TRUE,logic3 = NULL,
                                    asv_col = asv_num) {
  # if(is.list(anomalies_result) == FALSE){
  #   stop("Function stopped: anomalies_result needs to be a list form the get_anomalies function")
  # }
  # if(is.logical({{anomaly_in1}}) == FALSE){
  #   stop("Function stopped: anomaly_in1 needs to be logical (TRUE/FALSE)")
  # }

  asv_potential_bloomers <-
    anomalies_result |>
    dplyr::filter(if (!is.null(logic1)) {{anomaly_in1}} %in% logic1 else TRUE) |>
    dplyr::filter(if (!is.null(logic2)) {{anomaly_in2}} %in% logic2 else TRUE) |>
    dplyr::filter(if (!is.null(logic3)) {{anomaly_in3}} %in% logic3 else TRUE) |>
    dplyr::select({{asv_col}}) |>
    as_vector()

  return(asv_potential_bloomers)
}
