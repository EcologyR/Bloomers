#' Blooming event summary
#'
#' @description A function to calculate for how long anomalies persist in a time series.
#'
#' @param values The time series values. A vector.
#' @param z_vector A vector of anomalies (z-values) generated with get_anomalies()
#' @param cutoff Z-value threshold to detect an anomaly. default at 1.96, which is at percentile 95%
#' @param anomaly_point Instead of a z_vector, one can can add directly the
#' sampling point number(s) at which we have detected the anomaly manually.
#' @param range_percentage percentage of change around the values at which we admit that the blooming event is mantained
#'
#' @return A data frame with an entri for each anomaly point. For now we show the time point
#' at which this is detected, it's value, how many posterior data_points are within the range_percentage
#' (compatible_values) and the bloom_duration.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' z_vector <- get_anomalies(values = abundance)[[2]]
#' blooming_summary(values = abundance, z_vector = z_vector)
#' blooming_summary(values  = abundance, anomaly_point = 10)
#' }
#' @author O. Deulofeu, I. Bartomeus
blooming_summary <- function(values,
                             z_vector,
                             cutoff = 1.96,
                             anomaly_point = NULL,
                             range_percentage = 10){
  #test params are as expected.
  if(is.numeric(values) == FALSE){
    stop("Function stopped: values vector need to be numeric")
  }
  if(is.numeric(z_vector) == FALSE){
    stop("Function stopped: z_vector need to be numeric")
  }
  if(length(values) != length(z_vector)){
    stop("Function stopped: values and z_vector are of different length")
  }
  if(is.numeric(cutoff) == FALSE){
    stop("Function stopped: cutoff needs to be numeric")
  }
  if(length(cutoff) != 1){
    stop("Function stopped: cutoff needs to be length one")
  }
  if(!any(c(is.null(anomaly_point), is.numeric(anomaly_point)))){
    stop("Function stopped: anomaly_point has to be either NULL or numeric")
  }
  if(is.numeric(anomaly_point)){
    message("Warning: as anomaly_point is specified, z_vector is ignored")
  }
  if(is.numeric(range_percentage) == FALSE){
    stop("Function stopped: range_percentage needs to be numeric")
  }
  if(length(range_percentage) != 1){
    stop("Function stopped: range_percentage needs to be length one")
  }
  #select anomaly points from z_vector
  if(is.null(anomaly_point)){
    points_ <- which(z_vector > cutoff)
    if(length(points_) == 0){
      stop("Function stopped: there are no anomalies in the z vector")
    }
  } else {
    points_ <- anomaly_point
  }
  #select the values of those points
  anomaly_values <- values[points_]
  #select the range of similar values accepted
  perc <-   anomaly_values*(range_percentage/100)
  #classify which ones are within this range
  out <- data.frame(anomaly_time_point = NA, anomaly_value = NA, compatible_values = NA, bloom_duration = NA)
  for(i in 1:length(points_)){
    nexts <- values[(points_[i]+1):length(values)]
    next_logic <- ifelse(nexts < (anomaly_values[i] + perc[i]) &
                           nexts > (anomaly_values[i] - perc[i]), 1, 0)
    out[i,1] <- points_[i]
    out[i,2] <- anomaly_values[i]
    out[i,3] <- sum(next_logic)
    if(any(next_logic == 0, na.rm = TRUE)){ #catch when there are no zeros
      out[i,4] <- sum(next_logic[1:which(next_logic == 0)[1]])
    } else{
      out[i,4] <- 0
    }
  }
  return(out)
}
