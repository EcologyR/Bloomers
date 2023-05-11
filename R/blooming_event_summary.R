#' Blooming event summary
#'
#' @param data a tibble for each ASV that presented a blooming event
#' @param anomaly_point sampling point number at which we have detected the anomaly
#' @param relative_abundance column with relative abundance of the ASV
#' @param range_percentage percentage of change of the relative abundance that at which we admit that the blooming event is mantained
#'
#' @return
#'
#' @export
#'
#' @examples
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' z_vector <- get_anomalies(values = abundance)[[2]]
#' blooming_summary(values = abundance, z_vector = z_vector)
#' blooming_summary(values  = abundance, anomaly_point = 10)
blooming_summary <- function(values,
                             z_vector,
                             cutoff = 1.96,
                             anomaly_point = NULL,
                             range_percentage = 10){
  #test params are as expected.
  if(is.numeric(values) == FALSE){
    stop("Function stopped: values vector need to be numeric")
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
  out <- data.frame(anomaly_value = NA, compatible_values = NA, bloom_duration = NA)
  for(i in 1:length(points_)){
    nexts <- values[(points_[i]+1):length(values)]
    next_logic <- ifelse(nexts < (anomaly_values[i] + perc[i]) &
                           nexts > (anomaly_values[i] - perc[i]), 1, 0)
    out[i,1] <- anomaly_values[i]
    out[i,2] <- sum(next_logic)
    out[i,3] <- sum(next_logic[1:which(next_logic == 0)[1]])
  }
  return(out)
}
