#' Blooming event summary
#'
#' @description A function to calculate for how long anomalies persist in a time series.
#'
#' @param values (Required) The time series values. A vector.
#' @param z_vector (Optional) A vector of anomalies (z-values) generated with get_anomalies()
#' @param cutoff (Optional) Z-value threshold to detect an anomaly. default at 1.96, which is at percentile 95%
#' @param anomaly_point (Optional) Instead of a z_vector, one can can add directly the
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
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' z_vector <- get_anomalies(values = abundance)[[3]]
#' blooming_summary(values = abundance, z_vector = z_vector)
#' blooming_summary(values  = abundance, anomaly_point = 10)
#'
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' abundance[11] <- 25000
#' abundance[12] <- 500
#' z_vector <- get_anomalies(values = abundance)[[3]]
#' blooming_summary(values = abundance, z_vector = z_vector)
#' blooming_summary(values  = abundance, anomaly_point = c(10, 11))
#' @author O. Deulofeu, I. Bartomeus
blooming_summary <- function(values = NULL,
                             anomaly_point = NULL,
                             z_vector = NULL,
                             cutoff = 1.96,
                             range_percentage = 10){
  #test params are as expected.
  if (!is.numeric(values)){
    stop("Values vector need to be numeric")
  }

  if (is.null(z_vector) && is.null(anomaly_point)) {
    stop("Please provide one of z_vector or anomaly_point")
  }

  if (!is.null(z_vector)) {

    if (!is.null(anomaly_point)) {
      stop("Please provide only one of z_vector or anomaly_point")
    }

    if (!is.numeric(z_vector)) {
      stop("z_vector must be numeric")
    }

    if(length(values) != length(z_vector)){
      stop("Values and z_vector are of different length")
    }

    if (!is.numeric(cutoff)){
      stop("cutoff needs to be numeric")
    }

    if (length(cutoff) != 1){
      stop("cutoff needs to be length one")
    }

  }

  if (!is.null(anomaly_point)) {

    if (!is.numeric(anomaly_point)){
      stop("anomaly_point must be numeric")
    }

  }


  if (!is.numeric(range_percentage)){
    stop("range_percentage needs to be numeric")
  }
  if (length(range_percentage) != 1){
    stop("range_percentage needs to be length one")
  }

  ## End argument checking



  #select anomaly points from z_vector
  if (is.null(anomaly_point)){
    anomaly_point <- which(z_vector > cutoff)

    if (length(anomaly_point) == 0){
      out <- message("There are no anomalies in the z vector")
    }
  }

  #select the values of those points
  anomaly_values <- values[anomaly_point]
  #select the range of similar values accepted
  perc <-   anomaly_values*(range_percentage/100)
  #classify which ones are within this range
  out <- data.frame(anomaly_time_point = NA,
                    anomaly_value = NA,
                    compatible_values = NA,
                    bloom_duration = NA)
  for(i in 1:length(anomaly_point)){
    nexts <- values[(anomaly_point[i]+1):length(values)]
    nexts_anom <- ifelse(nexts > (anomaly_values[i] - perc[i])
                         # & nexts > (anomaly_values[i] + perc[i]) #esto es para detectar subidas tambien
                         ,1, 0)
    out[i,1] <- anomaly_point[i]
    out[i,2] <- anomaly_values[i]
    out[i,3] <- sum(nexts_anom)
    if(any(nexts_anom == 0, na.rm = TRUE)){ #catch when there are no zeros
      out[i,4] <- sum(nexts_anom[1:which(nexts_anom == 0)[1]])
    } else{
      out[i,4] <- sum(nexts_anom)
    }
  }
  return(out)
}
