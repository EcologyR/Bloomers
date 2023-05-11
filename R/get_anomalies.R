#' get_anomalies
#'
#' @description A function to calculate anomalies in a time series by calculating
#' z-values of each data point with respect to a specified previous moving window.
#'
#' @param value a vector of abundances, relative abundances or any other parameter
#' per sampling period (numeric)
#' @param time_lag a single value (number) specifying how many previous data points do
#' you want to consider to calculate the anomalies. Default to 4
#' @param cutoff Z-value threshold to detect an anomaly. default at 1.96, which is at percentile 95%
#' @param negative we want to detect only positive anomaliess or also negative. Default to FALSE
#' @param plotting should we plot the time series? Default to TRUE
#'
#' @return A list of two elements, "anomaly" (TRUE / FALSE) and "z" (a vector
#' of z-scores, one per time_point. Note that the first time points are NA, as those have
#' no previous records to calculate the moving window)
#'
#' @export
#'
#' @examples
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' get_anomalies(values = abundance)
#' @author I. Bartomeus
#'
get_anomalies <- function(values = NULL,
                          time_lag = 4,
                          cutoff = 1.96,
                          negative = FALSE,
                          plotting = TRUE) {
  #test params are as expected.
  if(is.numeric(values) == FALSE){
    stop("Function stopped: values vector need to be numeric")
  }
  if(is.vector(values) == FALSE){
    stop("Function stopped: values needs to be a vector")
  }
  if(is.numeric(time_lag) == FALSE){
    stop("Function stopped: time_lag needs to be numeric")
  }
  if(length(time_lag) != 1){
    stop("Function stopped: time_lag needs to be length one")
  }
  if(is.numeric(cutoff) == FALSE){
    stop("Function stopped: cutoff needs to be numeric")
  }
  if(length(cutoff) != 1){
    stop("Function stopped: cutoff needs to be length one")
  }
  if(is.logical(negative) == FALSE){
    stop('Function stopped: negative needs to be logical (TRUE / FALSE)')
  }
  if(is.logical(plotting) == FALSE){
    stop("Function stopped: plotting needs to be logical (TRUE / FALSE)")
  }
  #prevent issues with time_lags larger than the series.
  if(length(values) < (length(cutoff)*2)){
    stop("Function stopped: Time series need to be at least twice the length of the cutoff")
  }
  #calculate for each element, the mean and sd values for the previous time lag
  xt <- rep(NA, length(values))
  sdt <- rep(NA, length(values))
  z <- rep(NA, length(values))
  for (i in (1 + time_lag):length(values)) {
    xt[i] <- mean(values[(i - time_lag):(i-1)])
    sdt[i] <- sd(values[(i - time_lag):(i-1)])
    ifelse(sdt[i] == 0, 0.001, sdt[i]) #This is to prevent Inf's when there is no variance.
    #and calculate the z-score for this moving window.
    z[i] <- round((values[i] - xt[i]) / sdt[i],3)
  }
  #catch if there is an anomaly or not
  if(negative == FALSE){
    if(any(z > cutoff, na.rm = TRUE)){
      anomaly <- TRUE
    } else {
      anomaly <- FALSE
    }
  } else{
    if(any(abs(z) > cutoff, na.rm = TRUE)){
      anomaly <- TRUE
    } else {
      anomaly <- FALSE
    }
  }
  if (plotting) {"black"
    color_ <- ifelse(abs(z) > cutoff, "#b41e31", "black")
    color_ <- ifelse(is.na(z), "white", color_)
    pch_ <- ifelse(abs(z) > cutoff, 19, 1)
    time <- 1:length(values)
    plot(
      values ~ time,
      t = "b",
      col = color_,
      pch = pch_,
      las = 1,
      xlab = "time"
    )
    lines(values ~ time)
  }
  return(list(anomaly = anomaly, z = z))
}
