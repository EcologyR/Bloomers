#' get_anomalies
#'
#' @description A function to calculate anomalies in a time series by calculating
#' z-values with respect to a specified moving window.
#'
#' @param abundance a vector of abundance per sampling period (numeric)
#' @param time_lag a single value (number) specifying how many previous data points do
#' you want to consider to calculate the anomalies
#' @param plotting should we plot the time series? Default to TRUE
#'
#' @return
#' @export
#'
#' @examples
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' get_anomalies(values = abundance, time_lag)
#' @author I. Bartomeus
#'
get_anomalies <- function(values = NULL,
                          time_lag = 4,
                          cutoff = 1.96,
                          plotting = TRUE) {
  #test values is numeric
  # Rest of the function code that calculates anomalies using the `values` vector
  stopifnot(is.numeric(values))
  #test time_lag is a single value
  stopifnot(length(time_lag) == 1)

  #calculate for each element, the mean and ds values for the previous time lag
  xt <- rep(NA, length(values))
  xt[1:time_lag] <- NA
  sdt <- rep(NA, length(values))
  sdt[1:time_lag] <- NA
  z <- rep(NA, length(values))
  z[1:time_lag] <- NA
  for (i in (1 + time_lag):length(values)) {
    xt[i] <- mean(values[(i - time_lag):(i-1)])
    sdt[i] <- sd(values[(i - time_lag):(i-1)])
    #and calculate the z-score for this moving window.
    z[i] <- round((values[i] - xt[i]) / sdt[i],3)
  }
  if(any(z > cutoff, na.rm = TRUE)){
    anomaly <- "anomaly_detected"
  } else {
    anomaly <- "no_anomalies"
  }
  #z <- ifelse(z = Inf, NA, z) # would Inf's be problematic?
  if (plotting) {
    color_ <- ifelse(abs(z) > cutoff, "red", "black")
    color_ <- ifelse(is.na(z), "white", color_)
    pch_ <- ifelse(z > cutoff, 19, 1)
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
  return(list(anomaly = anomaly, time_points = z))
}
