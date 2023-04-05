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
#' get_anomalies(abundance, time_lag)
#' @author I. Bartomeus
#'
get_anomalies <- function(abundance = NULL,
get_anomalies <- function(data = NULL,
    abundance_col = NULL,
                          time_lag = 4,
                          plotting = TRUE) {
  #test abundance is numeric

    abundance <- data[[which(colnames(data) == abundance_col)]]
    # Rest of the function code that calculates anomalies using the `abundance` vector


  abundance <- data[[abundance_col]]

  stopifnot(is.numeric(abundance))

  #test time_lag is a single value
  stopifnot(length(time_lag) == 1)
  stopifnot(is.numeric(abundance))

  #calculate for each element, the mean and ds abundance for the previous time lag
  xt <- rep(NA, length(abundance))
  xt[1:time_lag] <- NA
  sdt <- rep(NA, length(abundance))
  sdt[1:time_lag] <- NA
  z <- rep(NA, length(abundance))
  z[1:time_lag] <- NA
  for (i in (1 + time_lag):length(abundance)) {
    xt[i] <- mean(abundance[(i - time_lag):i])
    sdt[i] <- sd(abundance[(i - time_lag):i])
    #and calculate the z-score for this moving window.
    z[i] <- (abundance[i] - xt[i]) / sdt[i]
  }
  #z <- ifelse(z = Inf, NA, z) # would Inf's be problematic?
  if (plotting) {
    color_ <- ifelse(abs(z) > 1.5, "red", "black")
    color_ <- ifelse(is.na(z), "white", color_)
    pch_ <- ifelse(z > 1.5, 19, 1)
    time <- 1:length(abundance)
    plot(
      abundance ~ time,
      t = "b",
      col = color_,
      pch = pch_,
      las = 1,
      xlab = "time"
    )
    lines(abundance ~ time)
  }
  return(z)
}
