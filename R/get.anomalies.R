#' get.anomalies
#'
#' @param dat
#' @param time_lag
#'
#' @return
#' @export
#'
#' @examples
#' get.anomalies(abundance = c(runif(16, 0, 2000), 40000), time_stamp = 1:16, time_lag = 4)
#'
get.anomalies <- function(abundance = NULL,
                          time_stamp = NULL,
                          time_lag = NULL,
                          plotting = TRUE){
  #abundance <- runif(16, 0, 2000)
  #abundance[10] <- 20000
  #time_stamp <- 1:16
  #time_lag <- 4
  #plot(abundance ~ time_stamp, t = "b")
  #test abundance is numeric

  #test time_stamp is numeric

  #calculate for each element, the mean abundance for the previous time lag
  xt <- rep(NA, length(abundance))
  xt[1:time_lag] <- NA
  sdt <- rep(NA, length(abundance))
  sdt[1:time_lag] <- NA
  z <- rep(NA, length(abundance))
  z[1:time_lag] <- NA
  for(i in (1+time_lag):length(abundance)){
    xt[i] <- mean(abundance[(i-time_lag):i])
    sdt[i] <- sd(abundance[(i-time_lag):i])
    z[i] <- (abundance[i] - xt[i]) / sdt[i]
  }
  #z <- ifelse(z = Inf, NA, z)
  if(plotting){
    color_ <- ifelse(abs(z) > 1.5, "red", "black")
    color_ <- ifelse(is.na(z), "white", color_)
    pch_ <- ifelse(z > 2 | !is.na(z), 1, 19)
    plot(abundance ~ time_stamp, t = "b",
         col = color_, pch = pch_, las = 1, xlab = "time")
  }
}

