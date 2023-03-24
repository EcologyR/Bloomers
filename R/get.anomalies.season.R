#' get.anomalies.season
#'
#' @description A function to calculate anomalies across seasons in a time series by calculating
#' z-values with respect to past seasons.
#'
#' @param abundance a vector of abundance per sampling period (numeric)
#' @param season a vector of when (season) each sample was collected
#' @param compare_season The season to compare
#' @param plotting should we plot the time series? Default to TRUE
#'
#' @return
#' @export
#'
#' @examples
#' abundance <- runif(16, 0, 2000)
#' abundance[10] <- 20000
#' season <- rep(c("march", "april", "may", "june"), 4)
#' get.anomalies.season(abundance, season)
#' @author I. Bartomeus
#'
get.anomalies.season <- function(abundance = NULL,
                          season = NULL,
                          compare_season = "all",
                          plotting = TRUE){
  #test abundance is numeric
  stopifnot(is.numeric(abundance))
  #crate a dtaframe
  d <- data.frame(id = 1:length(abundance), abundance, season)
  if(compare_season == "all"){ #so far only all implmented. should we allow selecting only one season?
    #loop through seasons
    u_season <- unique(season)
    seasons <- data.frame(season = u_season,
                          x_abundance_season = rep(NA, length(u_season)),
                          sd_abundance_season = rep(NA, length(u_season)))
    for(i in 1:length(u_season)){
      #calculate for each season, the mean and ds abundance.
      seasons$x_abundance_season[i] <- mean(d$abundance[which(d$season == u_season[i])])
      seasons$sd_abundance_season[i] <- sd(d$abundance[which(d$season == u_season[i])])
    }
    d2 <- merge(d, seasons, by = "season", all.x = TRUE)
    d2 <- d2[order(d2$id),]
    #calculate z-value
    z <- rep(NA, length(abundance))
    for(j in d2$id){
      z[j] <- (d2$abundance[j] - d2$x_abundance_season[j]) / d2$sd_abundance_season[j]
    }
    #z <- ifelse(z = Inf, NA, z) # would Inf's be problematic?
    if(plotting){
      color_ <- rep(1:length(u_season), length(abundance)/length(u_season)) #This will FAIL if years are not complete. FIX
      pch_ <- ifelse(z > 1.5, 19, 1)
      time <- 1:length(abundance)
      plot(abundance ~ time, t = "b",
             col = color_, pch = pch_, las = 1, xlab = "time")
      lines(abundance ~ time)
    }
  }
  return(z)
}

