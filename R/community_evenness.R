#' Calculate community evenness index
#'
#' Calculates the evenness index of a community given its abundance data and
#' diversity index. It is possible to choose among three different indices (Shannon, Simpson or Pielou)
#'
#' @param abundances a vector or matrix of species abundances
#' @param index a string indicating the diversity index to be used.
#'   Options are "Shannon", "Simpson" or "Pielou".
#'
#' @return a numeric value representing the community evenness index
#'
#' @examples
#' community_evenness(c(10, 20, 30), "Shannon")
#' community_evenness(matrix(c(10, 20, 30, 5, 15, 25), ncol = 2), "Simpson")
#'
#' community_evenness(abundances, index = "Shannon") #it goes from 0 to 1
#' community_evenness(abundances, index = "Simpson") #it goes from 0 to number of species
#' community_evenness(abundances, index = "Pielou")
#'
#'
#' @export


community_evenness <- function(abundances, index = "Shannon") {
  if (index == "Shannon") {
    n <- sum(abundances)
    pi <- abundances / n
    H <- -sum(pi * log(pi))
    H_max <- log(length(abundances))
    E <- H / H_max
  }

  else if (index == "Simpson") {
    N <- sum(abundances)
    pi <- abundances / N
    D <- sum(pi^2)
    E <- 1/D
  }

  else if (index == "Pielou"){

    E <- -sum((abundances/sum(abundances))*log(abundances/sum(abundances)))
  }

   else {
    stop("Invalid index option. Please choose among 'Shannon', 'Simpson' or 'Pielou'. Be aware of capitalization")
  }
  E
}


## Example with bloomers data

load("./data/bloomersdata.rda")
library(tidyr)

bloomers_abund <- bloomersdata %>%
  pivot_wider(names_from = date_hour, values_from = pseudoabundance)

abund1 <- c(as.numeric(unlist(bloomers_abund[2])))

community_evenness(abund1, index = "Shannon")
community_evenness(abund1, index = "Simpson")
community_evenness(abund1, index = "Pielou")

for (i in 2:dim(bloomers_abund)[2]){

  abundt = community_evenness(c(as.numeric(unlist(bloomers_abund[i]))), index = "Shannon")
 if (i == 2) results = abundt else results = rbind(results, abundt)
  }
data.frame(time_event = 1:length(results), evenness = as.numeric(results))


