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
#' #community_evenness(c(10, 20, 30), "Shannon")
#' #community_evenness(matrix(c(10, 20, 30, 5, 15, 25), ncol = 2), "Simpson")
#'
#' #abundances <- c(60,54,87,5)
#' #community_evenness(abundances, index = "Shannon") #it goes from 0 to 1
#' #community_evenness(abundances, index = "Simpson") #it goes from 0 to number of species
#' #community_evenness(abundances, index = "Pielou")
#'
#' @author I. Mendoza
#'
#' @export


community_evenness <- function(abundances, index = "Shannon") {
  if (index == "Shannon") {
    n <- sum(abundances)
    pi <- abundances / n
    H <- -sum(pi * log(pi+0.00000001))
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




