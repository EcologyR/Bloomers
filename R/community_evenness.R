#' Calculate community evenness index
#'
#' Calculates the evenness index of a community given its abundance data and
#' diversity index. It is possible to choose among three different indices
#' (Shannon, Simpson, Pielou)
#'
#' @param abundances a vector or matrix of species abundances
#' @param index a string indicating the diversity index to be used.
#'   Options are "Shannon", "Simpson" or "Pielou".
#'
#' @return a numeric value representing the community evenness index
#'
#' @examples
#' community_evenness(c(10, 20, 30), "Shannon")
#' community_evenness(matrix(c(10, 20, 30, 1, 15, 25), ncol = 2), "Simpson")
#'
#' abundances <- c(60,54,87,5)
#' community_evenness(abundances, index = "Shannon")
#' community_evenness(abundances, index = "Simpson")
#' community_evenness(abundances, index = "Pielou")
#'
#' @author I. Mendoza
#'
#' @export


community_evenness <- function(abundances = NULL,
                               index = c("Shannon", "Simpson", "Pielou")
                               ) {

  index <- match.arg(index)
  index <- tolower(index)

  if (index == "shannon" || index == "pielou") {
    shannon <- vegan::diversity(abundances, index = "shannon")
    out <- shannon
  }

  if (index == "simpson") {
    simpson <- vegan::diversity(abundances, index = "simpson")
    out <- simpson
  }

  ##to obtain 7 significant digits and not 2.
  old <- options(pillar.sigfig = 7)

  if (index == "pielou"){
    specnumber <- vegan::specnumber(abundances)
    pielou <- shannon/log(specnumber)
    out <- pielou
  }

  return(out)

}




