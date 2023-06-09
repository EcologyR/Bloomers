% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/community_evenness.R
\name{community_evenness}
\alias{community_evenness}
\title{Calculate community evenness index}
\usage{
community_evenness(
  abundances = NULL,
  index = c("Shannon", "Simpson", "Pielou")
)
}
\arguments{
\item{abundances}{a vector or matrix of species abundances}

\item{index}{a string indicating the diversity index to be used.
Options are "Shannon", "Simpson" or "Pielou".}
}
\value{
a numeric value representing the community evenness index
}
\description{
Calculates the evenness index of a community given its abundance data and
diversity index. It is possible to choose among three different indices
(Shannon, Simpson, Pielou)
}
\examples{
community_evenness(c(10, 20, 30), "Shannon")
community_evenness(matrix(c(10, 20, 30, 1, 15, 25), ncol = 2), "Simpson")

abundances <- c(60,54,87,5)
community_evenness(abundances, index = "Shannon")
community_evenness(abundances, index = "Simpson")
community_evenness(abundances, index = "Pielou")

}
\author{
I. Mendoza
}
