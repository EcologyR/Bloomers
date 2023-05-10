# Shannon-Wiener index function
shannon_index <- function(abundances) {
  n <- sum(abundances)
  pi <- abundances / n
  -sum(pi * log(pi))
}

# Community evenness function with options for Shannon or Simpson index
community_evenness <- function(abundances, index = "shannon") {
  if (index == "shannon") {
    H <- shannon_index(abundances)
    H_max <- log(length(abundances))
    E <- H / H_max
  }

  else if (index == "simpson") {
    N <- sum(abundances)
    pi <- abundances / N
    D <- sum(pi^2)
    E <- 1/D
  } else if (index == "pielou"){

    E <- -sum((abundances/sum(abundances))*log(abundances/sum(abundances)))
  }

   else {
    stop("Invalid index option. Please choose among 'shannon', 'simpson' or 'pielou'")
  }
  E
}

# Example usage
abundances <- c(10, 20, 30, 40)
abundances <- c(10,10,10,10)
community_evenness(abundances, index = "shannon") #it goes from 0 to 1
community_evenness(abundances, index = "simpson") #it goes from 0 to number of species
community_evenness(abundances, index = "pielou")

## Example with bloomers data

bloomersdata <- asv_tab_blooms %>% mutate(taxon = asv_num, date_hour = date_hour.x) %>%
  select(taxon, date_hour, pseudoabundance)

bloomers_abund <- bloomersdata %>% pivot_wider(names_from = date_hour, values_from = pseudoabundance)

abund1 <- c(as.numeric(unlist(bloomers_abund[2])))

community_evenness(abund1, index = "shannon")
community_evenness(abund1, index = "simpson")
community_evenness(abund1, index = "pielou")

for (i in 2:dim(bloomers_abund)[2]){

  abundt = community_evenness(c(as.numeric(unlist(bloomers_abund[i]))), index = "shannon")
 if (i == 2) results = abundt else results = rbind(results, abundt)
  }
data.frame(time_event = 1:length(results), evenness = as.numeric(results))


