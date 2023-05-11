
source('R/prepare_example_data.R')
source('R/calculate_relative_abundance.R')
source('R/calculate_pseudoabund.R')
source('R/get_anomalies.R')
source("R/blooming_event_summary.R")

library(tidyverse)
library(vegan)

# PREPROCESSING -----
# get example data
data <- prepare_example_data()
str(data)

# calculate relative abundances
asv_tab_l_rel_abund <- data$asv_tab_l %>%
  calculate_rel_abund(group_cols = sample_id)
str(asv_tab_l_rel_abund)

# calculate pseudoabundandances
asv_tab_pseudoabund <- asv_tab_l_rel_abund %>%
  calculate_pseudoabund(abund_data = data$abund_data,  by_ =  c('sample_id', 'sampling_site'),
                        rel_abund = as.numeric(relative_abundance),
                        total_abund = as.numeric(mean_total_bac))
str(asv_tab_pseudoabund)
view(asv_tab_pseudoabund)
#Discover anomalies
z <- asv_tab_pseudoabund %>%
  as_tibble() %>%
  group_by(asv_num) %>%
  dplyr::summarize(anomalies_ab = get_anomalies(values = pseudoabundance, plotting = TRUE)[[1]],
                   anomalies_ra = get_anomalies(values = relative_abundance, plotting = TRUE)[[1]])
z

#miramos anomalias solo de preudoabundancias
taxa <- unique(asv_tab_pseudoabund$asv_num)
out <- data.frame(taxa = taxa,
                  tp1 = NA,
                  tp2 = NA,
                  tp3 = NA,
                  tp4 = NA,
                  tp5 = NA,
                  tp6 = NA,
                  tp7 = NA,
                  tp8 = NA,
                  tp9 = NA,
                  tp10 = NA,
                  tp11 = NA,
                  tp12 = NA,
                  tp13 = NA,
                  tp14 = NA,
                  tp15 = NA,
                  tp16 = NA)
for(i in 1:length(taxa)){
  temp <- subset(asv_tab_pseudoabund, asv_num == taxa[i])
  out[i, 2:17] <- get_anomalies(values = temp$pseudoabundance, plotting = TRUE)[[2]]
}
out

#y hacemos summaries

out2 <- data.frame(taxa = taxa,
                   anomaly_time_point = NA,
                   anomaly_value = NA,
                   compatible_values = NA,
                   bloom_duration = NA)
for(i in 1:length(taxa)){
  temp <- subset(asv_tab_pseudoabund, asv_num == taxa[i])
  z_temp <- subset(out, taxa == taxa[i])
  out2[i, 2:5] <- blooming_summary(values = temp$pseudoabundance,
                                   z_vector = as.numeric(z_temp[2:17]))
} #WARNING THIS DROPS THE SECOND ANOMALY IF PRESENYT
out2
