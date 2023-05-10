
source('R/prepare_example_data.R')
source('R/calculate_relative_abundance.R')
source('R/calculate_pseudoabund.R')
source('R/general_max_sd_abundance.R')
source("R/calculate_rolling_mean_sd.R")
source('R/find_ASVs_high_abund_changes.R')

library(tidyverse)
library(vegan)

### PREPROCESSING ###
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

### IDENTIFICATION ###
# Calculate the general maximum sd for the dataset and return the highest, to have an idea of which are the changes
# in relative abundances for our datset
general_max_sd_abundance(asv_tab_pseudoabund, group_var = asv_num, abundance_col = pseudoabundance, x = 5)

# Calculate for each ASV the rolling mean and sd with the previous values for the abundance to detect highest changes
asv_tab_pseudoabund_roll_mean_sd <- rolling_mean_and_sd(
  df = asv_tab_pseudoabund,
  abundance_column = pseudoabundance,
  group_var = asv_num,
  group_size = 3) %>%
  as_tibble()

# Create a vector with the ASVs that present the highest changes in relative abundances
high_abund_changes_asvs <- ASVs_high_abund_changes(data = asv_tab_pseudoabund_roll_mean_sd,
                                                   asv_col = asv_num,
                                                   high_change = 50000)


##filter ASV_tab by those ASVs that have high changes in abundances
asv_tab_blooms <- asv_tab_pseudoabund_roll_mean_sd %>%
  dplyr::filter(asv_num %in% high_abund_changes_asvs)

# CHARACTERIZATION -----
#explore the changes of each ASV and characterize it
library(ggplot2)
asv_tab_blooms %>%
  ggplot(aes(date_hour.x, pseudoabundance))+
  geom_point()+
  facet_grid(vars(asv_num))+
  #geom_line()+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

asv_tab_pseudoabund %>%
  ggplot(aes(date_hour.x, pseudoabundance))+
  geom_point(aes(colour = ))+
  geom_line(aes(group = asv_num))+
  facet_grid(vars(asv_num))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

##group potential bloomers that behave similarly
cluster_bloomers <-

##create groups of environmental variables that behave similarly
  ## kmeans
cluster <- env_variables()

##correlate changes in abundances with changes in environmental parameters

# Blooming event effects on the community
## Eveness




## Bray Curtis dissimilarity function
# asv_tab_l_rel_abund %>%
#   colnames()

bray_curtis_results <- dissimilarity_matrix(data = asv_tab_l_rel_abund, sample_id_col = sample_id)

### recober metadata for plotting
metadata <- asv_tab_l_rel_abund %>%
  select(sample_id, date_hour, day_moment) %>%
  unique()

### plot the community dissimilarity
bray_curtis_results %>%
  left_join(metadata, by = c('samples' = 'sample_id')) %>%
  mutate(date_hour = (as.POSIXct(date_hour, format = "%d/%m/%y %H:%M:%S"))) %>%
  ungroup() %>%
  ggplot(aes(date_hour, bray_curtis_result))+
  geom_point()+
  geom_line()+
  labs(y = 'Bray-Curtis dissimilariy', x = 'Samples')+
  scale_x_datetime()+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())





#create a vector with sample names to not lose them
# sample_id <- asv_tab_l_rel_abund %>%
#   group_by(sample_id) %>%
#   arrange(.by_group = TRUE) %$%
#   sample_id %>%
#   unique()

##samples index (create a number for each sample to be able to filter only comparisons between one sample and the one before)
# samples_index <- asv_tab_l_rel_abund %>%
#   pivot_wider(id_cols = sample_id,  names_from = asv_num, values_from = relative_abundance) %>%
#   group_by(sample_id) %>%
#   arrange(.by_group = TRUE) %>%
#   column_to_rownames('sample_id') %>%
#   vegan::vegdist(method = 'bray', upper = T) %>%
#   as.matrix() %>%
#   as_data_frame() %>%
#   cbind(rownames) %>%
#   mutate(row_index_2 =row_number()) %>%
#   dplyr::select(row_index_2, rownames)

# samples_index <- asv_tab_l_rel_abund %>%
#   group_by(sample_id) %>%
#   arrange(.by_group = TRUE) %>%
#   select(sample_id) %>%
#   distinct(sample_id) %>%
#   as_tibble() %>%
#   mutate(row_index_2 = row_number()) %>%
#   dplyr::select(sample_id, row_index_2)

#calculate Bray Curtis dissimilarities for all samples
# bray_curtis_results <- asv_tab_l_rel_abund %>%
#   pivot_wider(id_cols = sample_id,  names_from = asv_num, values_from = relative_abundance) %>%
#   group_by(sample_id) %>%
#   arrange(.by_group = TRUE) %>%
#   column_to_rownames('sample_id') %>%
#   vegan::vegdist(method = 'bray', upper = T) %>%
#   as.matrix() %>%
#   as_data_frame() %>%
#   cbind(sample_id) %>%
#   mutate(row_index = row_number()) %>%
#  # rowid_to_column() %>%
#   as_tibble() %>%
#   pivot_longer(cols = starts_with('HFW'), values_to = 'bray_curtis_result', names_to = 'samples') %>%
#   left_join(samples_index, by = c('samples' = 'sample_id')) %>%
#   dplyr::filter(row_index == (row_index_2-1))

  # mutate(comparison_samples = paste(rownames_bray, samples)) %>%
  # dplyr::filter(comparison_samples %in% c('HFW-210208M HFW-210208T', 'HFW-210208T HFW-210209M', 'HFW-210209M HFW-210209T',
  #                                 'HFW-210209T HFW-210210M', 'HFW-210210M HFW-210210T', 'HFW-210210T HFW-210211M',
  #                                 'HFW-210211M HFW-210211T', 'HFW-210211T HFW-210212M', 'HFW-210212T HFW-210213M',
  #                                 'HFW-210213M HFW-210213T', 'HFW-210213T HFW-210214M', 'HFW-210214M HFW-210215M',
  #                                 'HFW-210215M HFW-210215T', 'HFW-210215T HFW-210216M'))# %>%


##calculate the derivative to obtain the rate
asv_tab_pseudoabund_split$asv1 %>%
  colnames()
asv_tab_pseudoabund_split$asv1 %>%
  D(sample_id, pseudoabundance)


## calculate changes in relative abundances
