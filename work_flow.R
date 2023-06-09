
source('R/prepare_example_data.R')
source('R/calculate_relative_abundance.R')
source('R/calculate_pseudoabund.R')
source('R/general_max_sd_abundance.R')
source("R/calculate_rolling_mean_sd.R")
source('R/find_ASVs_high_abund_changes.R')
source('R/compute_bray_curtis_dissimilariy.R')
source('R/community_evenness.R')
source('R/blooming_event_summary.R')
source('R/get_anomalies.R')

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

# data for relative abundances
relatabun <- data$asv_tab_l %>% select(asv_num, reads, sample_id) %>% rename(taxa = asv_num, abund = reads)

# calculate pseudoabundandances
asv_tab_pseudoabund <- asv_tab_l_rel_abund %>%
  calculate_pseudoabund(abund_data = data$abund_data,  by_ =  c('sample_id', 'sampling_site'),
                        rel_abund = as.numeric(relative_abundance),
                        total_abund = as.numeric(mean_total_bac))
str(asv_tab_pseudoabund)

data$abund_data %>%
  pivot_wider(names_from = sample_id, values_from = relative_abundance)

# IDENTIFICATION -----

#general_max_sd_abundance(asv_tab_pseudoabund, group_var = asv_num, abundance_col = pseudoabundance, x = 5)

# Calculate for each ASV the rolling mean and sd with the previous values for the abundance to detect highest changes
# asv_tab_pseudoabund_roll_mean_sd <- rolling_mean_and_sd(
#   df = asv_tab_pseudoabund,
#   abundance_column = pseudoabundance,
#   group_var = asv_num,
#   group_size = 3) %>%
#   as_tibble()

# Create a vector with the ASVs that present the highest changes in relative abundances
# high_abund_changes_asvs <- ASVs_high_abund_changes(data = asv_tab_pseudoabund_roll_mean_sd,
#                                                    asv_col = asv_num,
#                                                    high_change = 50000)

##filter ASV_tab by those ASVs that have high changes in abundances
# asv_tab_blooms <- asv_tab_pseudoabund_roll_mean_sd %>%
#   dplyr::filter(asv_num %in% high_abund_changes_asvs)

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
test <- asv_tab_l_rel_abund %>%
  pivot_wider(values_from = relative_abundance, names_from = sample_id)

test
community_evenness(abundances = relative_abundance)


## Bray Curtis dissimilarity function
# asv_tab_l_rel_abund %>%
#   colnames()

bray_curtis_results <- dissimilarity_matrix(data = asv_tab_l_rel_abund, sample_id_col = sample_id)


#transform function for a vector as input
dissimilarity_matrix <- function(data, sample_id_col) {

  # Extract rownames to mantain them at the output table
  sample_id_unique <- data %>%
    group_by({{sample_id_col}}) %>% ##sample id that identifies uniquely each sample
    dplyr::arrange(.by_group = TRUE)  %>% ## reorder so that it is ordered equally
    dplyr::distinct({{sample_id_col}})

  # Index samples to filter for only consecutive comparisons
  # check name of the columns
  #if ({{sample_id_col}} %in% colnames(data)==FALSE) {stop("There is no sample_id_col column in your data tibble")}

  samples_index <- data %>%
    dplyr::group_by({{sample_id_col}}) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    dplyr::select({{sample_id_col}}) %>%
    dplyr::distinct({{sample_id_col}}) %>%
    as_tibble() %>%
    dplyr::mutate('row_index_2' := dplyr::row_number()) %>%
    dplyr::select({{sample_id_col}}, row_index_2)

  # Compute pairwise Bray-Curtis distances between all rows of data

  dissim_mat <- data %>%
    pivot_wider(id_cols = {{sample_id_col}},  names_from = asv_num, values_from = relative_abundance) %>%
    group_by({{sample_id_col}}) %>%
    dplyr::arrange(.by_group = TRUE) %>%
    column_to_rownames('sample_id') %>%
    vegan::vegdist(method = 'bray', upper = T) %>%
    as.matrix() %>%
    as_data_frame() %>%
    cbind(sample_id_unique) %>%
    dplyr::mutate(row_index := row_number()) %>%
    rowid_to_column() %>%
    as_tibble() %>%
    pivot_longer(cols = starts_with('HFW'), values_to = 'bray_curtis_result', names_to = 'samples') %>%
    left_join(samples_index, by = c('samples' = 'sample_id')) %>%
    dplyr::filter(row_index == (row_index_2-1))


  # # Chech that diagonal elements to zero (i.e., each sample is identical to itself)
  # diag(dissim_mat) <- 0

  # Return the dissimilarity matrix
  return(dissim_mat)
}


### recover metadata for plotting
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

##anomalies
bray_curtis_dissimilarity <- bray_curtis_results %>%
  as_tibble() %>%
  select(bray_curtis_result) %>%
  unlist()

str(bray_curtis_dissimilarity)

get_anomalies(values = bray_curtis_dissimilarity, time_lag = 2, negative = FALSE, plotting = TRUE)


º#create a vector with sample names to not lose them
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

## blooming event duration
asv_tab_pseudoabund %>%
  subset(asv_num == 'asv1') %>%
  select(asv_num, relative_abundance) %>%
  dplyr::mutate(row_index := row_number()) %>%
  ggplot(aes(row_index, relative_abundance))+
  geom_point()

asv1 %>%
  select(asv_num, relative_abundance) %>%
  dplyr::mutate(row_index := row_number()) %>%
  dplyr::filter(row_index >= 8) %>% #anomaly in point 4
  dplyr::mutate(mantaining_bloom = case_when(between(relative_abundance, 0.052-0.015, 0.052+0.045) ~ 'TRUE',
                                           #relative_abundance <  ~ 'TRUE',
                                           .default  = 'FALSE')) %>%
  dplyr::mutate(binomial_bloom = case_when(mantaining_bloom == 'TRUE' ~ 1,
                                              mantaining_bloom == 'FALSE' ~ 0)) %>%
  dplyr::mutate(anormal_abundance_points = sum(binomial_bloom)) %>%
  group_by(asv_num, grp = with(rle(binomial_bloom), rep(seq_along(lengths), lengths))) %>%
  mutate(consecutive_bloom = 1:n()) %>%
  ungroup() %>%
  dplyr::filter(grp == 1) %>%
  select(-grp, -binomial_bloom, -mantaining_bloom, -row_index) %>%
  slice_max(consecutive_bloom, n = 1)

# dplyr::mutate(mantainance = sum(which(consecutive_bloom>0 )))
  # dplyr::mutate(mantainance = sum(lag(consecutive_bloom, 4)))
  #
  #                 case_when(lag(consecutive_bloom, 1) != 0 ~ sum(lag(consecutive_bloom, 3)),
  #                                       lag(consecutive_bloom, 1) == 0 ~ 'FALSE'))

#which(consecutive_bloom, equal)
# %$%
#   mantaining_bloom %$%
#   is.logical() %>%
#   summary()['TRUE']

# asv1 <-  %>%
#   select(asv_num, relative_abundance)

#x*0.01

##input dataframe a subset for each ASV

abundance <- runif(16, 0, 2000)
#create an anomaly
abundance[8:12] <- runif(5, 3000, 4000)
#test it
get_anomalies(abundance)

blooming_summary <- function(data, anomaly_point, relative_abundance,  range_percentage){

  relative_abundance_anomaly <- data %>%
    dplyr::mutate(row_index := row_number()) %>%
    dplyr::filter(row_index == anomaly_point) %>%
    select({{relative_abundance}}) %>%
    as.numeric()

  perc <-   relative_abundance_anomaly*range_percentage

  data_blooming_maintained  <- data %>%
    dplyr::mutate(row_index := row_number()) %>%
    dplyr::filter(row_index >= anomaly_point) %>%
    dplyr::mutate(mantaining_bloom = case_when(between({{relative_abundance}}, relative_abundance_anomaly-perc, relative_abundance_anomaly+perc) ~ 'TRUE',
                                               .default  = 'FALSE')) %>%
    dplyr::mutate(binomial_bloom = case_when(mantaining_bloom == 'TRUE' ~ 1,
                                                mantaining_bloom == 'FALSE' ~ 0)) %>%
    dplyr::mutate(anormal_abundance_points = sum(binomial_bloom)) %>%
    group_by(grp = with(rle(binomial_bloom), rep(seq_along(lengths), lengths))) %>% #asv_num,
    mutate(consecutive_bloom = 1:n()) %>%
    ungroup() %>%
    dplyr::filter(grp == 1) %>%
    select(-grp, -binomial_bloom, -mantaining_bloom, -row_index, -{{relative_abundance}}) %>%
    slice_max(consecutive_bloom, n = 1) %>%
    cbind(relative_abundance_anomaly)
return(data_blooming_maintained)
}

#blooming_summary(data = asv1, anomaly_point = 8, relative_abundance = relative_abundance, range_percentage = 0.6)

abundance <- abundance %>%
  as_tibble()

blooming_summary(data = abundance, anomaly_point = 8, relative_abundance = value, range_percentage = 0.3)

3023+ 3023*0.2
3023- 3023*0.4

between(abundance$value, 3023-3023*0.2, 3023+3023*0.2)
