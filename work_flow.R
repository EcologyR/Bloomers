
source('R/prepare_example_data.R')
source('R/calculate_relative_abundance.R')
source('R/calculate_pseudoabund.R')
#source('R/general_max_sd_abundance.R')
source("R/calculate_rolling_mean_sd.R")
source('R/find_ASVs_high_abund_changes.R')
source('R/compute_bray_curtis_dissimilariy.R')
source('R/community_evenness.R')
source('R/blooming_summary.R')
source('R/get_anomalies.R')
source('R/blooming_summary.R')

##prefixer paquete para saber de donde pertenecen las funciones.

library(tidyverse)
library(vegan)
library(Bloomers)


# PREPROCESSING -----
# get example data
data <- prepare_example_data()
str(data)

# calculate relative abundances
asv_tab_l_rel_abund <- data$asv_tab_l |>
  calculate_rel_abund(group_cols = sample_id)
str(asv_tab_l_rel_abund)

# data for relative abundances
# relatabun <- data$asv_tab_l %>%
#   select(asv_num, reads, sample_id) %>%
#   rename(taxa = asv_num, abund = reads)

# calculate pseudoabundandances
asv_tab_pseudoabund <- asv_tab_l_rel_abund |>
  calculate_pseudoabund(abund_data = data$abund_data,  by_ =  c('sample_id', 'sampling_site'),
                        rel_abund = as.numeric(relative_abundance),
                        total_abund = as.numeric(mean_total_bac))
str(asv_tab_pseudoabund)

# data$abund_data %>%
#   pivot_wider(names_from = sample_id, values_from = relative_abundance)

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
# asv_tab_blooms %>%
#   ggplot(aes(date_hour.x, pseudoabundance))+
#   geom_point()+
#   facet_grid(vars(asv_num))+
#   #geom_line()+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

asv_tab_pseudoabund %>%
  ggplot(aes(date_hour.x, pseudoabundance))+
  geom_point(aes(colour = ))+
  geom_line(aes(group = asv_num))+
  facet_grid(vars(asv_num))+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

##group potential bloomers that behave similarly
#cluster_bloomers <-

##create groups of environmental variables that behave similarly
  ## kmeans
cluster <- env_variables()

##correlate changes in abundances with changes in environmental parameters

# Blooming event effects on the community
## Eveness
# test <- asv_tab_l_rel_abund %>%
#   pivot_wider(values_from = relative_abundance, names_from = sample_id)

community_eveness <- asv_tab_pseudoabund %>%
  select(sample_id, reads, asv_num) %>%
  #subset(sample_id == 'HFW-210211M') %>%
  as_tibble() %>%
  #pivot_wider(values_from = reads, names_from = asv_num)%>%
  group_by(sample_id) %>%
  dplyr::summarize(community_eveness_result = community_evenness(abundances = reads, index = "Pielou"))

#community_evenness(abundances = relative_abundance)

# E <- -sum((abundances/sum(abundances))*log(abundances/sum(abundances)))

# #library(janitor)
# shannon <- asv_tab_pseudoabund %>%
#   select(sample_id, reads, asv_num) %>%
#   #mutate(pseudoabundance = as.numeric(pseudoabundance)) %>%
#   pivot_wider(values_from = reads, names_from = asv_num) %>%
#   column_to_rownames(var = 'sample_id') %>%
#   #select(pseudoabundance) %>%
#   vegan::diversity()
#
# asv_tab_pseudoabund %>%
#   select(sample_id, reads, asv_num) %>%
#   subset(sample_id == 'HFW-210209M') %>%
#   pivot_wider(values_from = reads, names_from = asv_num) %>%
#   column_to_rownames(var = 'sample_id') %T>%
#   #select(pseudoabundance) %>%
#   vegan::specnumber() %>%
#   vegan::diversity()
#
# spec_num <- asv_tab_pseudoabund %>%
#   select(sample_id, reads, asv_num) %>%
#   #mutate(pseudoabundance = as.numeric(pseudoabundance)) %>%
#   pivot_wider(values_from = reads, names_from = asv_num) %>%
#   column_to_rownames(var = 'sample_id') %>%
#   #select(pseudoabundance) %>%
#   vegan::specnumber()
#
# cbind(shannon, spec_num) %>%
#   as_tibble() %>%
#   mutate(eveness = temp/log(temp_spec))
#
#
# community_eveness_vegan <- function(){
#
# }

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
    as_tibble() %>%
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
community_eveness |>
  left_join(bray_curtis_results) %>%
  left_join(metadata, by = c('samples' = 'sample_id')) %>%
  mutate(date_hour = (as.POSIXct(date_hour, format = "%d/%m/%y %H:%M:%S"))) %>%
  ungroup() %>%
  pivot_longer(cols = c('community_eveness_result', 'bray_curtis_result')) |>
  ggplot(aes(date_hour, value))+
  geom_point()+
  geom_line()+
  facet_wrap(vars(name), scales = 'free')+
  labs(y = 'Diveristy', x = 'Samples')+
  scale_x_datetime()+
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

##anomalies
bray_curtis_dissimilarity <- bray_curtis_results %>%
  as_tibble() %>%
  select(bray_curtis_result) %>%
  unlist()

str(bray_curtis_dissimilarity)

get_anomalies(values = bray_curtis_dissimilarity, time_lag = 3, negative = TRUE, plotting = TRUE)

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
# asv_tab_pseudoabund_split$asv1 %>%
#   colnames()
# asv_tab_pseudoabund_split$asv1 %>%
#   D(sample_id, pseudoabundance)

## calculate changes in relative abundances

## blooming event duration
# asv1 <- asv_tab_pseudoabund %>%
#   subset(asv_num == 'asv1') %>%
#   select(asv_num, relative_abundance) %>%
#   dplyr::mutate(row_index := row_number()) %>%
#   ggplot(aes(row_index, relative_abundance))+
#   geom_point()
#
# asv1 %>%
#   dplyr::select(asv_num, relative_abundance) %>%
#   dplyr::mutate(row_index := row_number()) %>%
#   dplyr::filter(row_index >= 8) %>% #anomaly in point 4
#   dplyr::mutate(mantaining_bloom = case_when(between(relative_abundance, 0.052-0.015, 0.052+0.045) ~ 'TRUE',
#                                            #relative_abundance <  ~ 'TRUE',
#                                            .default  = 'FALSE')) %>%
#   dplyr::mutate(binomial_bloom = case_when(mantaining_bloom == 'TRUE' ~ 1,
#                                               mantaining_bloom == 'FALSE' ~ 0)) %>%
#   dplyr::mutate(anormal_abundance_points = sum(binomial_bloom)) %>%
#   group_by(asv_num, grp = with(rle(binomial_bloom), rep(seq_along(lengths), lengths))) %>%
#   mutate(consecutive_bloom = 1:n()) %>%
#   ungroup() %>%
#   dplyr::filter(grp == 1) %>%
#   select(-grp, -binomial_bloom, -mantaining_bloom, -row_index) %>%
#   slice_max(consecutive_bloom, n = 1)

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

# abundance <- runif(16, 0, 2000)
# #create an anomaly
# abundance[8:12] <- runif(5, 3000, 4000)
# #test it
# get_anomalies(abundance)
#
# blooming_summary <- function(data, anomaly_point, relative_abundance,  range_percentage){
#
#   relative_abundance_anomaly <- data %>%
#     dplyr::mutate(row_index := row_number()) %>%
#     dplyr::filter(row_index == anomaly_point) %>%
#     select({{relative_abundance}}) %>%
#     as.numeric()
#
#   perc <-   relative_abundance_anomaly*range_percentage
#
#   data_blooming_maintained  <- data %>%
#     dplyr::mutate(row_index := row_number()) %>%
#     dplyr::filter(row_index >= anomaly_point) %>%
#     dplyr::mutate(mantaining_bloom = case_when(between({{relative_abundance}}, relative_abundance_anomaly-perc, relative_abundance_anomaly+perc) ~ 'TRUE',
#                                                .default  = 'FALSE')) %>%
#     dplyr::mutate(binomial_bloom = case_when(mantaining_bloom == 'TRUE' ~ 1,
#                                                 mantaining_bloom == 'FALSE' ~ 0)) %>%
#     dplyr::mutate(anormal_abundance_points = sum(binomial_bloom)) %>%
#     group_by(grp = with(rle(binomial_bloom), rep(seq_along(lengths), lengths))) %>% #asv_num,
#     mutate(consecutive_bloom = 1:n()) %>%
#     ungroup() %>%
#     dplyr::filter(grp == 1) %>%
#     select(-grp, -binomial_bloom, -mantaining_bloom, -row_index, -{{relative_abundance}}) %>%
#     slice_max(consecutive_bloom, n = 1) %>%
#     cbind(relative_abundance_anomaly)
# return(data_blooming_maintained)
# }

#blooming_summary(data = asv1, anomaly_point = 8, relative_abundance = relative_abundance, range_percentage = 0.6)

# abundance <- abundance %>%
#   as_vector()
#
# blooming_summary(values = abundance, anomaly_point = 8, z_vector = NULL, range_percentage = 30) # primer prova amb un vector
#
# 3023+ 3023*0.2
# 3023- 3023*0.4
# blooming_summary(values = )
#
# between(abundance$value, 3023-3023*0.2, 3023+3023*0.2)
#
# blooming_summary()
# ##trying to apply blooming summary a todo el dataset a la vez
# tenim dos camins per arribar al blooming summary:
# library(tidyverse)
# filt <- z |>
#   keep(~ any(!is.na(.x))) |>
#   dplyr::filter(anomalies_ab != 'FALSE' &
#                   anomalies_ra != 'FALSE')
#
#
#
# z_tib <- z |>
#   as_tibble()
#
#
# duplicate_rows_ra <- z_tib |>
#   dplyr::filter(length(z_tib) >= 1) |>
#   #dplyr::mutate(anomalies_ra = as.character(anomalies_ra)) |>
#   unnest(anomalies_ra) |>
#   dplyr::select(-anomalies_ab)
#
# duplicate_rows_ab <- z_tib |>
#   dplyr::filter(length(z_tib) >= 1) |>
#   #dplyr::mutate(anomalies_ab = as.character(anomalies_ab)) |>
#   unnest(c(anomalies_ab)) |>
#   dplyr::select(-anomalies_ra)
#
# duplicated_rows <- duplicate_rows_ra |>
#   full_join(duplicate_rows_ab, by = 'asv_num', relationship = 'many-to-many')
#
# # duplicated_rows <- duplicate_rows_ra |>
# #   bind_cols(duplicate_rows_ab)
#
# result <- bind_rows(unique_rows_ra, duplicated_rows, unique_rows_ab)
# bind_rows(duplicate_rows_ra, duplicate_rows_ab)

##Summarize blooming event with the list of anomalies ----
## it is more difficult because I need to recover the list of abundances from each ASV
anom_points_tibble_ra <- asv_tab_pseudoabund %>%
  as_tibble() |>
  group_by(asv_num) |>
  dplyr::reframe(anomalies_ra = get_anomalies(values = relative_abundance, plotting = FALSE)[2]) |>
  unnest(anomalies_ra) |>
  group_by(asv_num) |>
  dplyr::mutate(sample_num = row_number())

anom_points_tibble_ab <- asv_tab_pseudoabund %>%
  as_tibble() |>
  group_by(asv_num) |>
  dplyr::reframe(anomalies_ab = get_anomalies(values = pseudoabundance, plotting = FALSE)[2]) |>
  unnest(anomalies_ab) |>
  group_by(asv_num) |>
  dplyr::mutate(sample_num = row_number())

anomalies_summary_ra <- asv_tab_pseudoabund %>%
  dplyr::select(sample_id, relative_abundance, asv_num) |>
  as_tibble() |>
  dplyr::filter(asv_num != 'asv3') |>
  left_join(anom_points_tibble_ra, by = 'asv_num', relationship = 'many-to-many')  |>
  #group_by(sample_id, pseudoabundance, asv_num) |>
  distinct(sample_id, relative_abundance, asv_num, .keep_all = TRUE) |>
  group_by(asv_num) |>
  dplyr::reframe(summary =
      blooming_summary(values = relative_abundance,
                       anomaly_point = anomalies_ra,
                       z_vector = NULL,
                       range_percentage = 30)) |>
  ungroup() |>
  #distinct(asv_num, summary$anomaly_time_point, .keep_all = TRUE)
  distinct(across(everything()))

anomalies_summary_ab <- asv_tab_pseudoabund %>%
  dplyr::select(sample_id, pseudoabundance, asv_num) |>
  as_tibble() |>
  dplyr::filter(asv_num != 'asv3') |>
  left_join(anom_points_tibble_ra, by = 'asv_num', relationship = 'many-to-many')  |>
  #group_by(sample_id, pseudoabundance, asv_num) |>
  distinct(sample_id, pseudoabundance, asv_num, .keep_all = TRUE) |>
  group_by(asv_num) |>
  dplyr::reframe(summary =
                   blooming_summary(values = pseudoabundance,
                                    anomaly_point = anomalies_ra,
                                    z_vector = NULL,
                                    range_percentage = 30)) |>
  ungroup() |>
  #distinct(asv_num, summary$anomaly_time_point, .keep_all = TRUE)
  distinct(across(everything()))

## Event summary using z-scores for all ASVs and anomalies at the same time----
### transform the list from get_anomalies to a tibble
z_scores_tibble <- asv_tab_pseudoabund %>%
  as_tibble() |>
  group_by(asv_num) |>
  dplyr::reframe(anomalies_ab = get_anomalies(values = pseudoabundance, plotting = FALSE)[c(3)],
                 anomalies_ra = get_anomalies(values = relative_abundance, plotting = FALSE)[3]) |>
  unnest(c(anomalies_ra, anomalies_ab)) |>
  group_by(asv_num) |>
  dplyr::mutate(sample_num = row_number())

### summarize pseudoabundance anomalies
summary_z_ab <- z_values %>%
  as_tibble() |>
  group_by(asv_num) |>
  dplyr::filter(any(anomalies_ab > 2)) |> # we need to filter those ASVs that do not have any anomaly time point.
  group_by(asv_num) |>
  dplyr::reframe(summary_ab =
                   blooming_summary(cutoff = 2,
                                    values = sample_num,
                                    #anomaly_point = anomalies_ra,
                                    z_vector = anomalies_ab,
                                    range_percentage = 30)) |>
  ungroup()

### summarize relative_abundance anomalies
summary_z_ra <- z_values %>%
  as_tibble() |>
  group_by(asv_num) |>
  dplyr::filter(any(anomalies_ra > 2)) |> # we need to filter those ASVs that do not have any anomaly time point.
  group_by(asv_num) |>
  dplyr::reframe(summary_ra =
                   blooming_summary(cutoff = 2,
                                    values = sample_num,
                                    #anomaly_point = anomalies_ra,
                                    z_vector = anomalies_ra,
                                    range_percentage = 30)) |>
  ungroup()

## Crear un link entre la funció de GET ANOMALIES i la següent funció que seria obtenir una llista per poder filtrar el meu dataset per aquestes ASVs----

z_diversity <- bray_curtis_results |>
  dplyr::right_join(community_eveness) |>
  #ungroup() %>%
  #group_by(sample_id) %>%
  dplyr::reframe(anomalies_bray = get_anomalies_ed(values = bray_curtis_result, plotting = FALSE, na_rm = TRUE)[c(1,2)])

community_eveness |>
  colnames()

z_diversity <- bray_curtis_results |>
  dplyr::right_join(community_eveness) |> ##el problema és que no troba anomalies crec!
  #ungroup() %>%
  #group_by(sample_id) %>%
  dplyr::reframe(anomalies_eveness = get_anomalies_ed(values = community_eveness_result, plotting = TRUE, na_rm = TRUE, negative = FALSE)[c(1,2)])

z_diversity %>%
  str()
z_diversity$anomalies_eveness[2]

##si faig el mateix amb les dades d'ASVs què surt:
### 1 TURE/FALSE anomaly
### 2 anomaly position
### 3 z-score values
z <- asv_tab_pseudoabund %>%
  as_tibble() |>
  group_by(asv_num) |>
  dplyr::reframe(anomalies_ab = get_anomalies(values = pseudoabundance, plotting = FALSE)[c(1,2,3)],
                   anomalies_ra = get_anomalies(values = relative_abundance, plotting = FALSE)[c(1,2,3)])


z[[1]]
z$asv_num
z$anomalies_ab
z$anomalies_ra

# asv_potential_bloomers <- z |>
#   dplyr::filter(anomalies_ab == 'TRUE' &
#                   anomalies_ra == 'TRUE') |>
#   dplyr::select(asv_num) |>
#   as_vector()
#
# asv_tab_pseudoabund |>
#   dplyr::filter(asv_num %in% asv_potential_bloomers)
#
# test <- unlist(z) |>
#   as_tibble()
# z |>
#   class()

##create a vector to filter asv_tab for those ASVs that have anomalies in the different parammeters calculated (i.e relative_abundances or pseudoabundances)
# find_asv_with_anomalies <- function(anomalies_result, anomaly_in1, anomaly_in2, logic1 = TRUE, logic2 = TRUE, asv_col = asv_num){
#   # if(is.list(z) == FALSE){
#   #   stop("Function stopped: anomalies_result needs to be a list form the get_anomalies function")
#   # }
#   # if(is.logical({{anomaly_in1}}) == FALSE){
#   #   stop("Function stopped: anomaly_in1 needs to be logical (TRUE/FALSE)")
#   # }
#   #
#   asv_potential_bloomers <-
#     anomalies_result |>
#     dplyr::filter({{anomaly_in1}} %in% logic1 &
#                     {{anomaly_in2}} %in% logic2) |>
#     dplyr::select({{asv_col}}) |>
#     as_vector()
#   return(asv_potential_bloomers)
# }
#
# find_asv_with_anomalies(anomalies_result = z, anomaly_in1 = anomalies_ab, anomaly_in2 = anomalies_ra, logic1 = TRUE, logic2 = TRUE, asv_col = asv_num)


# do the different anomalies calculation meet in time (next function) ---- en principi ja tinc una altra funció que fa això la de filter anterior
# test_tibble <- tibble(x = 1:16) %>%
#   mutate(anomaly_rel_abund = c('FALSE', 'TRUE', 'TRUE', 'FALSE', 'FALSE', 'TRUE', 'TRUE', 'FALSE',
#                                'FALSE', 'FALSE', 'FALSE', 'TRUE', 'TRUE', 'FALSE', 'FALSE', 'TRUE'),
#          anomaly_pseudoabund = c('FALSE', 'FALSE', 'FALSE', 'TRUE', 'TRUE', 'FALSE', 'FALSE', 'TRUE',
#                                  'FALSE', 'TRUE', 'TRUE', 'FALSE', 'FALSE', 'TRUE', 'TRUE', 'FALSE'),
#          anomaly_eveness = c('FALSE', 'TRUE', 'TRUE','FALSE', 'TRUE', 'TRUE', 'TRUE', 'FALSE',
#                              'FALSE', 'FALSE', 'FALSE', 'TRUE', 'TRUE', 'FALSE', 'FALSE', 'TRUE'))
#
# test_tibble %>%
#   dplyr::mutate(anomaly_rel_abund = as.logical(anomaly_rel_abund),
#          anomaly_pseudoabund = as.logical(anomaly_pseudoabund),
#          anomaly_eveness = as.logical(anomaly_eveness)) %>%
#   pivot_longer(cols = starts_with('anomaly')) |>
#   dplyr::mutate(true_numeric = case_when(value == TRUE ~ 1,
#                                           value == FALSE ~ 0 )) |>
#   group_by(as.character(x)) |>
#   dplyr::summarize(true = sum(true_numeric)) |>
#   dplyr::filter(true >= 2)

  #group_by(asv_num)
 #%>%
  #dplyr::mutate()


