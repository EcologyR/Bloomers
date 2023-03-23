#import data----
metadata_HF <- read_delim("data-raw/HFW_HFS_metadaata.csv", delim = ";") %>%
  as_tibble()

edit.sample.names <- function(data){
  stopifnot(data = as.tibble(data)),
  stopifnot(sample_id %in% names(data))
            )
  data_ed <- data %>%
    mutate(sample_id = str_replace(sample_id, '_', '-')) %>%
    mutate(sample_id = str_replace(sample_id, 'BL', '')) %>%
    mutate(season = case_when( str_detect(sample_id, 'HFW') ~ 'Winter',
                               str_detect(sample_id, 'HFS') ~ 'Summer')) %>%
    mutate(date_hour = paste0(date, sep = " ", sampling_hour))
  return(data_ed)
}
metadata_HF <- metadata_HF %>%
  edit.sample.names()

asv_tab <- read.table('data-raw/asv_tab_HF_MIAU_filt_test_dataset.txt') %>%
  as_tibble(rownames = 'sample')

sample_codes <- read.table('data-raw/sample_codes_HF_MIAU.txt', header = T) %>%
  as_tibble() %>%
  dplyr::filter(str_detect(Sample_ID, 'HF')) %>%
  separate(Sample_ID, '-*NA', into = c('Sample_id_ed', 'filter')) %>%
  mutate(Sample_id_ed = str_replace(Sample_id_ed, '-D', '')) %>%
  mutate(Sample_id_ed = str_replace(Sample_id_ed, '-R', '')) %>%
  mutate(Sample_id_ed = str_replace(Sample_id_ed, 'BL', ''))
