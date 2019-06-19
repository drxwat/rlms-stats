library(haven)
library(dplyr)
library(stringr)
library(ggplot2)
library(fs)

source("helpers.R")

setwd('~/stats/rlms-stats/')
data_root =  paste0('data/','Полная выборка 09.09.2018') # dafault name after archive extraction

### Searching for waves files in data folder
pattern = regex("(\\d+)-я волна")
data_sources = dir_info(data_root, recurse = TRUE) %>% 
  filter(str_detect(path, 'ИНДИВИДЫ') & type == 'file') %>%
  select(path) %>% rowwise() %>%
  mutate(wave_num = as.integer(str_match(path, pattern)[[2]])) %>%
  arrange(wave_num) %>%
  ungroup()

waves_meta = list(
  list(n = 5, quo(a_born_y), quo(aj10), quo(aj1), quo(aj2cod08), quo(ah5), quo(aj38)),
  list(n = 6, quo(b_born_y), quo(bj10), quo(bj1), quo(bj2cod08), quo(bh5), quo(bj8)),
  list(n = 26, quo(vh6), quo(vj13.2), quo(vj1), quo(vj2cod08), quo(vh5), quo(vj8))
)

for (wave_meta in waves_meta) {
  path = (data_sources %>% filter(wave_num == wave_meta$n))$path
  tmpData = prepareData(
    read_sav(path), wave_meta$n, 
    !!wave_meta[[2]], !!wave_meta[[3]], !!wave_meta[[4]], !!wave_meta[[5]], 
    !!wave_meta[[6]], !!wave_meta[[7]]
    )
  attributes(tmpData$idind) = NULL
  if (exists("data", inherits = FALSE)) {
    tmpData = tmpData %>% select(-c('gender', 'age'))
    colnames(tmpData) = c("idind", paste(colnames(tmpData)[-1], wave_meta$n, sep = "_"))
    data = full_join(data, tmpData, by = 'idind')
  } else {
    data = tmpData
  }
}
# remove(tmpData, wave_meta, data_sources, waves_meta, data_root, path, pattern)
View(data)
