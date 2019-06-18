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

data_sources

# r05iall26c = read_sav("data/Полная выборка 09.09.2018/5-я волна/ИНДИВИДЫ/r05iall26c.sav")
# data = prepareData(r05iall26c, 5, a_born_y, aj10, aj1, aj2cod08, ah5)

