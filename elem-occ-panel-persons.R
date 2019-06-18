library(haven)
library(dplyr)
library(ggplot2)


setwd('~/stats/rlms-stats/')
source("helpers.R")

r05iall26c = read_sav("data/Полная выборка 09.09.2018/5-я волна/ИНДИВИДЫ/r05iall26c.sav")


data = prepareData(r05iall26c, 5, a_born_y, aj10, aj1, aj2cod08, ah5)
data

ggplot(data, aes(salary)) + geom_histogram()
