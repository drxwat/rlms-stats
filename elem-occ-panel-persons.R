library(haven)
library(dplyr)
library(stringr)
library(ggplot2)
library(fs)

setwd('~/stats/rlms-stats/')
source("helpers.R")

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
  list(n = 5, yaer = 1994, born = quo(a_born_y), gender = quo(ah5), salary = quo(aj10), 
       has_job = quo(aj1),job_code = quo(aj2cod08), whours = quo(aj8)),
  
  list(n = 6, yaer = 1995, born = quo(b_born_y), gender = quo(bh5), salary = quo(bj10), 
       has_job = quo(bj1), job_code = quo(bj2cod08), whours = quo(bj8)),
  
  list(n = 7, yaer = 1996, born = quo(c_born_y), gender = quo(ch5), salary = quo(cj10), 
       has_job = quo(cj1), job_code = quo(cj2cod08), whours = quo(cj8)),
  
  list(n = 8, yaer = 1998, born = quo(d_born_y), gender = quo(dh5), salary = quo(dj10), 
       has_job = quo(dj1), job_code = quo(dj2cod08), whours = quo(dj8)),
  
  list(n = 9, yaer = 2000, born = quo(e_born_y), gender = quo(eh5), salary = quo(ej10), 
       has_job = quo(ej1), job_code = quo(ej2cod08), whours = quo(ej8)),
  
  list(n = 10, yaer = 2001, born = quo(f_born_y), gender = quo(fh5), salary = quo(fj10), 
       has_job = quo(fj1), job_code = quo(fj2cod08), whours = quo(fj8)),
  
  list(n = 11, yaer = 2002, born = quo(g_born_y), gender = quo(gh5), salary = quo(gj10), 
       has_job = quo(gj1), job_code = quo(gj2cod08), whours = quo(gj8)),
  
  list(n = 12, yaer = 2003, born = quo(h_born_y), gender = quo(hh5), salary = quo(hj10), 
       has_job = quo(hj1), job_code = quo(hj2cod08), whours = quo(hj8)),
  
  list(n = 13, yaer = 2004, born = quo(i_born_y), gender = quo(ih5), salary = quo(ij10), 
       has_job = quo(ij1), job_code = quo(ij2cod08), whours = quo(ij8)),
  
  list(n = 14, yaer = 2005, born = quo(j_born_y), gender = quo(jh5), salary = quo(jj10), 
       has_job = quo(jj1), job_code = quo(jj2cod08), whours = quo(jj8)),
  
  list(n = 15, yaer = 2006, born = quo(k_born_y), gender = quo(kh5), salary = quo(kj10), 
       has_job = quo(kj1), job_code = quo(kj2cod08), whours = quo(kj8)),
  
  list(n = 16, yaer = 2007, born = quo(l_born_y), gender = quo(lh5), salary = quo(lj10), 
       has_job = quo(lj1), job_code = quo(lj2cod08), whours = quo(lj8)),
  
  list(n = 17, yaer = 2008, born = quo(m_born_y), gender = quo(mh5), salary = quo(mj10), 
       has_job = quo(mj1), job_code = quo(mj2cod08), whours = quo(mj8)),
  
  list(n = 18, yaer = 2009, born = quo(n_born_y), gender = quo(nh5), salary = quo(nj10), 
       has_job = quo(nj1), job_code = quo(nj2cod08), whours = quo(nj8)),
  
  list(n = 19, yaer = 2010, born = quo(oh6), gender = quo(oh5), salary = quo(oj10), 
       has_job = quo(oj1), job_code = quo(oj2cod08), whours = quo(oj8)),

  list(n = 20, yaer = 2011, born = quo(ph6), gender = quo(ph5), salary = quo(pj10), 
       has_job = quo(pj1), job_code = quo(pj2cod08), whours = quo(pj8)),
  
  list(n = 21, yaer = 2012, born = quo(qh6), gender = quo(qh5), salary = quo(qj10), 
       has_job = quo(qj1), job_code = quo(qj2cod08), whours = quo(qj8)),
  
  list(n = 22, yaer = 2013, born = quo(rh6), gender = quo(rh5), salary = quo(rj10), 
       has_job = quo(rj1), job_code = quo(rj2cod08), whours = quo(rj8)),
  
  list(n = 23, yaer = 2014, born = quo(sh6), gender = quo(sh5), salary = quo(sj10), 
       has_job = quo(sj1), job_code = quo(sj2cod08), whours = quo(sj8)),
  
  list(n = 24, yaer = 2015, born = quo(th6), gender = quo(th5), salary = quo(tj10), 
       has_job = quo(tj1), job_code = quo(tj2cod08), whours = quo(tj8)),

  list(n = 25, yaer = 2016, born = quo(uh6), gender = quo(uh5), salary = quo(uj10), 
       has_job = quo(uj1), job_code = quo(uj2cod08), whours = quo(uj8)),
  
  list(n = 26, yaer = 2017, born = quo(vh6), gender = quo(vh5),  salary = quo(vj13.2), 
       has_job = quo(vj1), job_code = quo(vj2cod08), whours = quo(vj8))
)

for (wave_meta in waves_meta) {
  path = (data_sources %>% filter(wave_num == wave_meta$n))$path
  tmpData = prepareData(
    read_sav(path), wave_meta$n, wave_meta$yaer,
    !!wave_meta$born, !!wave_meta$gender, !!wave_meta$salary, !!wave_meta$has_job, 
    !!wave_meta$job_code, !!wave_meta$whours
    )
  print(paste0('Processing wave #', wave_meta$n))
  attributes(tmpData$idind) = NULL
  if (exists("data", inherits = FALSE)) {
    #tmpData = tmpData %>% select(-c('born', 'gender'))
    #wave_prefix_cols = paste(tail(colnames(tmpData), ncol(tmpData) - 1), wave_meta$n, sep = "_")
    #colnames(tmpData) = c("idind", wave_prefix_cols)
    #data = full_join(data, tmpData, by = 'idind')
    data = rbind(data, tmpData)
  } else {
    #wave_prefix_cols = paste(tail(colnames(tmpData), ncol(tmpData) - 3), wave_meta$n, sep = "_")
    #colnames(tmpData) = c('idind', 'born', 'gender', wave_prefix_cols)
    data = tmpData
  }
}
remove(tmpData, wave_meta, data_sources, waves_meta, data_root, path, pattern)

#
# ELEMENTARY OCCUPATIONS - PERCENTAGE
#
data = data %>% mutate(age = year - born)

# general data
eloc_data = data %>% filter(has_job == TRUE & isco08major == '9')

# calculating statistics
total_workers = data %>% filter(has_job == TRUE) %>% group_by(year) %>% 
  summarise(workers_total = n())

eloc_workers = eloc_data %>% group_by(year, wave_n) %>% 
  summarise(workers_eloc = n())

eloc_workers_freq = full_join(total_workers, eloc_workers, by = 'year') %>% 
  mutate(eloc_2_total = workers_total / workers_eloc)

# building LM to get coefs
eloc_panel_lm = lm(eloc_2_total ~ wave_n, data = eloc_workers_freq)
eloc_panel_lm_slope = round(summary(eloc_panel_lm)$coefficients[2] * 100, 2)

# percentage plot
eloc_panel_labels = c(
  'Elementary occupation fraction',
  paste0('Linear model. Slope: ', eloc_panel_lm_slope, '%')
  )

ggplot(aes(x = year, y= eloc_2_total), data = eloc_workers_freq) + 
  geom_point(aes(colour = eloc_panel_labels[1])) +
  geom_line(size = 0.75) +
  geom_smooth(method = "lm", aes(colour=eloc_panel_labels[2])) + 
  scale_colour_manual(name = 'Legend',
                      values = c("black", "blue"), 
                      labels = c(eloc_panel_labels[1], eloc_panel_labels[2])) +
  labs(title = 'Elementary occupations percentage. Russia 1994-2017.',
       x = 'Year', y = 'Elementary occupations percentage') +
  scale_x_continuous(breaks = seq(1994, 2017, 1))

#
# ELEMENTARY OCCUPATIONS - AGE
#

# calculating statistics
eloc_age_stats = eloc_data %>% 
  group_by(year, wave_n) %>% summarise(
  mean = mean(age),
  sd = sd(age),
  median = median(age),
  iqr = IQR(age)
)

# building LM to get coefs
age_panel_lm = lm(age ~ wave_n, data = eloc_data)
eloc_age_panel_lm_slope = round(summary(age_panel_lm)$coefficients[2], 2)

eloc_age_panel_labels = c(
  'Elementary occupation mean age',
  paste0('Linear model. Slope: ', eloc_age_panel_lm_slope, '%')
)

ggplot(aes(x = year, y = mean), data = eloc_age_stats) + 
  geom_line(aes(colour = eloc_age_panel_labels[1])) +
  geom_pointrange(aes(ymin=mean - sd, ymax=mean + sd)) +
  geom_smooth(aes(x = year, y = age, colour = eloc_age_panel_labels[2]), data = eloc_data, method = "lm") + 
  ylim(18, 60) + 
  labs(title = 'Elementary occupations mean age with standard deviation. Russia 1994-2017.', 
       x = 'Year', y = 'Mean age (with SD)') +
  scale_colour_manual(name = 'Legend',
                      values = c("black", "blue"), 
                      labels = c(eloc_age_panel_labels[1], eloc_age_panel_labels[2])) +
  scale_x_continuous(breaks = seq(1994, 2017, 1))


#
# ELEMENTARY OCCUPATIONS - WORKING HOURS
#

data_by_y_stats = data %>% 
  filter(has_job == TRUE) %>% 
  group_by(year, wave_n) %>% summarise(
    mean = mean(whours),
    sd = sd(whours),
    median = median(whours),
    iqr = IQR(whours)
  )
  