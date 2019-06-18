library(haven)
library(dplyr)
library(ggplot2)

### Don't forget to change directory to this script folder
# setwd('~/stats/rlms-stats/')

## TODO: Set WD to this script and use relative path
source("helpers.R")

# ISCO-08 skill level to job code mapping
job2skill_level = list()
# Managers
job2skill_level['11'] = '4'
job2skill_level['12'] = '4'
job2skill_level['13'] = '4'
job2skill_level['14'] = '3'
# Armed forces occupations
job2skill_level['01'] = '4'
job2skill_level['02'] = '2'
job2skill_level['03'] = '1'

job2skill_level['2'] = '4' # Professionals
job2skill_level['3'] = '3' # Technicans and associate professionals
job2skill_level['4'] = '2' # Clerical support workers
job2skill_level['5'] = '2' # Services and sales workers
job2skill_level['6'] = '2' # Skilled agricultural, foresty and fishery workers
job2skill_level['7'] = '2' # Craft and related trade workers
job2skill_level['8'] = '2' # Plant and machine operators and assemblers
job2skill_level['9'] = '1' # Elementary occupations

# https://www.hse.ru/data/2018/09/09/1155096846/r26iall26b.sav
data = read_sav("data/r26iall26b.sav")

# Adults that have job or on vacation
data = data %>% mutate(age = 2017 - vh6) %>% filter(
  age > 17 & 
  (vj1 == 1 | vj1 == 2 | vj1 == 3 | vj1 == 4) & # 1 - has job, 5 - no job
  !is.na(vj2cod08)
)
# use ungroup to disable rowwise
data$salary = as.integer(data$vj13.2);
data = data %>% rowwise() %>% mutate(
  isco08code = toString(vj2cod08),
  isco08major = substring(isco08code, 1, 1),
  salary = ifelse(salary == 99999997 | salary == 99999998 | salary == 99999999, NA, salary)
) %>% ungroup()

data$gender = factor(data$vh5, levels = c(1, 2), c('Male', 'Female'))

# Common color palette
getPalette = colorRampPalette(RColorBrewer::brewer.pal(8, "Set3"))

### Elementary occupations

elementary_occ = (data %>% filter(isco08major == '9'))
table(elementary_occ$isco08code)
prop.table(table(elementary_occ$isco08code))

elementary_occ$isco08code_factor = factor(
  elementary_occ$isco08code, 
  levels = c('9112', '9122', '9211', '9212', '9215', '9313', '9321', '9333', '9412', '9613', '9621', '9629'),
  labels = c(
    'Cleaners and Helpers in Offices, Hotels and Other Establishments',
    'Vehicle Cleaners',
    'Crop Farm Labourers',
    'Livestock Farm Labourers',
    'Forestry Labourers',
    'Building Construction Labourers',
    'Hand Packers',
    'Freight Handlers',
    'Kitchen Helpers',
    'Sweepers and Related Labourers',
    'Messengers, Package Deliverers and Luggage Porters',
    'Elementary Workers Not Elsewhere Classified'
    )
)
elementary_occ$isco08code_factor = replaceNaWithNamedFactor(elementary_occ$isco08code_factor, 'Other')

### General distribution of jobs ###
elementary_occ_freq = elementary_occ %>% select(isco08code_factor) %>% 
  group_by(isco08code_factor) %>% 
  summarise(n = n()) %>%
  mutate(freq=n/sum(n)*100.0) %>%
  arrange(desc(n))

el_occupations_labels = ifelse(round(elementary_occ_freq$freq) < 3, '', paste0(round(elementary_occ_freq$freq), '%'))
ggplot(elementary_occ_freq, aes(x = "", y = freq, fill = isco08code_factor)) + 
  geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
  geom_text(aes(label = el_occupations_labels), 
            position = position_stack(vjust = 0.5)) +
  labs(fill="Occupation ISCO-08", 
       x=NULL, 
       y=NULL, 
       fill = NULL,
       title="Elementary occupations. General distribution - Russia 2017", 
       caption="Source: rlms26") + 
  coord_polar("y") +
  scale_fill_manual(values = getPalette(length(unique(elementary_occ$isco08code_factor))))

### Distribution by gender ###

elementary_occ_gender = elementary_occ %>% 
  select(gender, isco08code_factor) %>% 
  group_by(isco08code_factor, gender) %>%
  summarise(n = n()) %>% 
  mutate(freq=n/nrow(elementary_occ)*100.0)

el_occupations_gender_labels = ifelse(round(elementary_occ_gender$freq) < 1, '', paste0(round(elementary_occ_gender$freq), '%'))
ggplot(elementary_occ_gender, aes(isco08code_factor, freq)) + 
  geom_bar(aes(fill = gender), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") +
  labs(fill="Gender", 
       x=NULL, 
       y=NULL, 
       fill = NULL,
       title="Elementary occupations. Gender distribution - Russia 2017", 
       caption="Source: rlms26") +
  geom_text(aes(label = el_occupations_gender_labels), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2")

### USSR republic birthplace ###

elementary_occ$birthplace = factor(
  elementary_occ$vi2, 
  levels = c(1:16),
  labels = c(
    'Russia', 'Ukraine', 'Belarus', 'Azerbaijan', 'Armenia', 'Georgia', 
    'Kazakhstan', 'Kyrgyzstan', 'Latvia', 'Lithuania', 'Moldova', 'Tajikistan',
    'Turkmenistan', 'Uzbekistan', 'Estonia', 'Other Country'
    )
  )
elementary_occ$birthplace = replaceNaWithNamedFactor(elementary_occ$birthplace, 'Declined to Respond')

elementary_occ_birth = elementary_occ %>% 
  select(birthplace, isco08code_factor) %>% 
  group_by(isco08code_factor, birthplace) %>%
  summarise(n = n()) %>% 
  mutate(freq=n/nrow(elementary_occ)*100.0)

el_occupations_birth_labels = ifelse(round(elementary_occ_birth$freq) < 2, '', paste0(round(elementary_occ_birth$freq), '%'))
ggplot(elementary_occ_birth, aes(isco08code_factor, freq)) + 
  geom_bar(aes(fill = birthplace), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") +
  labs(fill="USSR Republic Bithplace", 
       x=NULL, 
       y=NULL, 
       fill = NULL,
       title="Elementary occupations. Birthplace distribution - Russia 2017", 
       caption="Source: rlms26") +
  geom_text(aes(label = el_occupations_birth_labels), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = getPalette(length(unique(elementary_occ$birthplace))))


### NATIONALUTY ###
elementary_occ = elementary_occ %>% mutate(nationality = if_else(vi4 == 1, 'Russian', 'Other'))
elementary_occ$nationality = as.factor(elementary_occ$nationality)

elementary_occ_nat = elementary_occ %>% 
  select(nationality, isco08code_factor) %>% 
  group_by(isco08code_factor, nationality) %>%
  summarise(n = n()) %>% 
  mutate(freq=n/nrow(elementary_occ)*100.0)

el_occupations_nat_labels = ifelse(round(elementary_occ_nat$freq) < 1, '', paste0(round(elementary_occ_nat$freq), '%'))
ggplot(elementary_occ_nat, aes(isco08code_factor, freq)) + 
  geom_bar(aes(fill = nationality), position = position_stack(reverse = TRUE), stat = "identity") +
  coord_flip() +
  theme(legend.position = "top") +
  labs(fill="nationality", 
       x=NULL, 
       y=NULL, 
       fill = NULL,
       title="Elementary occupations. Nationality distribution - Russia 2017", 
       caption="Source: rlms26") +
  geom_text(aes(label = el_occupations_nat_labels), 
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette="Dark2")

### SALARY 

ggplot(elementary_occ, aes(x = salary)) + 
  geom_histogram(color="#E69F00", fill="#E69F00", alpha=0.5) +
  labs(x = "Salary in Rubles", y = NULL, 
       title="Elementary occupations. Salary distribution - Russia 2017",
       caption="Source: rlms26"
  ) +
  geom_vline(
    aes(xintercept = median(na.omit(elementary_occ$salary)), color="median"), 
    linetype = "longdash"
    ) + 
  geom_vline(
    aes(xintercept = mean(na.omit(elementary_occ$salary)), color="mean"), 
    linetype = "longdash"
    ) + 
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "blue"))

elementary_occ_salary_dev_sd = sd(elementary_occ$salary, na.rm = TRUE)
elementary_occ_salary_dev = elementary_occ %>% 
  mutate(
    salary_deviation = (salary - mean(salary, na.rm = TRUE))/sd(salary, na.rm = TRUE),
  ) %>% 
  group_by(isco08code_factor) %>% 
  summarise(
    mean_salary_deviation = mean(salary_deviation, na.rm = TRUE),
    mean_salary_deviation_type = ifelse(mean_salary_deviation < 0, "below", "above")
  ) %>% 
  arrange(mean_salary_deviation)
# reseting ordered factor to get order on plot
elementary_occ_salary_dev$isco08code_factor = factor(
  elementary_occ_salary_dev$isco08code_factor,
  levels = elementary_occ_salary_dev$isco08code_factor
  )

ggplot(elementary_occ_salary_dev, aes(x=isco08code_factor, y=mean_salary_deviation, label=mean_salary_deviation)) + 
  geom_bar(stat='identity', aes(fill=mean_salary_deviation_type), width=.5)  +
  scale_fill_manual(name="Salary", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(x = NULL,
       y = NULL, 
       title="Elementary occupations. Mean normalized salary divergence - Russia 2017",
       subtitle = paste0(
         "Amount of SDs (", 
         round(sd(elementary_occ$salary, na.rm = TRUE)), 
         " RUB) divergence from mean salary in elementary occupations (", 
         round(mean(elementary_occ$salary, na.rm = TRUE)), " RUB)."),
       caption="Source: rlms26"
  ) +
  geom_text(aes(label = round(elementary_occ_salary_dev$mean_salary_deviation, 2)), 
            position = position_stack(vjust = 0.5)) + 
  coord_flip()

### JOB SPHERE
industry_labels =tolower(c(
  'ЛЕГКАЯ, ПИЩЕВАЯ ПРОМЫШЛЕННОСТЬ', 'ГРАЖДАНСКОЕ МАШИНОСТРОЕНИЕ', 'ВОЕННО-ПРОМЫШЛЕННЫЙ КОМПЛЕКС',
  'НЕФТЕГАЗОВАЯ ПРОМЫШЛЕННОСТЬ', 'ДРУГАЯ ОТРАСЛЬ ТЯЖЕЛОЙ ПРОМЫШЛЕННОСТИ', 'СТРОИТЕЛЬСТВО',
  'ТРАНСПОРТ, СВЯЗЬ', 'СЕЛЬСКОЕ ХОЗЯЙСТВО', 'ОРГАНЫ УПРАВЛЕНИЯ', 'ОБРАЗОВАНИЕ', 'НАУКА, КУЛЬТУРА',
  'ЗДРАВООХРАНЕНИЕ', 'АРМИЯ, МВД, ОРГАНЫ БЕЗОПАСНОСТИ', 'ТОРГОВЛЯ, БЫТОВОЕ ОБСЛУЖИВАНИЕ',
  'ФИНАНСЫ', 'ЭНЕРГЕТИЧЕСКАЯ ПРОМЫШЛЕННОСТЬ', 'ЖИЛИЩНО-КОММУНАЛЬНОЕ ХОЗЯЙСТВО',
  'ОПЕРАЦИИ С НЕДВИЖИМОСТЬЮ', 'СОЦИАЛЬНОЕ ОБСЛУЖИВАНИЕ', 'ЮРИСПРУДЕНЦИЯ', 'ЦЕРКОВЬ',
  'ХИМИЧЕСКАЯ ПРОМЫШЛЕННОСТЬ', 'ДЕРЕВООБРАБАТЫВАЮЩАЯ ПРОМЫШЛЕННОСТЬ', 'ЛЕСНОЕ ХОЗЯЙСТВО',
  'СПОРТ, ТУРИЗМ, РАЗВЛЕЧЕНИЯ', 'УСЛУГИ НАСЕЛЕНИЮ', 'IT, ИНФОРМАЦИОННЫЕ ТЕХНОЛОГИИ',
  'ЭКОЛОГИЯ, ЗАЩИТА ОКРУЖАЮЩЕЙ СРЕДЫ', 'ОРГАНИЗАЦИЯ ОБЩЕСТВЕННОГО ПИТАНИЯ',
  'СМИ, ИЗДАТЕЛЬСТВО, ПЕЧАТЬ, ТЕЛЕКОММУНИКАЦИИ', 'РЕКЛАМА, МАРКЕТИНГ',
  'ОБЩЕСТВЕННЫЕ ОРГАНИЗАЦИИ, СОВЕТ ВЕТЕРАНОВ И ПР'
))
industry_labels = paste0(toupper(substring(industry_labels, 1,1)), substring(industry_labels, 2))
      
elementary_occ$industry = factor(
  elementary_occ$vj4.1,
  levels = c(1:32),
  labels = industry_labels
)
elementary_occ$industry = replaceNaWithNamedFactor(elementary_occ$industry, 'Другое')

elementary_occ_ind_freq = elementary_occ %>% 
  group_by(industry) %>% 
  summarise(n = n()) %>%
  mutate(freq=n/nrow(elementary_occ)*100.0) %>%
  arrange(n)

elementary_occ_ind_freq$industry = factor(
  elementary_occ_ind_freq$industry,
  levels = elementary_occ_ind_freq$industry
)

ggplot(elementary_occ_ind_freq, aes(industry, freq)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat = "identity", fill="#136593") +
  coord_flip() +
  theme(legend.position = "top") +
  labs(x=NULL, 
       y=NULL, 
       fill = NULL,
       title="Elementary occupations. Industry distribution - Russia 2017", 
       caption="Source: rlms26") +
  scale_fill_brewer(palette="Dark2")

### AGE 

ggplot(elementary_occ, aes(x = age)) + 
  geom_histogram(color="#E69F00", fill="#E69F00", alpha=0.5) +
  labs(x = "Age", y = NULL, 
       title="Elementary occupations. Age distribution - Russia 2017",
       caption="Source: rlms26"
  ) +
  geom_vline(
    aes(xintercept = median(na.omit(elementary_occ$age)), color="median"), 
    linetype = "longdash"
  ) + 
  geom_vline(
    aes(xintercept = mean(na.omit(elementary_occ$age)), color="mean"), 
    linetype = "longdash"
  ) + 
  scale_color_manual(name = "statistics", values = c(median = "red", mean = "blue"))

