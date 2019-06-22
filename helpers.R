library(dplyr)

replaceNaWithNamedFactor = function(factor_col, name) {
  factor_levels = levels(factor_col)
  factor_levels[length(factor_levels) + 1] = name
  
  new_factor_col = factor(factor_col, levels = factor_levels)
  new_factor_col[is.na(new_factor_col)] = name
  return(new_factor_col)
}

# Birth year, salary, has job, job code, gender, birthplace, 30 day working hours
prepareData = function(data, wave, year, born_col, gender_col, salary_col, has_job_col, job_code_col, whours_col) {
  born_col = enquo(born_col)
  salary_col = enquo(salary_col)
  has_job_col = enquo(has_job_col)
  job_code_col = enquo(job_code_col)
  gender_col = enquo(gender_col)
  whours_col = enquo(whours_col)
  
  data %>% 
    mutate(
      salary_int = as.integer(!! salary_col),
      has_job_int = as.integer(!! has_job_col),
      whours_int = as.integer( !! whours_col)
    ) %>% 
    transmute(
      idind = idind,
      born = !! born_col,
      gender = factor(!! gender_col, levels = c(1, 2), c('Male', 'Female')),
      salary = ifelse(salary_int == 99999997 | salary_int == 99999998 | salary_int == 99999999, NA, salary_int),
      has_job = ifelse(has_job_int == 1 | has_job_int == 2 | has_job_int == 3 | has_job_int == 4, TRUE, FALSE),
      isco08code = as.character(!! job_code_col),
      isco08major = substring(isco08code, 1, 1),
      whours = ifelse(whours_int == 99999997 | whours_int == 99999998 | whours_int == 99999999, NA, whours_int),
      wave_n = wave,
      year = year
    )
}