# =============================================================================
# This script deletes unecessary columns from the ENOE cohort data, creates new
# variables that will be used in the analysis, and saves the final dataset for 
# each cohort to then be merged into a single panel dataset.
# =============================================================================

# Load necessary libraries
library(tidyverse)
library(haven)
library(readxl)
library(lubridate)

delete_unnecessary_columns <- function(df, columns_to_keep) {
  df <- df[, columns_to_keep]
}

year_month <- function(df){
  df <- df %>%
    mutate(
      year = as.character(format(observation_date, "%y")),
      month = as.character(month(observation_date))) %>%
    select(-observation_date)
}

create_new_variables <- function(df, cohort_number) {
  df <- df %>% rename(hrly_salary = ing_x_hrs,
                      years_schooling = anios_esc,
                      school = cs_p17,
                      weekly_hours_worked = hrsocup,
                      monthly_salary = ingocup,
                      household_moved = h_mud,
                      month = d_mes,
                      year = d_anio,
                      max_edu = cs_p13_1,
                      weights = fac
                      ) %>%
    mutate(school = ifelse(school == 1, 1, 0)) %>%
    mutate(cohort = cohort_number,
           hrly_salary = as.numeric(hrly_salary),
           monthly_salary = as.numeric(monthly_salary),
           year = sprintf("%02d", as.numeric(year)),
           year = as.character(year),
           month = as.character(month),
           month_year = paste0("20", year, "-", sprintf("%02d", as.numeric(month))),
           employed = ifelse(clase1 == 1 & clase2 == 1, 1, 0),
           unemployed = ifelse(clase1 == 1 & clase2 == 2, 1, 0),
           PEA = ifelse(clase1 == 1, 1, 0),
           PNEA = ifelse(clase1 == 2, 1, 0),
           primary = age >= 6 & age <= 11,
           secondary = age >= 12 & age <= 14,
           high = age >= 15 & age <= 18,
           senior = age >= 17 & age <= 18) %>%
    arrange(id, n_ent) %>%
    group_by(id) %>%
    mutate(dropout = if_else(school == 0 & lag(school) == 1 &
                               ((!senior) | (senior & month < 5)), 1, 0, missing = 0)) %>%
    ungroup() %>% 
    group_by(id_hog) %>%
    mutate(n_hh = n_distinct(id))
  
  
  # load in FX and convert to USD
  path <- "/Users/wernerd/Desktop/Daniel Werner/FX, CPI"
  fx_rates <- read_excel(file.path(path, "FX.xlsx"))
  fx_rates <- year_month(fx_rates)
  fx_rates <- fx_rates %>% mutate(FX = as.numeric(FX))
  df <- df %>% left_join(fx_rates, by = c("year", "month")) %>%
    mutate(hrly_salary_usd = hrly_salary / FX,
           monthly_salary_usd = monthly_salary / FX) %>%
    select(-FX)
  # load in Mexican and American CPI to normalize set both in real terms
  cpi_mex <- read_excel(file.path(path, "MEX_CPI.xlsx"))
  cpi_mex <- year_month(cpi_mex)
  cpi_mex <- cpi_mex %>% mutate(MEX_CPI = as.numeric(MEX_CPI))
  cpi_us <- read_excel(file.path(path, "US_CPI.xlsx"))
  cpi_us <- year_month(cpi_us)
  cpi_us <- cpi_us %>% mutate(US_CPI = as.numeric(US_CPI))
  df <- df %>% left_join(cpi_mex, by = c("year", "month")) %>%
    left_join(cpi_us, by = c("year", "month")) %>%
    mutate(hrly_salary_real = hrly_salary / MEX_CPI,
           monthly_salary_real = monthly_salary / MEX_CPI,
           hrly_salary_real_usd = hrly_salary_usd / US_CPI,
           monthly_salary_real_usd = monthly_salary_usd / US_CPI) %>%
    select(-US_CPI, -MEX_CPI)
  #done
}

main <- function(){
  download_path <- "/Users/wernerd/Desktop/Daniel Werner/Cohorts/"
  #real download path is "../output/" but storing locally for testing
  args <- commandArgs(trailingOnly = TRUE)
  cohort_number <- as.integer(args[1])
  cohort_file <- sprintf("Cohort_%d.dta", cohort_number)
  cohort <- read_dta(file.path(download_path, cohort_file))
  columns_to_keep <- c("municipality", "id_viv", "id_hog", "id", "age", "ing_x_hrs",
                       "ingocup", "hrsocup", "anios_esc", "cs_p17", "clase1",
                       "clase2", "h_mud", "sex", "d_mes", "d_anio",
                       "cs_p13_1", "n_ent", "fac_tri")
  cohort <- delete_unnecessary_columns(cohort, columns_to_keep)
  cohort <- create_new_variables(cohort, cohort_number)
  write_dta(cohort, paste0("../output/",
                           sprintf("CleanCohort_%d.dta", cohort_number)))
  
}
# Execute
main()