# =============================================================================
# this script makes municipal level panel data set from the ENOE panel dataset.
# this will make up the core of the analysis outcome variables and controls.
# =============================================================================


library(haven)
library(tidyverse)
library(ggplot2)

## load in panel dataset
base_path <- "/Users/wernerd/Desktop/Daniel Werner"
panel <- read_dta(file.path(base_path,"ENOE_panel.dta"))

panel <- panel %>%
  mutate(across(
    c(dropout, school, weights, sex, n_hh, weekly_hours_worked, years_schooling, 
      employed, unemployed, PEA, age, monthly_salary_real, hrly_salary_real,
      monthly_salary_real_usd, hrly_salary_real_usd),
    ~ as.numeric(.)
  ))

## First make the municipal level data set. No Survey Weigths!

municipal_panel <- panel %>%
  group_by(municipality, month, year) %>%
  summarise(
    n_kids = sum(school, na.rm = TRUE),
    dropout_rate_total = sum(dropout, na.rm = TRUE) / sum(school, na.rm = TRUE),
    dropout_rate_primary = sum(dropout * primary, na.rm = TRUE) / sum(school * primary, na.rm = TRUE),
    dropout_rate_secondary = sum(dropout * secondary, na.rm = TRUE) / sum(school * secondary, na.rm = TRUE),
    dropout_rate_high = sum(dropout * high, na.rm = TRUE) / sum(school * high, na.rm = TRUE),
    dropout_rate_male = sum(dropout * (sex == 1), na.rm = TRUE) / sum(school * (sex == 1), na.rm = TRUE),
    dropout_rate_female = sum(dropout * (sex == 2), na.rm = TRUE) / sum(school * (sex == 2), na.rm = TRUE),
    avg_hh_size = mean(n_hh, na.rm = TRUE),
    avg_weekly_hours_worked = mean(weekly_hours_worked, na.rm = TRUE),
    avg_weekly_hours_worked_workers = mean(
      weekly_hours_worked[weekly_hours_worked > 0],
      na.rm = TRUE
    ),
    avg_years_schooling = mean(if_else(years_schooling == 99, NA_real_, years_schooling), na.rm = TRUE),
    employment_rate =  sum(employed, na.rm = TRUE)/sum(PEA, na.rm = TRUE),
    unemployment_rate =  sum(unemployed, na.rm = TRUE)/sum(PEA, na.rm = TRUE),
    avg_age = mean(if_else(age %in% c(99,98,0), NA_real_, age), na.rm = TRUE),
    avg_monthly_salary_real = mean(monthly_salary_real, na.rm = TRUE),
    avg_hrly_salary_real = mean(hrly_salary_real, na.rm = TRUE),
    avg_monthly_salary_real_USD = mean(monthly_salary_real_usd, na.rm = TRUE),
    avg_hrly_salary_real_USD = mean(hrly_salary_real_usd, na.rm = TRUE),
    avg_monthly_salary_real_earners = mean(
      monthly_salary_real[monthly_salary_real > 0],
      na.rm = TRUE
    ),
    avg_hrly_salary_real_earners = mean(
      hrly_salary_real[hrly_salary_real > 0],
      na.rm = TRUE
    ),
    avg_monthly_salary_real_USD_earners = mean(
      monthly_salary_real_usd[monthly_salary_real_usd > 0],
      na.rm = TRUE
    ),
    avg_hrly_salary_real_USD_earners = mean(
      hrly_salary_real_usd[hrly_salary_real_usd > 0],
      na.rm = TRUE
    ),
    .groups = "drop_last"
  )

municipal_panel <- municipal_panel %>%
  mutate(across(everything(), ~ replace(.x, is.nan(.x) | is.infinite(.x), NA_real_)))

municipal_panel <- municipal_panel %>%
  rename(
    avg_ms_real_usd_earners = avg_monthly_salary_real_USD_earners,
    avg_hs_real_usd_earners = avg_hrly_salary_real_USD_earners,
    avg_ms_real_earners = avg_monthly_salary_real_earners,
    avg_hs_real_earners = avg_hrly_salary_real_earners
  )

#Save Municipal Dataset
write_dta(municipal_panel, file.path(base_path,"Municipal_data.dta"))

### Now, make the individual level data set 
rm(municipal_panel)

individual_panel <- panel %>%
  group_by(id_hog, month, year) %>%
  mutate(
    avg_weekly_hours_worked_hh = mean(weekly_hours_worked, na.rm = TRUE),
    avg_weekly_hours_worked_workers_hh = mean(
      weekly_hours_worked[weekly_hours_worked > 0],
      na.rm = TRUE
    ),
    avg_years_schooling_hh = mean(if_else(years_schooling == 99, NA_real_, years_schooling), na.rm = TRUE),
    max_years_schooling_hh = max(if_else(years_schooling == 99, NA_real_, years_schooling), na.rm = TRUE),
    employment_rate_hh =  sum(employed, na.rm = TRUE)/sum(PEA, na.rm = TRUE),
    unemployment_rate_hh =  sum(unemployed, na.rm = TRUE)/sum(PEA, na.rm = TRUE),
    avg_monthly_salary_real_hh = mean(monthly_salary_real, na.rm = TRUE),
    avg_hrly_salary_real_hh = mean(hrly_salary_real, na.rm = TRUE),
    avg_monthly_salary_real_USD_hh = mean(monthly_salary_real_usd, na.rm = TRUE),
    avg_hrly_salary_real_USD_hh = mean(hrly_salary_real_usd, na.rm = TRUE),
    avg_monthly_salary_real_earners_hh = mean(
      monthly_salary_real[monthly_salary_real > 0],
      na.rm = TRUE
    ),
    avg_hrly_salary_real_earners_hh = mean(
      hrly_salary_real[hrly_salary_real > 0],
      na.rm = TRUE
    ),
    avg_monthly_salary_real_USD_earners_hh = mean(
      monthly_salary_real_usd[monthly_salary_real_usd > 0],
      na.rm = TRUE
    ),
    avg_hrly_salary_real_USD_earners_hh = mean(
      hrly_salary_real_usd[hrly_salary_real_usd > 0],
      na.rm = TRUE
    )
  ) %>% ungroup()

individual_panel <- individual_panel %>% filter(age>05 & age<=18)

individual_panel <- individual_panel %>%
  mutate(across(everything(), ~ replace(.x, is.nan(.x) | is.infinite(.x), NA_real_)))

individual_panel <- individual_panel %>%
  rename(
    avg_ms_real_usd_earners = avg_monthly_salary_real_USD_earners_hh,
    avg_hs_real_usd_earners = avg_hrly_salary_real_USD_earners_hh,
    avg_ms_real_earners = avg_monthly_salary_real_earners_hh,
    avg_hs_real_earners = avg_hrly_salary_real_earners_hh,
    avg_whw_hh = avg_weekly_hours_worked_hh,
    avg_whww_hh = avg_weekly_hours_worked_workers_hh
  )

write_dta(individual_panel, file.path(base_path,"Individual_data.dta"))