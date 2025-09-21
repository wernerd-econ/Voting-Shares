## =============================================================== ##
#make final data set
## ============================================================== ##
library(haven)
library(tidyverse)
library(readxl)
library(lubridate)

#START WITH MUNICIPAL DATA
base_path <-  "/Users/wernerd/Desktop/Daniel Werner/"
municipal_data <- read_dta(file.path(base_path, "Municipal_data.dta"))
homicide_data <- read_dta(file.path(base_path, "homicides.dta"))
geo_data <- read_dta(file.path(base_path, "final_geo.dta"))
total_seizures <- read_dta(file.path(base_path, "seizure_data.dta"))

common_codes <- intersect(intersect(municipal_data$municipality, homicide_data$municipality), geo_data$municipality)
municipal_data <- municipal_data %>% filter(municipality %in% common_codes)
homicide_data <- homicide_data %>% filter(municipality %in% common_codes)
geo_data <- geo_data %>% filter(municipality %in% common_codes)

total_seizures <- total_seizures %>% rename(year = Year)
total_seizures <- total_seizures %>% rename(month = Month)

years <- as.character(unique(total_seizures$year))
municipal_data <- municipal_data %>% mutate(year = paste0("20", year))
municipal_data <- municipal_data %>% filter(year %in% years)
homicide_data <- homicide_data %>% filter(year %in% years)
homicide_data <- homicide_data %>%
  mutate(
    year = as.character(year),
    month = sprintf("%02d", as.integer(month)),  # adds leading zero
    year_month = paste0(year, "-", month)        # combine into YYYY-MM
  )
municipal_data <- municipal_data %>%
  mutate(
    year = as.character(year),
    month = sprintf("%02d", as.integer(month)),  # adds leading zero
    year_month = paste0(year, "-", month)        # combine into YYYY-MM
  )
total_seizures <- total_seizures %>%
  mutate(
    year = as.character(year),
    month = sprintf("%02d", as.integer(month)),  # adds leading zero
    year_month = paste0(year, "-", month)        # combine into YYYY-MM
  )
common_dates <- intersect(intersect(municipal_data$year_month, homicide_data$year_month), total_seizures$year_month)
municipal_data <- municipal_data %>% filter(year_month %in% common_dates)
homicide_data <- homicide_data %>% filter(year_month %in% common_dates)
total_seizures <- total_seizures %>% filter(year_month %in% common_dates)

total_seizures <- total_seizures %>% ungroup() %>% select(-month, -year)
full <- homicide_data %>%
  left_join(total_seizures, by = "year_month")

full <- full %>% left_join(geo_data, by = "municipality")

municipal_data <- municipal_data %>% select(-month, -year)

full <- full %>% inner_join(municipal_data, by = c("municipality", "year_month"))

#save full dataset
write_dta(full, file.path(base_path, "final_mun.dta"))

## MOVE ON TOP INDIVIDUAL LEVEL DATA SET
rm(full,municipal_data, total_seizures, geo_data, homicide_data)
rm(common_codes, common_dates,years)

individual_data <- read_dta(file.path(base_path, "Individual_data.dta"))
homicide_data <- read_dta(file.path(base_path, "homicides.dta"))
geo_data <- read_dta(file.path(base_path, "final_geo.dta"))
total_seizures <- read_dta(file.path(base_path, "seizure_data.dta"))

common_codes <- intersect(intersect(individual_data$municipality, homicide_data$municipality), geo_data$municipality)
individual_data <- individual_data %>% filter(municipality %in% common_codes)
homicide_data <- homicide_data %>% filter(municipality %in% common_codes)
geo_data <- geo_data %>% filter(municipality %in% common_codes)

total_seizures <- total_seizures %>% rename(year = Year)
total_seizures <- total_seizures %>% rename(month = Month)

years <- as.character(unique(total_seizures$year))
individual_data <- individual_data %>% mutate(year = paste0("20", year))
individual_data <- individual_data %>% filter(year %in% years)
homicide_data <- homicide_data %>% filter(year %in% years)
homicide_data <- homicide_data %>%
  mutate(
    year = as.character(year),
    month = sprintf("%02d", as.integer(month)),  # adds leading zero
    year_month = paste0(year, "-", month)        # combine into YYYY-MM
  )
individual_data <- individual_data %>%
  mutate(
    year = as.character(year),
    month = sprintf("%02d", as.integer(month)),  # adds leading zero
    year_month = paste0(year, "-", month)        # combine into YYYY-MM
  )
total_seizures <- total_seizures %>%
  mutate(
    year = as.character(year),
    month = sprintf("%02d", as.integer(month)),  # adds leading zero
    year_month = paste0(year, "-", month)        # combine into YYYY-MM
  )
common_dates <- intersect(intersect(individual_data$year_month, homicide_data$year_month), total_seizures$year_month)
individual_data <- individual_data %>% filter(year_month %in% common_dates)
homicide_data <- homicide_data %>% filter(year_month %in% common_dates)
total_seizures <- total_seizures %>% filter(year_month %in% common_dates)

total_seizures <- total_seizures %>% ungroup() %>% select(-month, -year)
full <- homicide_data %>%
  left_join(total_seizures, by = "year_month")

full <- full %>% left_join(geo_data, by = "municipality")

individual_data <- individual_data %>% select(-month, -year)

full <- full %>% inner_join(individual_data, by = c("municipality", "year_month"))

write_dta(full, file.path(base_path, "final_indiv.dta"))








