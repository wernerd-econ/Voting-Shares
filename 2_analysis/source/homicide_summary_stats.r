# ========================================================================== #
 # Making summary statistics for national homicide data
# =========================================================================== #

library(haven)
library(tidyverse)
library(knitr)
library(kableExtra)
base_path <- "/Users/wernerd/Desktop/Daniel Werner"
homicide_data <- read_dta(file.path(base_path,"homicides.dta"))

ntl_homicide <- homicide_data %>% 
  group_by(month, year) %>%
  summarise(hom_ntl = sum(homicides),
            pop_ntl = sum(pop_tot),
            ntl_hom_per_tenk = hom_ntl/pop_ntl * 10000,
            year_month = paste0(year, "-", month)) %>%
  distinct(year_month, .keep_all = TRUE)

ntl_homicide$year_month <- as.Date(paste0(ntl_homicide$year_month, "-01"))

ntl_homicide <- ntl_homicide %>%
  mutate(Period = case_when(
    year_month >= as.Date("2007-01-01") & year_month <= as.Date("2012-12-01") ~ "Calderón's War on Drugs",
    year_month >= as.Date("2013-01-01") & year_month <= as.Date("2015-03-01") ~ "Intermediate Period",
    year_month >= as.Date("2015-04-01") & year_month <= as.Date("2024-01-01") ~ "Respike",  
    TRUE ~ NA_character_
  ))

summary_stats <- ntl_homicide %>% 
  bind_rows(mutate(ntl_homicide, period = "Overall")) %>%
  group_by(Period) %>%
  summarise(
    Min    = min(ntl_hom_per_tenk, na.rm = TRUE),
    Max    = max(ntl_hom_per_tenk, na.rm = TRUE),
    Mean   = mean(ntl_hom_per_tenk, na.rm = TRUE),
    Median = median(ntl_hom_per_tenk, na.rm = TRUE),
    `Std Dev` = sd(ntl_hom_per_tenk, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(Period, levels = c("Overall","Calderón's War on Drugs","Intermediate Period","Respike")))


table <- summary_stats %>%
  kable("latex", booktabs = TRUE, digits = 3) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

cat(table, file = "Homicide_Summary_Stats.tex")