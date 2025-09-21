# =============================================================================
# This script puts five consecutive quarters together to form cohorts of people
# =============================================================================
suppressPackageStartupMessages({
  suppressMessages({
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(stringr)
    library(purrr)
    library(data.table)
  })
})

# =============================================================================
join_quarters <- function(five_quarters){
  five_quarters <- map(five_quarters, ~ mutate(.x, across(everything(), as.character)))
  common_cols <- Reduce(intersect, list(names(five_quarters[[1]]),
                                        names(five_quarters[[2]]),
                                        names(five_quarters[[3]] ),
                                        names(five_quarters[[4]]),
                                        names(five_quarters[[5]])))
  five_quarters <- map(five_quarters, ~ .x[, common_cols])
  five_quarters <- map(five_quarters, ~ as.data.table(.x))
  panel <- merge(five_quarters[[1]], five_quarters[[2]],
                   by = names(common_cols), all = TRUE)
  panel <- merge(panel, five_quarters[[3]],
                   by = names(common_cols), all = TRUE)
  panel <- merge(panel, five_quarters[[4]],
                   by = names(common_cols), all = TRUE)
  merge(panel, five_quarters[[5]],
        by = names(common_cols), all = TRUE)
}

add_labels <- function(first, first_name, second, second_name,
                       third, third_name, fourth, fourth_name,
                       fifth, fifth_name) {
  first <- first %>% filter(n_ent == 1) %>%
    mutate(trim = make_labels(first_name)$label)
  second <- second %>% filter(n_ent == 2) %>%
    mutate(trim = make_labels(second_name)$label)
  third <- third %>% filter(n_ent == 3) %>%
    mutate(trim = make_labels(third_name)$label)
  fourth <- fourth %>% filter(n_ent == 4) %>%
    mutate(trim = make_labels(fourth_name)$label)
  fifth <- fifth %>% filter(n_ent == 5) %>%
    mutate(trim = make_labels(fifth_name)$label)
  list(first, second, third, fourth, fifth)
}


make_labels <- function(file_name) {
  year <- str_extract(file_name, "\\d{4}")
  quarter <- str_extract(file_name, "T\\d")
  label <- paste0(year, "_", quarter)
  list(year = year, quarter = quarter, label = label)
}

# ---- Main loop over cohorts ----

main <- function(){
  cohorts <- c(
    "2007_T1.dta", "2007_T2.dta", "2007_T3.dta", "2007_T4.dta", "2008_T1.dta",
    "2007_T2.dta", "2007_T3.dta", "2007_T4.dta", "2008_T1.dta", "2008_T2.dta",
    "2007_T3.dta", "2007_T4.dta", "2008_T1.dta", "2008_T2.dta", "2008_T3.dta",
    "2007_T4.dta", "2008_T1.dta", "2008_T2.dta", "2008_T3.dta", "2008_T4.dta",
    "2008_T1.dta", "2008_T2.dta", "2008_T3.dta", "2008_T4.dta", "2009_T1.dta",
    "2008_T2.dta", "2008_T3.dta", "2008_T4.dta", "2009_T1.dta", "2009_T2.dta",
    "2008_T3.dta", "2008_T4.dta", "2009_T1.dta", "2009_T2.dta", "2009_T3.dta",
    "2008_T4.dta", "2009_T1.dta", "2009_T2.dta", "2009_T3.dta", "2009_T4.dta",
    "2009_T1.dta", "2009_T2.dta", "2009_T3.dta", "2009_T4.dta", "2010_T1.dta",
    "2009_T2.dta", "2009_T3.dta", "2009_T4.dta", "2010_T1.dta", "2010_T2.dta",
    "2009_T3.dta", "2009_T4.dta", "2010_T1.dta", "2010_T2.dta", "2010_T3.dta",
    "2009_T4.dta", "2010_T1.dta", "2010_T2.dta", "2010_T3.dta", "2010_T4.dta",
    "2010_T1.dta", "2010_T2.dta", "2010_T3.dta", "2010_T4.dta", "2011_T1.dta",
    "2010_T2.dta", "2010_T3.dta", "2010_T4.dta", "2011_T1.dta", "2011_T2.dta",
    "2010_T3.dta", "2010_T4.dta", "2011_T1.dta", "2011_T2.dta", "2011_T3.dta",
    "2010_T4.dta", "2011_T1.dta", "2011_T2.dta", "2011_T3.dta", "2011_T4.dta",
    "2011_T1.dta", "2011_T2.dta", "2011_T3.dta", "2011_T4.dta", "2012_T1.dta",
    "2011_T2.dta", "2011_T3.dta", "2011_T4.dta", "2012_T1.dta", "2012_T2.dta",
    "2011_T3.dta", "2011_T4.dta", "2012_T1.dta", "2012_T2.dta", "2012_T3.dta",
    "2011_T4.dta", "2012_T1.dta", "2012_T2.dta", "2012_T3.dta", "2012_T4.dta",
    "2012_T1.dta", "2012_T2.dta", "2012_T3.dta", "2012_T4.dta", "2013_T1.dta",
    "2012_T2.dta", "2012_T3.dta", "2012_T4.dta", "2013_T1.dta", "2013_T2.dta",
    "2012_T3.dta", "2012_T4.dta", "2013_T1.dta", "2013_T2.dta", "2013_T3.dta",
    "2012_T4.dta", "2013_T1.dta", "2013_T2.dta", "2013_T3.dta", "2013_T4.dta",
    "2013_T1.dta", "2013_T2.dta", "2013_T3.dta", "2013_T4.dta", "2014_T1.dta",
    "2013_T2.dta", "2013_T3.dta", "2013_T4.dta", "2014_T1.dta", "2014_T2.dta",
    "2013_T3.dta", "2013_T4.dta", "2014_T1.dta", "2014_T2.dta", "2014_T3.dta",
    "2013_T4.dta", "2014_T1.dta", "2014_T2.dta", "2014_T3.dta", "2014_T4.dta",
    "2014_T1.dta", "2014_T2.dta", "2014_T3.dta", "2014_T4.dta", "2015_T1.dta",
    "2014_T2.dta", "2014_T3.dta", "2014_T4.dta", "2015_T1.dta", "2015_T2.dta",
    "2014_T3.dta", "2014_T4.dta", "2015_T1.dta", "2015_T2.dta", "2015_T3.dta",
    "2014_T4.dta", "2015_T1.dta", "2015_T2.dta", "2015_T3.dta", "2015_T4.dta",
    "2015_T1.dta", "2015_T2.dta", "2015_T3.dta", "2015_T4.dta", "2016_T1.dta",
    "2015_T2.dta", "2015_T3.dta", "2015_T4.dta", "2016_T1.dta", "2016_T2.dta",
    "2015_T3.dta", "2015_T4.dta", "2016_T1.dta", "2016_T2.dta", "2016_T3.dta",
    "2015_T4.dta", "2016_T1.dta", "2016_T2.dta", "2016_T3.dta", "2016_T4.dta",
    "2016_T1.dta", "2016_T2.dta", "2016_T3.dta", "2016_T4.dta", "2017_T1.dta",
    "2016_T2.dta", "2016_T3.dta", "2016_T4.dta", "2017_T1.dta", "2017_T2.dta",
    "2016_T3.dta", "2016_T4.dta", "2017_T1.dta", "2017_T2.dta", "2017_T3.dta",
    "2016_T4.dta", "2017_T1.dta", "2017_T2.dta", "2017_T3.dta", "2017_T4.dta",
    "2017_T1.dta", "2017_T2.dta", "2017_T3.dta", "2017_T4.dta", "2018_T1.dta",
    "2017_T2.dta", "2017_T3.dta", "2017_T4.dta", "2018_T1.dta", "2018_T2.dta",
    "2017_T3.dta", "2017_T4.dta", "2018_T1.dta", "2018_T2.dta", "2018_T3.dta",
    "2017_T4.dta", "2018_T1.dta", "2018_T2.dta", "2018_T3.dta", "2018_T4.dta",
    "2018_T1.dta", "2018_T2.dta", "2018_T3.dta", "2018_T4.dta", "2019_T1.dta",
    "2018_T2.dta", "2018_T3.dta", "2018_T4.dta", "2019_T1.dta", "2019_T2.dta",
    "2018_T3.dta", "2018_T4.dta", "2019_T1.dta", "2019_T2.dta", "2019_T3.dta",
    "2018_T4.dta", "2019_T1.dta", "2019_T2.dta", "2019_T3.dta", "2019_T4.dta",
    "2019_T1.dta", "2019_T2.dta", "2019_T3.dta", "2019_T4.dta", "2020_T1.dta"
  )
  download_path <- "/Users/wernerd/Desktop/Daniel Werner/Quarterly"
  #real download path is "../output/" but storing locally for testing
  args <- commandArgs(trailingOnly = TRUE)
  i <- as.integer(args[1])
  first <- read_dta(file.path(download_path, cohorts[i]))
  second <- read_dta(file.path(download_path, cohorts[i + 1]))
  third <- read_dta(file.path(download_path, cohorts[i + 2]))
  fourth <- read_dta(file.path(download_path, cohorts[i + 3]))
  fifth <- read_dta(file.path(download_path, cohorts[i + 4]))

  cohort <- add_labels(first, cohorts[i],
                       second, cohorts[i + 1],
                       third, cohorts[i + 2],
                       fourth, cohorts[i + 3],
                       fifth, cohorts[i + 4])
  panel <- join_quarters(cohort)
  rm(first, second, third, fourth, fifth, cohort)
  invisible(gc())

  # Checks on data
  panel <- panel %>%
    mutate(age = as.numeric(eda)) %>%
    group_by(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren) %>%
    #filter cases where individual age discrepancy is too large
    #(cannot age more than 2 years in 5 quarters)
    mutate(min_age = min(age), max_age = max(age)) %>%
    mutate(age_diff = abs(max_age - min_age)) %>%
    ungroup() %>% 
    filter(age_diff < 2) %>%
    #also make sure sex is constant over the duration of panel
    group_by(cd_a, ent, con, v_sel, n_hog, h_mud, n_ren, sex) %>%
    add_tally() %>%
    rename(total_n = n) %>%
    ungroup() %>%
    #selecting only individuals who are viewed in all 5 quarters
    filter(total_n == 5)

  #make IDs -- Individual, Household, and Dwelling
  #Individual ID
  panel <- panel %>%
    group_by(cd_a, ent, con, v_sel,n_hog, h_mud, n_ren) %>%
    mutate(id = cur_group_id()) %>%
    ungroup()
  #HH ID
  panel <- panel %>%
    group_by(cd_a, ent, con, v_sel,n_hog, h_mud) %>%
    mutate(id_hog = cur_group_id()) %>%
    ungroup()
  #Dwelling ID
  panel <- panel %>%
    group_by(cd_a, ent, con, v_sel) %>%
    mutate(id_viv = cur_group_id()) %>%
    ungroup()
  #Creating Full Municipality Code (Ent + Mun)
  panel <- panel %>%
    mutate(municipality = paste0(ent, "0", sprintf("%02s", mun)))
  cohort_number <- (i-1) / 5 + 1
  write_dta(panel, paste0("../output/", sprintf("Cohort_%d.dta", cohort_number)))
}

# ---- Execute ----
main()