library(lubridate)
library(zoo)
library(readxl)
library(tidyverse)
library(haven)
library(purrr)
library(broom)

base_path <- "/Users/wernerd/Desktop/Voting Shares Raw"
pres_muns <- read_excel(file.path(base_path, "presidentes_municipales.xlsx"))

pres_muns <- pres_muns %>% 
  filter(!if_all(everything(), is.na)) %>%
  select(mun, ano, presmun, parmun1, gobernador, pargob1, presidente, parpres1) %>%
  mutate(mun = as.character(mun))

voting <- read.csv(file.path(base_path, "voting.csv")) %>%
  filter(!status %in% c("cancelled", "appointed", "new--cancelled", "new--appointed", "prep",
                        "missing", "missing--keepHistory"))

voting$date <- as.Date(as.character(voting$date), format = "%Y%m%d")

voting <- voting %>% 
  left_join(pres_muns, by = c("inegi" = "mun", "yr" = "ano"))

voting <- voting %>%
  mutate(
    # inauguration happens 3 months after election
    inauguration_date = date,
    # convert to year-quarter
    inauguration_qtr = as.yearqtr(inauguration_date),
    election_qtr = as.yearqtr(date),
    # shift by one quarter
    inauguration_qtr_lag1 = inauguration_qtr + 2/4,
    # format back into "YYYY-QX"
    inauguration_trim = format(inauguration_qtr_lag1, "%Y-Q%q"),
    election_trim = format(election_qtr, "%Y-Q%q")
  )

voting <- voting %>%
  mutate(across(matches("^v\\d{2}$"), ~ .x / efec,
                .names = "s{substr(.col, 2, 3)}"))

voting_long <- voting %>%
  pivot_longer(
    cols = matches("^(l|s)\\d{2}$"),
    names_to = c(".value", "num"),
    names_pattern = "([ls])(\\d{2})"
  )

voting_long <- voting_long %>% 
  mutate(l = str_split(l, "-", simplify = TRUE)[, 1],
         win = str_split(win, "-", simplify = TRUE)[, 1])

base_margin <- voting_long %>%
  group_by(yr, inegi) %>%
  arrange(desc(s), .by_group = TRUE) %>%
  mutate(winner_party = first(l)) %>%
  mutate(max_other = max(s[!str_detect(l, winner_party)], na.rm = TRUE)) %>%
  ungroup() 

# (a) Winner same as governor
margin_gov <- base_margin %>%
  filter(winner_party == pargob1) %>%
  group_by(yr, inegi) %>%
  transmute(
    yr,
    inegi,
    winner_share = s,
    max_other,
    margin = winner_share - max_other,
    win,
    inauguration_trim,
    election_trim, 
    party = unique(pargob1)
  ) %>%
  ungroup()

# (b) Winner same as both governor and president
margin_gov_pres <- base_margin %>%
  filter(winner_party == pargob1 & winner_party == parpres1) %>%
  group_by(yr, inegi) %>%
  summarise(
    win_share = s[winner_party == governor_party & winner_party == president_party][1],
    max_other = max_other[1],
    margin = win_share - max_other,
    win = 1,
    .groups = "drop"
  )

# (c) Winner same as president
margin_pres <- base_margin %>%
  filter(winner_party == parpres1) %>%
  group_by(yr, inegi) %>%
  summarise(
    win_share = s[winner_party == president_party][1],
    max_other = max_other[1],
    margin = win_share - max_other,
    win = 1,
    .groups = "drop"
  )
