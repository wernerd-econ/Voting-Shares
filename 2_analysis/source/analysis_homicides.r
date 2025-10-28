library(tidyverse)
library(stringr)
library(haven)
library(broom)
library(purrr)

base_path <- "/Users/wernerd/Desktop/Voting Shares Raw"

voting <- read.csv(file.path(base_path, "voting.csv"))

voting$date <- as.Date(as.character(voting$date), format = "%Y%m%d")

voting <- voting %>%
  mutate(
    election_date = date,
    inauguration_date = date %m+% months(6)
  ) %>%
  select(-date)

voting <- voting %>%
  mutate(across(matches("^v\\d{2}$"), ~ .x / efec,
                .names = "s{substr(.col, 2, 3)}"))

voting <- voting %>% filter(yr >= 2018)

voting_long <- voting %>%
  pivot_longer(
    cols = matches("^(l|s)\\d{2}$"),
    names_to = c(".value", "num"),
    names_pattern = "([ls])(\\d{2})"
  )

margin_strict <- voting_long %>%
  group_by(yr, inegi) %>%
  mutate(max_other = max(s[!str_detect(l, "morena")], na.rm = TRUE)) %>%
  filter(l == "morena") %>%
  transmute(
    yr,
    inegi,
    morena_share = s,
    max_other,
    margin = morena_share - max_other,
    win,
    inauguration_date,
    election_date
  ) %>%
  ungroup()

margin_coalition_included <- voting_long %>%
  group_by(yr, inegi) %>%
  mutate(max_other = max(s[!str_detect(l, "morena")], na.rm = TRUE)) %>%
  filter(str_detect(l, "morena")) %>%
  transmute(
    yr,
    inegi,
    morena_share = s,
    max_other,
    margin = morena_share - max_other,
    win,
    inauguration_date,
    election_date
  ) %>%
  ungroup()

margin_strict <- margin_strict %>% 
  mutate(morenaWin = as.integer(margin>0),
         yr = as.integer(yr))

margin_coalition_included <- margin_coalition_included %>% 
  mutate(morenaWin = as.integer(margin>0),
         yr = as.integer(yr))

########################## GET THE HOMICIDE DATA INVOLVED #####################
homicides <- read_dta(file.path(base_path, "../Daniel Werner/homicides.dta"))
homicides <- homicides %>% select(municipality, year, month, pop_tot, homicides)

homicides <- homicides %>%
  mutate(
    date = make_date(year, month, 1)  
  )

margin_strict <- margin_strict %>%
  filter(yr >= 2019 & yr <= 2022)

margin_coalition_included <- margin_coalition_included %>%
  filter(yr >= 2019 & yr <= 2022)

homicides_voting <- margin_strict %>%
  left_join(homicides, by = c("inegi" = "municipality"))

homicides_voting_coalition <- margin_coalition_included %>%
  left_join(homicides, by = c("inegi" = "municipality"))

homicides_voting <- homicides_voting %>%
  mutate(
    period = case_when(
      date >= (election_date %m-% months(12)) & date < election_date       ~ "pre",
      date >  election_date & date < inauguration_date        ~ "lame_duck",
      date >= inauguration_date & date <= inauguration_date %m+% months(12)      ~ "post",
      TRUE                                                                   ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

homicides_voting_coalition <- homicides_voting_coalition %>%
  mutate(
    period = case_when(
      date >= (election_date %m-% months(12)) & date < election_date       ~ "pre",
      date >  election_date & date < inauguration_date        ~ "lame_duck",
      date >= inauguration_date & date <= inauguration_date %m+% months(12)      ~ "post",
      TRUE                                                                   ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

make_avg <- function(df) {
  df %>%
    group_by(inegi, inauguration_date, period) %>%
    summarise(
      avg_homicide_rate = mean(homicides, na.rm = TRUE)/mean(pop_tot) * 12 * 100000,
      pop_tot = mean(pop_tot),
      morenaWin = unique(morenaWin),
      margin = unique(margin),
      .groups = "drop"
    )
}

# Make perioded averages for homicide data
perioded_post      <- make_avg(homicides_voting %>% filter(period == "post"))     
perioded_pre       <- make_avg(homicides_voting %>% filter(period == "pre"))       
perioded_lame      <- make_avg(homicides_voting %>% filter(period == "lame_duck")) 

perioded_post_coal <- make_avg(homicides_voting_coalition %>% filter(period == "post"))      
perioded_pre_coal  <- make_avg(homicides_voting_coalition %>% filter(period == "pre"))       
perioded_lame_coal <- make_avg(homicides_voting_coalition %>% filter(period == "lame_duck")) 

################################# RD ###########################################
# Prepare RD data 
prep_rd <- function(df, binwidth) {
  df %>%
    mutate(
      spread_w  = margin * morenaWin,
      spread_l  = margin * (1 - morenaWin),
      spread_w2 = spread_w^2,
      spread_l2 = spread_l^2
    ) %>%
    filter(abs(margin) <= binwidth)
}

rd_post      <- prep_rd(perioded_post, 0.08)
rd_pre       <- prep_rd(perioded_pre, 0.08)
rd_lame      <- prep_rd(perioded_lame, 0.08)
rd_post_coal <- prep_rd(perioded_post_coal, 0.08)
rd_pre_coal  <- prep_rd(perioded_pre_coal, 0.08)
rd_lame_coal <- prep_rd(perioded_lame_coal, 0.08)

outcomes <- c("avg_homicide_rate")

# Function to run RD for one outcome
run_rd_single <- function(outcome_var, data, weight_var = "pop_tot", func = "linear") {
  if (func == "linear"){
    frm <- as.formula(paste0(outcome_var, " ~ morenaWin + spread_w + spread_l"))
  } else {
    frm <- as.formula(paste0(outcome_var, " ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2"))
  }
  model <- lm(frm, data = data, weights = data[[weight_var]])
  tidy(model) %>%
    filter(term == "morenaWin") %>%
    transmute(
      outcome = outcome_var,
      estimate = estimate,
      std.error = std.error,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )
}

# Run for all outcomes × periods
run_all <- function(df, label, func) {
  map_dfr(outcomes, run_rd_single, data = df, weight_var = "pop_tot", func = func) %>%
    mutate(period = label)
}

results_post      <- run_all(rd_post, "post", func = "quadratic")
results_pre       <- run_all(rd_pre, "pre", func = "quadratic")
results_lame      <- run_all(rd_lame, "lame_duck", func = "quadratic")

results_post_coal <- run_all(rd_post_coal, "post", func = "quadratic")
results_pre_coal  <- run_all(rd_pre_coal, "pre", func = "quadratic")
results_lame_coal <- run_all(rd_lame_coal, "lame_duck", func = "quadratic")

results_strict    <- bind_rows(results_pre, results_lame, results_post) %>% mutate(sample = "strict")
results_coalition <- bind_rows(results_pre_coal, results_lame_coal, results_post_coal) %>% mutate(sample = "coalition")

results_all <- bind_rows(results_strict, results_coalition) %>%
  mutate(
    outcome = factor(outcome, levels = rev(outcomes)),
    period  = factor(period, levels = c("pre", "lame_duck", "post"))
  )

# Plot
ggplot(results_all, aes(x = outcome, y = estimate, color = period)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.6), width = 0.25) +
  facet_wrap(~ sample) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimated coefficient on morenaWin",
    title = "RD estimates (morenaWin) — pre / lame_duck / post",
    color = "Period"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))