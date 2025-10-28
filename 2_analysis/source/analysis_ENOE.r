library(lubridate)
library(zoo)
library(readxl)
library(tidyverse)
library(haven)
library(purrr)
library(broom)

pop <- read_excel(file.path("/Users/wernerd/Desktop/Daniel Werner/Population", "pop.xlsx"))
pop <- pop %>%
  filter(AÑO %in% 2019:2024) %>%
  group_by(CLAVE, AÑO) %>%
  summarise(
    pop_tot = sum(POB_TOTAL),
    .groups = "drop"
  ) %>%
  rename(municipality = CLAVE, year = AÑO) %>%
  select(municipality, year, pop_tot)

pop <- pop %>%
  arrange(municipality, year) %>%
  group_by(municipality) %>%
  mutate(
    pop_tot_next = lead(pop_tot),
    year_next = lead(year),
    tot_monthly_growth = (pop_tot_next / pop_tot)^(1/12) - 1,
  ) %>%
  filter(!is.na(tot_monthly_growth)) %>%
  ungroup() %>%
  # Expand each row into 12 months
  rowwise() %>%
  mutate(month_data = list(tibble(
    month = 1:12,
    pop_total = pop_tot * (1 + tot_monthly_growth)^(0:11),
  ))) %>%
  unnest(month_data) %>%
  ungroup()

pop <- pop %>% select(municipality, year, month, pop_total,) %>%
  rename(pop_tot = pop_total)

pop_quarterly <- pop %>%
  # create a Date object for the first of each month
  mutate(date = make_date(year, month, 1),
         # convert month to quarter (1: Jan-Mar -> Q1, etc.)
         quarter = quarter(date),
         # format YYYY-QX
         trim = paste0(year, "-Q", quarter)) %>%
  group_by(municipality, trim) %>%
  # aggregate monthly population into quarterly (take mean)
  summarise(
    pop_tot_qtr = mean(pop_tot, na.rm = TRUE),
    .groups = "drop"
  )

pop_quarterly <- pop_quarterly %>% mutate(municipality = as.character(municipality))



base_path <- "/Users/wernerd/Desktop/Voting Shares Raw"

voting <- read.csv(file.path(base_path, "voting.csv"))

voting$date <- as.Date(as.character(voting$date), format = "%Y%m%d")

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
    inauguration_trim,
    election_trim
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
    inauguration_trim,
    election_trim
  ) %>%
  ungroup()

margin_strict <- margin_strict %>% 
  mutate(morenaWin = as.integer(margin>0),
         yr = as.integer(yr))

margin_coalition_included <- margin_coalition_included %>% 
  mutate(morenaWin = as.integer(margin>0),
         yr = as.integer(yr))



stack <- read_dta(file.path(base_path,"STACK.dta"))
stack <- stack %>%
  mutate(year = as.integer(substr(trim, 1, 4))) %>%
  filter(year >= 2017, year <= 2024) 

stack <- stack %>%
  mutate(
    unemployed = ifelse(clase1 == 1 & clase2 == 2, 1, 0),
    employed = ifelse(clase1 == 1 & clase2 == 1, 1, 0), 
    PEA = ifelse(clase1 == 1, 1, 0),
    manufac = ifelse(employed * rama == 2, 1, 0),
    services = ifelse(employed * rama == 4, 1, 0),
    agriculture = ifelse(employed * rama == 6, 1, 0),
    construction = ifelse(employed * rama == 1, 1, 0),
    commerce = ifelse(employed * rama == 3, 1, 0),
    apoyo_economico = ifelse(p14apoyos == 1, 1, 0),
    no_apoyo_economico = ifelse(p14apoyos == 2, 1, 0),
    empleo_informal = ifelse(employed * emp_ppal == 1, 1, 0),
    seg_soc = ifelse(employed * seg_soc == 1, 1, 0)
  )




# Collapse to municipality-year-month level
collapsed <- stack %>%
  group_by(municipality, trim) %>%
  summarise(
    total_unemployed = sum(unemployed * weights, na.rm = TRUE),
    total_employed = sum(employed * weights, na.rm = TRUE),
    total_manufac = sum(manufac * weights, na.rm = TRUE),
    total_services = sum(services * weights, na.rm = TRUE),
    total_agriculture = sum(agriculture * weights, na.rm = TRUE),
    total_construction = sum(construction * weights, na.rm = TRUE),
    total_commerce = sum(commerce * weights, na.rm = TRUE),
    total_recieve_help = sum(apoyo_economico * weights, na.rm = TRUE),
    total_informal = sum(empleo_informal * weights, na.rm = TRUE),
    total_people_apoyo = sum((apoyo_economico+no_apoyo_economico)*weights, na.rm = TRUE),
    total_PEA = sum(PEA * weights, na.rm = TRUE),
    total_seg_soc = sum(seg_soc * weights, na.rm = TRUE),
    unemployment_rate = ifelse(total_PEA > 0, total_unemployed / total_PEA, NA) * 100,
    manufac_rate = ifelse(total_employed > 0, total_manufac / total_employed, NA) * 100,
    services_rate = ifelse(total_employed > 0, total_services / total_employed, NA) * 100,
    agriculture_rate = ifelse(total_employed > 0, total_agriculture / total_employed, NA) * 100,
    construction_rate = ifelse(total_employed > 0, total_construction / total_employed, NA) * 100,
    commerce_rate = ifelse(total_employed > 0, total_commerce / total_employed, NA) * 100,
    share_recieve_help = ifelse(total_people_apoyo > 0, total_recieve_help/ total_people_apoyo, NA) * 100,
    informal_rate = ifelse(total_employed > 0, total_informal / total_employed, NA) * 100,
    share_w_social_security = ifelse(total_employed >0, total_seg_soc / total_employed, NA) * 100,
    .groups = "drop"
  )

x <- 0.1
ineq <- stack %>%
  filter(employed == 1, ingocup > 0, !is.na(ingocup), !is.na(weights)) %>%
  group_by(municipality, trim) %>%
  arrange(ingocup, .by_group = TRUE) %>%
  mutate(
    w = weights / sum(weights, na.rm = TRUE),
    cum_w = cumsum(w),
    group = case_when(
      cum_w <= x ~ "bottom",
      cum_w >= 1 - x ~ "top",
      TRUE ~ "middle"
    )
  ) %>%
  group_by(municipality, trim, group) %>%
  summarise(avg_income = sum(ingocup * w, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = avg_income) %>%
  mutate(diff_top_bottom = top - bottom)

# Merge back
final <- collapsed %>%
  left_join(ineq %>% select(municipality, trim, diff_top_bottom),
            by = c("municipality", "trim"))


margin_strict <- margin_strict %>%
  filter(yr >= 2019 & yr <= 2022)

margin_coalition_included <- margin_coalition_included %>%
  filter(yr >= 2019 & yr <= 2022)

unemp_voting <- margin_strict %>%
  left_join(final, by = c("inegi" = "municipality"))

# Keep only the 4 quarters after inauguration
unemp_voting_coalition <- margin_coalition_included %>%
  left_join(final, by = c("inegi" = "municipality"))

unemp_voting <- unemp_voting %>%
  mutate(
    trim_fixed = str_replace(trim, "T", "Q"),
    inauguration_trim_fixed = str_replace(inauguration_trim, "T", "Q"),
    election_trim_fixed = str_replace(election_trim, "T", "Q"),
    trim_qtr = as.yearqtr(trim_fixed, format = "%Y-Q%q"),
    inauguration_qtr = as.yearqtr(inauguration_trim_fixed, format = "%Y-Q%q"),
    election_qtr = as.yearqtr(election_trim_fixed, format = "%Y-Q%q")
  ) 

unemp_voting_coalition <- unemp_voting_coalition %>%
  mutate(
    trim_fixed = str_replace(trim, "T", "Q"),
    inauguration_trim_fixed = str_replace(inauguration_trim, "T", "Q"),
    election_trim_fixed = str_replace(election_trim, "T", "Q"),
    trim_qtr = as.yearqtr(trim_fixed, format = "%Y-Q%q"),
    inauguration_qtr = as.yearqtr(inauguration_trim_fixed, format = "%Y-Q%q"),
    election_qtr = as.yearqtr(election_trim_fixed, format = "%Y-Q%q")
  ) 


# Merge population data (pop_quarterly must have columns: municipality, trim, pop_tot_qtr)
unemp_voting <- unemp_voting %>%
  left_join(pop_quarterly, by = c("inegi" = "municipality", "trim_fixed" = "trim"))

unemp_voting_coalition <- unemp_voting_coalition %>%
  left_join(pop_quarterly, by = c("inegi" = "municipality", "trim_fixed" = "trim"))

#make data sets for post and pre inauguration 
#pre inauguration will serve as lame duck or falsification test

unemp_voting_perioded <- unemp_voting %>%
  mutate(
    period = case_when(
      trim_qtr >= (election_qtr - 1) & trim_qtr < election_qtr           ~ "pre",        # 2 years prior to election
      trim_qtr >  election_qtr & trim_qtr < inauguration_qtr           ~ "lame_duck",  # between election and inauguration
      trim_qtr >=  inauguration_qtr & trim_qtr <= inauguration_qtr + 2   ~ "post",       # 2 years after inauguration
      TRUE                                                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

unemp_voting_coalition_perioded <- unemp_voting_coalition %>%
  mutate(
    period = case_when(
      trim_qtr >= (election_qtr - 1) & trim_qtr < election_qtr           ~ "pre",
      trim_qtr >  election_qtr & trim_qtr < inauguration_qtr           ~ "lame_duck",
      trim_qtr >=  inauguration_qtr & trim_qtr <= inauguration_qtr + 2   ~ "post",
      TRUE                                                               ~ NA_character_
    )
  ) %>%
  filter(!is.na(period))

# Quarterly average outcomes after inauguration and pre election
make_avg <- function(df) {
  df %>%
    group_by(inegi, inauguration_trim, period) %>%
    summarise(
      avg_unemp_rate       = mean(unemployment_rate, na.rm = TRUE),
      avg_manufac_rate     = mean(manufac_rate, na.rm = TRUE),
      avg_services_rate    = mean(services_rate, na.rm = TRUE),
      avg_construction_rate= mean(construction_rate, na.rm = TRUE),
      avg_commerce_rate    = mean(commerce_rate, na.rm = TRUE),
      avg_share_w_ss       = mean(share_w_social_security, na.rm = TRUE),
      avg_informal_rate    = mean(informal_rate, na.rm = TRUE),
      avg_inc_ineq         = mean(diff_top_bottom, na.rm = TRUE),
      avg_pop_tot          = mean(pop_tot_qtr, na.rm = TRUE),
      morenaWin            = unique(morenaWin),
      margin               = unique(margin),
      pop_tot              = mean(pop_tot_qtr, na.rm = TRUE),
      .groups = "drop"
    )
}

perioded_post      <- make_avg(unemp_voting_perioded %>% filter(period == "post"))     
perioded_pre       <- make_avg(unemp_voting_perioded %>% filter(period == "pre"))       
perioded_lame      <- make_avg(unemp_voting_perioded %>% filter(period == "lame_duck")) 

perioded_post_coal <- make_avg(unemp_voting_coalition_perioded %>% filter(period == "post"))      
perioded_pre_coal  <- make_avg(unemp_voting_coalition_perioded %>% filter(period == "pre"))       
perioded_lame_coal <- make_avg(unemp_voting_coalition_perioded %>% filter(period == "lame_duck")) 


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
    # apply same bandwidth you used earlier (example: 7%)
    filter(abs(margin) <= binwidth)
}

rd_post      <- prep_rd(perioded_post, 0.05)
rd_pre       <- prep_rd(perioded_pre, 0.05)
rd_lame      <- prep_rd(perioded_lame, 0.05)
rd_post_coal <- prep_rd(perioded_post_coal, 0.05)
rd_pre_coal  <- prep_rd(perioded_pre_coal, 0.05)
rd_lame_coal <- prep_rd(perioded_lame_coal, 0.05)

outcomes <- c(
  "avg_unemp_rate", "avg_manufac_rate", "avg_services_rate",
  "avg_construction_rate", "avg_commerce_rate", "avg_share_w_ss",
  "avg_informal_rate", "avg_inc_ineq"
)

# Function to run RD for one outcome
run_rd_single <- function(outcome_var, data, weight_var = "avg_pop_tot", func) {
  if (func == "linear"){
    frm <- as.formula(paste0(outcome_var, " ~ morenaWin + spread_w + spread_l"))
  } else {
    frm <- as.formula(paste0(outcome_var, " ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2"))
  }
  # default to 2-step: try lm with weights; catches errors gracefully
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

# 5) run for all outcomes × (pre/post) and combine into results table
run_all <- function(df, label, func) {
  map_dfr(outcomes, run_rd_single, data = df, weight_var = "avg_pop_tot", func = func) %>%
    mutate(period = label)
}

results_post      <- run_all(rd_post, "post", func = "linear")
results_pre       <- run_all(rd_pre, "pre", func = "linear")
results_lame      <- run_all(rd_lame, "lame_duck", func = "linear")

results_post_coal <- run_all(rd_post_coal, "post", func = "linear")
results_pre_coal  <- run_all(rd_pre_coal, "pre", func = "linear")
results_lame_coal <- run_all(rd_lame_coal, "lame_duck", func = "linear")

results_strict    <- bind_rows(results_pre, results_lame, results_post) %>% mutate(sample = "strict")
results_coalition <- bind_rows(results_pre_coal, results_lame_coal, results_post_coal) %>% mutate(sample = "coalition")

results_all <- bind_rows(results_strict, results_coalition) %>%
  mutate(
    outcome = factor(outcome, levels = rev(outcomes)),
    period = factor(period, levels = c("pre", "lame_duck", "post"))
  )

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

