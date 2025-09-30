library(tidyverse)
library(stringr)
library(haven)

base_path <- "/Users/wernerd/Desktop/Voting Shares Raw"

voting <- read.csv(file.path(base_path, "voting.csv"))

voting$date <- as.Date(as.character(voting$date), format = "%Y%m%d")

voting <- voting %>%
  mutate(
    inauguration_date = date %m+% months(3)
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
    inauguration_date
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
    inauguration_date
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
  filter(yr >= 2019 & yr <= 2021)

margin_coalition_included <- margin_coalition_included %>%
  filter(yr >= 2019 & yr <= 2021)

homicides_voting <- margin_strict %>%
  left_join(homicides, by = c("inegi" = "municipality"))

homicides_voting_coalition <- margin_coalition_included %>%
  left_join(homicides, by = c("inegi" = "municipality"))

homicides_voting <- homicides_voting %>%
  filter(date > inauguration_date,
         date <= inauguration_date + months(12))

homicides_voting_coalition <- homicides_voting_coalition %>%
  filter(date > inauguration_date,
         date <= inauguration_date + months(12))

homicides_voting_avg <- homicides_voting %>%
  group_by(inegi, inauguration_date) %>%
  summarise(
    n = n(),
    avg_homicide_rate_next12m = mean(homicides, na.rm = TRUE)/mean(pop_tot) * 12 * 100000,
    pop_tot = mean(pop_tot),
    morenaWin = unique(morenaWin),
    margin = unique(margin),
    .groups = "drop"
  )

homicides_voting_avg_coalition <- homicides_voting_coalition %>%
  group_by(inegi, inauguration_date) %>%
  summarise(
    n = n(),
    avg_homicide_rate_next12m = mean(homicides, na.rm = TRUE)/mean(pop_tot) * 12 * 100000,
    pop_tot = mean(pop_tot),
    morenaWin = unique(morenaWin),
    margin = unique(margin),
    .groups = "drop"
  )


############################### RD ############################################
rd_data <- homicides_voting_avg %>%
  rename(hom_rate = avg_homicide_rate_next12m) %>%
  mutate(
    spread_w = margin * morenaWin,       
    spread_l = margin * (1 - morenaWin), 
    spread_w2 = spread_w^2,
    spread_l2 = spread_l^2
  )

rd_model <- lm(
  hom_rate ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data,
  weights = pop_tot
)

summary(rd_model)

rd_data_bw5 <- rd_data %>%
  filter(abs(margin) < 0.02)

rd_model_bw5 <- lm(
  hom_rate ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data_bw5,
  weights = pop_tot
)

summary(rd_model_bw5)

rd_data_coalition <- homicides_voting_avg_coalition %>%
  rename(hom_rate = avg_homicide_rate_next12m) %>%
  mutate(
    spread_w = margin * morenaWin,       
    spread_l = margin * (1 - morenaWin), 
    spread_w2 = spread_w^2,
    spread_l2 = spread_l^2
  )

rd_model_coalition <- lm(
  hom_rate ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data_coalition,
  weights = pop_tot
)

summary(rd_model_coalition)

rd_data_bw5_coalition <- rd_data_coalition %>%
  filter(abs(margin) < 0.05)

rd_model_bw5_coalition <- lm(
  hom_rate ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data_bw5_coalition,
  weights = pop_tot
)

summary(rd_model_bw5_coalition)
