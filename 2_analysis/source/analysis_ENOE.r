library(lubridate)
library(zoo)
library(readxl)

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
    inauguration_date = date %m+% months(3),
    # convert to year-quarter
    inauguration_qtr = as.yearqtr(inauguration_date),
    # shift by one quarter
    inauguration_qtr_lag1 = inauguration_qtr + 1/4,
    # format back into "YYYY-QX"
    inauguration_trim = format(inauguration_qtr_lag1, "%Y-Q%q")
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
    inauguration_trim
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
    inauguration_trim
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
  filter(year >= 2019, year <= 2024)

stack <- stack %>%
  mutate(
    unemployed = ifelse(clase1 == 1 & clase2 == 2, 1, 0),
    employed = ifelse(clase1 == 1 & clase2 == 1, 1, 0), 
    PEA = ifelse(clase1 == 1, 1, 0),
    manufac = ifelse(PEA * rama == 2, 1, 0),
    apoyo_economico = ifelse(p14apoyos == "Sí recibe apoyos económicos", 1, 0),
    no_apoyo_economico = ifelse(p14apoyos == "No recibe apoyos económicos", 1, 0),
    empleo_informal = ifelse(emp_ppal == 1, 1, 0),
  )



# Collapse to municipality-year-month level
collapsed <- stack %>%
  group_by(municipality, trim) %>%
  summarise(
    total_unemployed = sum(unemployed * weights, na.rm = TRUE),
    total_employed = sum(employed * weights, na.rm = TRUE),
    total_manufac = sum(manufac * weights, na.rm = TRUE),
    total_recieve_help = sum(apoyo_economico * weights, na.rm = TRUE),
    total_informal = sum(empleo_informal * weights, na.rm = TRUE),
    total_people = sum((apoyo_economico+no_apoyo_economico)*weights, na.rm = TRUE),
    total_PEA = sum(PEA * weights, na.rm = TRUE),
    unemployment_rate = ifelse(total_PEA > 0, total_unemployed / total_PEA, NA) * 100,
    manufac_rate = ifelse(total_employed > 0, total_manufac / total_employed, NA) * 100,
    share_recieve_help = ifelse(total_people > 0, total_recieve_help/ total_people, NA) * 100,
    informal_rate = ifelse(total_employed > 0, total_informal / total_employed, NA) * 100,
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
    trim_qtr = as.yearqtr(trim_fixed, format = "%Y-Q%q"),
    inauguration_qtr = as.yearqtr(inauguration_trim_fixed, format = "%Y-Q%q")
  ) %>%
  filter(trim_qtr > inauguration_qtr & trim_qtr <= inauguration_qtr + 2)

unemp_voting_coalition <- unemp_voting_coalition %>%
  mutate(
    trim_fixed = str_replace(trim, "T", "Q"),
    inauguration_trim_fixed = str_replace(inauguration_trim, "T", "Q"),
    trim_qtr = as.yearqtr(trim_fixed, format = "%Y-Q%q"),
    inauguration_qtr = as.yearqtr(inauguration_trim_fixed, format = "%Y-Q%q")
  ) %>%
  filter(trim_qtr > inauguration_qtr & trim_qtr <= inauguration_qtr + 2)

# Merge population data (pop_quarterly must have columns: municipality, trim, pop_tot_qtr)
unemp_voting <- unemp_voting %>%
  left_join(pop_quarterly, by = c("inegi" = "municipality", "trim_fixed" = "trim"))

unemp_voting_coalition <- unemp_voting_coalition %>%
  left_join(pop_quarterly, by = c("inegi" = "municipality", "trim_fixed" = "trim"))



# Quarterly average unemployment rate and population after inauguration (strict)
unemp_voting_avg <- unemp_voting %>%
  group_by(inegi, inauguration_trim) %>%
  summarise(
    avg_unemp_rate = mean(unemployment_rate, na.rm = TRUE),
    avg_manufac_rate = mean(manufac_rate, na.rm = TRUE),
    avg_informal_rate = mean(informal_rate, na.rm = TRUE),
    avg_inc_ineq = mean(diff_top_bottom, na.rm = TRUE),
    avg_pop_tot = mean(pop_tot_qtr, na.rm = TRUE),
    morenaWin = unique(morenaWin),
    margin = unique(margin),
    pop_tot = mean(pop_tot_qtr, na.rm = TRUE),
    .groups = "drop"
  )

# Quarterly average unemployment rate and population after inauguration (coalition)
unemp_voting_avg_coalition <- unemp_voting_coalition %>%
  group_by(inegi, inauguration_trim) %>%
  summarise(
    avg_unemp_rate = mean(unemployment_rate, na.rm = TRUE),
    avg_manufac_rate = mean(manufac_rate, na.rm = TRUE),
    avg_informal_rate = mean(informal_rate, na.rm = TRUE),
    avg_inc_ineq = mean(diff_top_bottom, na.rm = TRUE),
    avg_pop_tot = mean(pop_tot_qtr, na.rm = TRUE),
    morenaWin = unique(morenaWin),
    margin = unique(margin),
    pop_tot = mean(pop_tot_qtr, na.rm = TRUE), 
    .groups = "drop"
  )

##RD
rd_data <- unemp_voting_avg %>%
  mutate(
    spread_w = margin * morenaWin,       
    spread_l = margin * (1 - morenaWin), 
    spread_w2 = spread_w^2,
    spread_l2 = spread_l^2
  )

rd_model <- lm(
  avg_inc_ineq ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data,
  weights = pop_tot
)

summary(rd_model)

rd_data_bw5 <- rd_data %>%
  filter(abs(margin) < 0.07)

rd_model_bw5 <- lm(
  avg_inc_ineq ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data_bw5,
  weights = pop_tot
)

summary(rd_model_bw5)

rd_data_coalition <- unemp_voting_avg_coalition %>%
  mutate(
    spread_w = margin * morenaWin,       
    spread_l = margin * (1 - morenaWin), 
    spread_w2 = spread_w^2,
    spread_l2 = spread_l^2
  )

rd_model_coalition <- lm(
  avg_inc_ineq ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data_coalition,
  weights = pop_tot
)

summary(rd_model_coalition)

rd_data_bw5_coalition <- rd_data_coalition %>%
  filter(abs(margin) < 0.05)

rd_model_bw5_coalition <- lm(
  avg_inc_ineq ~ morenaWin + spread_w + spread_l + spread_w2 + spread_l2,
  data = rd_data_bw5_coalition,
  weights = pop_tot
)

summary(rd_model_bw5_coalition)
