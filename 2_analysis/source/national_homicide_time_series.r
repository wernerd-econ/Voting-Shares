library(haven)
library(tidyverse)
library(ggplot2)

## load in homicide dataset
base_path <- "/Users/wernerd/Desktop/Daniel Werner"
homicide_data <- read_dta(file.path(base_path,"homicides.dta"))

ntl_homicide <- homicide_data %>% 
  group_by(month, year) %>%
  summarise(hom_ntl = sum(homicides),
            pop_ntl = sum(pop_tot),
            ntl_hom_per_tenk = hom_ntl/pop_ntl * 10000,
            avg_mun_hom = mean(homicide_rate_per_tenk),
            year_month = paste0(year, "-", month)) %>%
  distinct(year_month, .keep_all = TRUE)

ntl_homicide$year_month <- as.Date(paste0(ntl_homicide$year_month, "-01"))

p <- ggplot(ntl_homicide, aes(x = year_month)) +
  geom_line(aes(y = ntl_hom_per_tenk, color = "National Homicide Rate")) +
  geom_line(aes(y = avg_mun_hom, color = "Average Municipal Homicide Rate")) +
  annotate("rect",
           xmin = as.Date("2007-01-01"),
           xmax = as.Date("2012-12-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray30", alpha = 0.2) +
  annotate("rect",
           xmin = as.Date("2015-04-01"),
           xmax = as.Date("2020-12-01"),
           ymin = -Inf, ymax = Inf,
           fill = "gray30", alpha = 0.2) +
  geom_hline(yintercept = mean(ntl_homicide$ntl_hom_per_tenk, na.rm = TRUE),
             linetype = "dashed", color = "black", size = 0.5) +
  labs(
    x = "Year-Month",
    y = "Homicide Rate per 10k",
    title = "Homicide Rate Over Time",
    color = "Series"
  ) +
  scale_color_manual(values = c(
    "National Homicide Rate" = "firebrick",
    "Average Municipal Homicide Rate" = "steelblue"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "/Users/wernerd/Desktop/Daniel Werner/Figures/Homicide_TS.png",
       plot = p, width = 8, height = 6, units = "in", dpi = 300, bg = "white")
