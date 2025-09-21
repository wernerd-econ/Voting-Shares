## =============================================================== ##
        # This script takes in the raw cocaine seizure data
        # and processes it to create a clean dataset of metric tons
        # seized by year and month.
## =============================================================== ##

library(haven)
library(tidyverse)
library(readxl)

setwd("/Users/wernerd/Desktop/Daniel Werner/Cocaine")
#Cocaine Seizures - Base Cocaine and Cocaine

coca <- read_excel("Cocaina.xlsx")
base_coca <- read_excel("base.cocaina.xlsx")

coca <- coca %>% mutate(`Type of Seizure` = "coca")
base_coca <- base_coca %>% mutate(`Type of Seizure` = "base coca") %>%
  rename(`UNIDAD DE MEDIDA`=`UNIDADES DE MEDIDA`)
total_seizures <- rbind(coca,base_coca)

#Remove Seizures With Quantity of 0 KG

total_seizures <- total_seizures %>% filter(`CANTIDAD` != 0)

#Extract Year and Month

total_seizures <- total_seizures %>%
  mutate(
    Year = format(`FECHA HECHO`, "%Y"),
    Month = format(`FECHA HECHO`, "%m") 
  )

#Filter For Relevant Years

total_seizures <- total_seizures %>% mutate(Year=as.numeric(Year)) %>%
  filter(Year %in% c(2010:2021))

#List of Departments On Atlantic/Pacific Coast

ap <- c("LA GUAJIRA", "MAGDALENA", "ATLANTICO", "BOLIVAR", "SUCRE",
        "CORDOBA", "ANTIOQUIA", "CHOCO", "VALLE DEL CAUCA", "CAUCA",
        "NARIÑO", "SAN ANDRES ISLAS")

#Make Relevant Monthly Seizure Statistics

total_seizures <- total_seizures %>% group_by(Year, Month) %>%
  mutate(ts = sum(`CANTIDAD`)/1000,
         ts_n = n(),
         ts_big = sum(`CANTIDAD`[`CANTIDAD` >= 500])/1000,
         ts_big_n = sum(`CANTIDAD` >= 500),
         cs = sum(`CANTIDAD`[`DEPARTAMENTO` %in% ap])/1000,
         cs_n = sum(`DEPARTAMENTO` %in% ap),
         cs_big = sum(`CANTIDAD`[`CANTIDAD` >= 500 & `DEPARTAMENTO` %in% ap])/1000,
         cs_big_n = sum(`CANTIDAD` >= 500 & `DEPARTAMENTO` %in% ap)
  ) %>%
  ungroup()

total_seizures <- total_seizures %>% distinct(Year, Month, .keep_all = TRUE)

inc <- read_excel("Incautaciones.xlsx")

inc <- inc %>% filter(`CLASE ELEMENTO` == "COCAINA Y DERIVADOS")
inc <- inc %>% filter(`ELEMENTO` %in% c("BASE DE COCA", "CLORHIDRATO DE COCAINA"))
inc <- inc %>% filter(`CANTIDAD` != 0)

#Extract Year and Month

inc <- inc %>%
  mutate(
    Year = format(`FECHA`, "%Y"),
    Month = format(`FECHA`, "%m") 
  )

#Filter For Relevant Years

inc <- inc %>% mutate(Year=as.numeric(Year)) %>%
  filter(Year %in% c(2007:2009))

inc <- inc %>% group_by(Year, Month) %>%
  mutate(ts = sum(`CANTIDAD`)/1000,
         ts_n = n(),
         ts_big = sum(`CANTIDAD`[`CANTIDAD` >= 500])/1000,
         ts_big_n = sum(`CANTIDAD` >= 500),
         cs = sum(`CANTIDAD`[`DEPARTAMENTO` %in% ap])/1000,
         cs_n = sum(`DEPARTAMENTO` %in% ap),
         cs_big = sum(`CANTIDAD`[`CANTIDAD` >= 500 & `DEPARTAMENTO` %in% ap])/1000,
         cs_big_n = sum(`CANTIDAD` >= 500 & `DEPARTAMENTO` %in% ap)
  ) %>%
  ungroup() %>% distinct(Year, Month, .keep_all = TRUE)

### Merge the two datasets
inc <- inc %>% select(Year, Month, ts, ts_n, ts_big, ts_big_n,
                      cs, cs_n, cs_big, cs_big_n)

total_seizures <- total_seizures %>% select(Year, Month, ts, ts_n, ts_big,
                                            ts_big_n, cs, cs_n, cs_big, cs_big_n) %>% 
  mutate(Year = as.numeric(Year), Month=as.character(Month))

seizure_data <- bind_rows(inc, total_seizures)

write_dta(seizure_data, "/Users/wernerd/Desktop/Daniel Werner/seizure_data.dta")