#   experimenting with new raw data
library(readxl);library(lubridate);library(tidyverse);library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

sm_r <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2017.xlsx")

#   smolts
sm <- sm_r %>%
  select(Date, Klo, Smolt) %>%
  transmute(
    date = as_date(Date),
    klo = format(Klo, format="%H:%M:%S"),
    smolt = Smolt,
    id = as.POSIXct(paste(date,klo), format = "%Y-%m-%d%H"))

#   smolts by hour
smolt_hourly <- sm %>% 
  group_by(id) %>% 
  summarise(smolt = sum(smolt, na.rm =T))

#   smolts by day
smolt_daily <- sm %>% 
  group_by(date) %>% 
  summarise(smolt = sum(smolt, na.rm =T))


t_r <- read_excel("01.5-Data_raw/Utsjoki_veden lämpö_2016-2017.xlsx", skip = 1)
#   temperatures hourly
td <- t_r %>% 
  select(`Date Time, GMT+03:00`, `Temp, °C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`) %>% 
  transmute(
    date = as_date(`Date Time, GMT+03:00`),
    temp = `Temp, °C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`,
    id = `Date Time, GMT+03:00`
  )

temp_daily <- td %>%
  filter(temp<20) %>% 
  group_by(date) %>% 
  summarise(temp = mean(temp))

d_r <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx")
#   discharge
d <- d_r %>% 
  select_all() %>% 
  transmute(
    date = as.POSIXct(paste(Vuosi, Kuukausi, Päivä), format = "%Y %m %d"),
    disc = Virtaama
  ) 


data17 <- left_join(smolt_daily, temp_daily, by ="date") %>% 
  left_join(., d, by ="date") 

  







