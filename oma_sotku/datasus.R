#   creating data for susi MACHINE

source("oma_sotku/wrng-functions.R")
library(runjags);library(rjags)

load("01-Data/smolts_weather0221.RData")


nls20xtra <- read_excel("01.5-Data_raw/Utsjoki_lisäkamerat_kalat 2020_final_08102020.xlsx") %>% 
  select(Date, Smolt) %>% 
  transmute(
    date = as_date(Date),
    side = Smolt
  ) %>% 
  group_by(date) %>% 
  summarise(
    side = sum(side, na.rm =T) 
  )

dat <- data0221 %>% left_join(nls20xtra, by = "date")



years<-c(2011, 2017, 2020)
n_days<-61

df<-s_dat_jags(dat,years, n_days) # 61: only june & july

data<-list(
  nDays=n_days,
  nYears=length(years),
  s=df$Schools,
  flow=df$Flow,
  flow_std = (df$Flow-182)/102,
  Nobs=df$Smolts,
  Nobs_side=df$side,
  Temp=df$Temp,
  Temp_air = df$Temp_air,
  Rain = df$Rain,
  Rain_bf = df$Rain_bf
)


data$flow_std

mean(data$flow, na.rm =T)
save(data, file = "Susikoneelle/data.RData")
