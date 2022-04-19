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
  summarise(schools = sum(!is.na(smolt)),
            smolt = sum(smolt, na.rm =T)) %>% 
  select(id, smolt, schools)

#   smolts by day
smolt_daily <- sm %>% 
  group_by(date) %>% 
  summarise(schools = sum(!is.na(smolt)),
            smolt = sum(smolt, na.rm =T)) %>% 
  select(date, smolt, schools)


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



#   wrnglin wttr data

wtr_r <- read_excel("datanpaikkaus/wttr_17.xlsx",
                    sheet = "Havainnot")


#   all wetaher
wtr_all <- wtr_r %>%  
  select_all() %>% 
  transmute(
    date = as_date(paste(Vuosi, Kk, Pv)),
    klo = format(Klo, format = "%H:%M"),
    press = `Ilmanpaine (msl) (hPa)`,
    rain = `Sademäärä (mm)`,
    humi = `Suhteellinen kosteus (%)`,
    temp_air = `Ilman lämpötila (degC)`,
    wind = `Tuulen nopeus (m/s)`,
    id = as.POSIXct(paste(date,klo), format = "%Y-%m-%d%H")
  )  

#   hourly weather
wttr_hourly <- wtr_all %>% 
  group_by(id) %>% 
  summarise(
    press = mean(press, na.rm =T),
    rain = sum(rain, na.rm =T),
    humi = mean(humi, na.rm =T),
    temp_air = mean(temp_air, na.rm =T),
    wind = mean(wind, na.rm =T)
  )


#   daily weather
wttr_daily <- wtr_all %>% 
  group_by(date) %>% 
  summarise(
    press = mean(press, na.rm =T),
    rain = sum(rain, na.rm =T),
    humi = mean(humi, na.rm =T),
    temp_air = mean(temp_air, na.rm =T),
    wind = mean(wind, na.rm =T)
  )

#   getting amount of days from last rain

rainbf <- c()
for(i in 1:length(wttr_daily$rain)){
  
  if(wttr_daily$rain[i] > 0){
    rainbf[i] <- 0
    print("sataa")
  }
  
  else{
    print("ei sada")
    count = 0
    bf = 1
    if(i-bf == 1){
      print("alussa")
      count = bf
    }
    else{
      while (wttr_daily$rain[i-bf]==0) {
        print("ei sada vieläkään")
        bf = bf+1
      }
      print("satoi")
      count = bf
    }
    rainbf[i] = count
  }
}


wttr_daily %<>%
  select_all() %>% 
  mutate(
    rainbf = rainbf
  )


data17_all <- left_join(smolt_daily, temp_daily, by ="date") %>% 
  left_join(., d, by ="date") %>%  
  left_join(., wttr_daily, by = "date")

  


data17 <- data17_all %>% 
  select_all() %>% 
  mutate(
    Year = format(date, format("%Y")),
    Month = format(date, format("%m")),
    Day = format(date, format("%d")),
    flow = disc,
    smolts = smolt,
    meanTemp = temp
  ) %>% 
  select(Year, Month, Day, smolts, schools, flow, meanTemp)
   








