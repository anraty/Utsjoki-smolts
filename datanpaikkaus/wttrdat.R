library(readxl);library(lubridate);library(tidyverse);library(conflicted)

#   wrnglin wttr data

wtr_r <- read_excel("datanpaikkaus/xlsx-ec4c1478-85a5-4d30-b232-b8e08ec939f6.xlsx",
                    sheet = "Havainnot")



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
  
wttr_hourly <- wtr_all %>% 
  group_by(id) %>% 
  summarise(
    press = mean(press, na.rm =T),
    rain = sum(rain, na.rm =T),
    humi = mean(humi, na.rm =T),
    temp_air = mean(temp_air, na.rm =T),
    wind = mean(wind, na.rm =T)
  )
  


wttr_daily <- wtr_all %>% 
  group_by(date) %>% 
  summarise(
    press = mean(press, na.rm =T),
    rain = sum(rain, na.rm =T),
    humi = mean(humi, na.rm =T),
    temp_air = mean(temp_air, na.rm =T),
    wind = mean(wind, na.rm =T)
  )



