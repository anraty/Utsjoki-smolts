#   doind functio for datawrangle
library(tidyverse);library(conflicted)
conflict_prefer("select", "dplyr");conflict_prefer("filter", "dplyr")


smdwrg <- function(d_nls, d_wtemp, d_disc, d_wttr, read_data = F){
  #   first reading in all the datas from excel
  
  #   nousulohet ja smoltit (migrating salmons and smolts)
  if(read_data == T){
    sm_r <- read_excel(d_nls)
    print("Smolts OK")
    #   veden lämpötila (water temp)
    t_r <- read_excel(d_wtemp, skip = 1)
    print("Water temp OK")
    #   virtaama (discharge)
    d_r <- read_excel(d_disc)
    print("Discharge OK")
    #   sää (weather)
    wtr_r <- read_excel(d_wttr, sheet = "Havainnot")
    print("Weather OK")
  }
  else{
    sm_r <- d_nls
    t_r <- d_wtemp
    d_r <- d_disc
    wtr_r <- d_wttr
  }
  
  #   start of wranglinng

  #   SMOLTS    #
  ###############
  
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
  
  print("Smolts wrangling done!")
  
  #   WATER TEMP    #
  ###################
  #   temperatures hourly
  colnames(t_r) <- c("ind","DT", "TEMP")
  td <- t_r %>% 
    #select(`Date Time, GMT+03:00`, `Temp, °C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`) %>% 
    select(DT, TEMP) %>% 
    transmute(
      #date = as_date(`Date Time, GMT+03:00`),
      date = as_date(DT),
      #temp = `Temp, °C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`,
      temp = as.numeric(TEMP),
      id = DT
    )
  
  temp_daily <- td %>%
    filter(temp<20) %>% 
    group_by(date) %>% 
    summarise(temp = mean(temp))
  
  
  #return(list(td, temp_daily))
  
  print("Water temperature wrangling done!")
  #   DISCHARGE   #
  #################
  d <- d_r %>% 
    select_all() %>% 
    transmute(
      date = as.POSIXct(paste(Vuosi, Kuukausi, Päivä), format = "%Y %m %d"),
      disc = Virtaama
    ) 
  
  print("Discharge wrangling done!")
  #   WEATHERDATA   #
  ###################
  #   all weather
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
    }
    else{
      count = 0
      bf = 1
      if(i-bf == 1){
        count = bf
      }
      else{
        while (wttr_daily$rain[i-bf]==0) {
          bf = bf+1
        }
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
  
  print("Weather wrangling done!")
  #   colllecting data together
  
  data_all <- left_join(smolt_daily, temp_daily, by ="date") %>% 
    left_join(., d, by ="date") %>%  
    left_join(., wttr_daily, by = "date") %>% 
    select_all() %>% 
    mutate(
      Year = format(date, format("%Y")),
      Month = format(date, format("%m")),
      Day = format(date, format("%d")),
      flow = disc,
      smolts = smolt,
      meanTemp = temp,
      date = NULL,
      disc = NULL,
      smolt = NULL,
      temp = NULL
    ) %>% 
    select(Year, Month, Day, smolts, schools, flow, meanTemp, everything())
  print("Data combined!")
  data <- data_all %>% 
    select(Year,
           Month,
           Day,
           flow,
           smolts,
           schools,
           meanTemp)
  
  return(
    list(
      data,
      data_all
    )
  )
  
}

nls17 <- "01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2017.xlsx"
wtemp17 <- "01.5-Data_raw/Utsjoki_veden lämpö_2016-2017.xlsx"
disc17 <- "01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx"
wttr17 <- "datanpaikkaus/wttr_17.xlsx"

data17 <- smdwrg(nls17, wtemp17, disc17, wttr17, read_data = T)
view(data17[[2]])

nls18 <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2018.xlsx") %>% 
  rename(Klo = Hours)
wtemp18 <- read_excel("01.5-Data_raw/Utsjoki2018.xls", skip = 1) %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc18 <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx")
wttr18 <- read_excel("datanpaikkaus/wttr_18.xlsx", sheet = "Havainnot")

data18 <- smdwrg(nls18, wtemp18, disc18, wttr18)

view(data18[[2]])



