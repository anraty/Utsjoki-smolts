#   doind functio for datawrangle

library(tidyverse);library(conflicted);library(readxl);library(lubridate)
conflict_prefer("select", "dplyr");conflict_prefer("filter", "dplyr")


sustemp <- function(temp,oal = 15, dff = -5){
  n = length(temp)
  nas <- which(is.na(temp))
  for(i in nas){temp[i] <- temp[i-1]} 
  sus = c()
  if(temp[1] > oal){sus[1] <- T}
  else{sus[1] <- F}
  for(i in 2:n){
    if( temp[i]-temp[i-1] < dff ){
      count = 0
      j = 1
      diff = temp[i]-temp[i-1]
      sus[i] <- F
      while(diff <  dff){
        j = j+1
        if(i-j == 0){diff = 0}
        else{
        diff = temp[i]-temp[i-j]
        sus[i-j] <- T}
      }
    }
    else{sus[i] <- F}
  }
  return(sus)
}


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
    summarise(n_schools = sum(!is.na(smolt)),
              smolt = sum(smolt, na.rm =T),
              school_size = smolt/n_schools) %>% 
    select(id, smolt, n_schools, school_size)
  
  #   smolts by day
  smolt_daily <- sm %>% 
    group_by(date) %>% 
    summarise(n_schools = sum(!is.na(smolt)),
              smolt = sum(smolt, na.rm =T),
              school_size = smolt/n_schools) %>% 
    select(date, smolt, n_schools, school_size)
  
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
  td <- td %>% 
    mutate(
      sus = sustemp(temp)
    )
  
  temp_daily <- td %>%
    filter(!(sus)) %>% 
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
      schools = if_else(school_size == "NaN", 0.001, school_size),
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


smdwrg_m <- function(d_nls, d_wtemp, d_disc, d_wttr, read_data = F){
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
    summarise(n_schools = sum(smolt>0),
              #n_schools = sum(!is.na(smolt)),
              smolt = sum(smolt, na.rm =T),
              school_size = sum(smolt)/n_schools) %>% 
    select(id, smolt, n_schools, school_size)
  
  #   smolts by day
  smolt_daily <- sm %>% 
    group_by(date) %>% 
    summarise(n_schools = sum(smolt>0),
              #n_schools = sum(!is.na(smolt)),
              smolt = sum(smolt, na.rm =T),
              school_size = sum(smolt)/n_schools) %>% 
    select(date , smolt, n_schools, school_size)
  
  smolt_daily <- smolt_daily %>% mutate(
    day = cumsum(!is.na(date))
  ) 
  
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
  td <- td %>% 
    mutate(
      sus = sustemp(temp)
    )
  
  temp_daily <- td %>%
    filter(!(sus)) %>% 
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
      Year = as.numeric(format(date, format("%Y"))),
      Month = as.numeric(format(date, format("%m"))),
      Day = as.numeric(format(date, format("%d"))),
      day = day,
      flow = disc,
      smolts = smolt,
      schools = school_size,
      #schools = if_else(school_size == "NaN", 0.001, school_size),
      meanTemp = temp,
      disc = NULL,
      smolt = NULL,
      temp = NULL
    ) %>% 
    select(Year, Month, Day, day, smolts, schools, flow, meanTemp, date, 
           press, rain, humi, temp_air, rainbf)
  print("Data combined!")
  data <- data_all %>% 
    select(Year,
           Month,
           Day,
           day,
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

#k <- smdwrg_m(nls19, wtemp19, disc19, wttr19)


s_dat_jags <- function(dat, years, days){
  nYears <- length(years)
  nDays <- days
  #   filter data first
  #   needed variables: smolts, schools, flow, temp, temp_air, rain, rainbf
  dat_f <- dat %>% filter(Year %in% years, day <= days) %>% group_by(Year) %>% 
    group_split(keep=F) %>% unlist(recursive = F) %>% as.data.frame() 
  
  data = list(
    nYears = nYears,
    nDays = nDays,
    Smolts = dat_f %>% select(matches("smolts\\.|smolts$")) %>% as.matrix(),
    Schools = dat_f %>% select(matches("schools\\.|schools$")) %>% as.matrix(),
    Flow = dat_f %>% select(matches("flow\\.|flow$")) %>% as.matrix(),
    Temp = dat_f %>% select(matches("meanTemp\\.|meanTemp$")) %>% as.matrix(),
    Temp_air = dat_f %>% select(matches("temp_air\\.|temp_air$")) %>% as.matrix(), 
    Rain = dat_f %>% select(matches("rain\\.|rain$")) %>% as.matrix(),
    Rain_bf = dat_f %>% select(matches("rainbf\\.|rainbf$")) %>% as.matrix()
  )
  return(data)
}



