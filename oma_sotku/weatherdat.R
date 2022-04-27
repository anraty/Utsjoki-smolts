#   reading weathers from 2002 to 2016
#   and compiling together

w0204 <- read_excel("datanpaikkaus/wttr_0204.xlsx", sheet = "Havainnot")
w0507 <- read_excel("datanpaikkaus/wttr_0507.xlsx", sheet = "Havainnot")
w0810 <- read_excel("datanpaikkaus/wttr_0810.xlsx", sheet = "Havainnot")
w1113 <- read_excel("datanpaikkaus/wttr_1113.xlsx", sheet = "Havainnot")
w1416 <- read_excel("datanpaikkaus/wttr_1416.xlsx", sheet = "Havainnot")

wttr_0216 <- bind_rows(w0204, w0507) %>% 
  bind_rows(., w0810) %>% 
  bind_rows(.,w1113) %>% 
  bind_rows(.,w1416) %>% 

  transmute(
    date = as_date(paste(Vuosi, Kk, Pv)),
    klo = format(Klo, format = "%H:%M"),
    press = `Ilmanpaine (msl) (hPa)`,
    rain = if_else(`Sademäärä (mm)` < 0, 0, `Sademäärä (mm)`),
    humi = `Suhteellinen kosteus (%)`,
    temp_air = `Ilman lämpötila (degC)`,
    wind = `Tuulen nopeus (m/s)`,
    id = as.POSIXct(paste(Vuosi, Kk, Pv,klo), format = "%Y-%m-%d%H")
  ) %>% 
  
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
frain = FALSE
for(i in 1:length(wttr_0216$rain)){
  print(i)
  if(wttr_0216$rain[i] > 0){
    rainbf[i] <- 0
    frain = TRUE
  }
  else if(frain == FALSE){rainbf[i] <- 0}
  else{
    bf = 1
    flag = F
    while (flag == F) {
        if(i-bf <= 1){ 
          #print("alussa")
          flag = T}
        else{ 
          #print("läpi")
          if(wttr_0216$rain[i-bf] == 0){ bf = bf+1;flag = F}
          else if(wttr_0216$rain[i-bf] != 0){ flag = T}
        }
    }
    rainbf[i] = bf
    }
    
  }


wttr_0216 %<>%
  select_all() %>% 
  mutate(
    rainbf = rainbf
  ) 


dat0216_all <- dat_all %>% 
  mutate(
  date = as_date(paste(Year, Month, Day))
  ) %>% 
  left_join(., wttr_0216, by = "date")
