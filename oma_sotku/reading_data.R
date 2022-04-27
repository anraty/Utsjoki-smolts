library(tidyverse);library(conflicted);library(readxl);library(lubridate)
conflict_prefer("select", "dplyr");conflict_prefer("filter", "dplyr")




nls17 <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2017.xlsx")
wtemp17 <- read_excel("01.5-Data_raw/Utsjoki_veden lämpö_2016-2017.xlsx", skip = 1)%>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc17 <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx")
wttr17 <- read_excel("datanpaikkaus/wttr_17.xlsx", sheet = "Havainnot")

data17 <- smdwrg_m(nls17, wtemp17, disc17, wttr17)
#view(data17[[1]])


nls18 <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2018.xlsx") %>% 
  rename(Klo = Hours)
wtemp18 <- read_excel("01.5-Data_raw/Utsjoki2018.xls", skip = 1) %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc18 <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx")
wttr18 <- read_excel("datanpaikkaus/wttr_18.xlsx", sheet = "Havainnot")

data18 <- smdwrg_m(nls18, wtemp18, disc18, wttr18)
#view(data18[[2]])


nls19 <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2019.xlsx") %>% 
  rename(Klo = Hours)
wtemp19 <- read_excel("01.5-Data_raw/Utsjoki_veden lämpö_2018-2019.xlsx", skip = 1) %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc19 <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx")
wttr19 <- read_excel("datanpaikkaus/wttr_19.xlsx", sheet = "Havainnot")

data19 <- smdwrg_m(nls19, wtemp19, disc19, wttr19)
#view(data19[[2]])


nls20 <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2020_FINAL.xlsx") %>% 
  rename(Klo = Hours)
wtemp20 <- read_excel("01.5-Data_raw/Utsjoki2020.xlsx", skip = 1) %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc20 <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2020.xlsx")
wttr20 <- read_excel("datanpaikkaus/wttr_20.xlsx", sheet = "Havainnot")

data20 <- smdwrg_m(nls20, wtemp20, disc20, wttr20)
#view(data20[[2]])

nls21 <- read_excel("01.5-Data_raw/Utsjoki_nousulohet ja smoltit_2021_30112021.xlsx") %>% 
  rename(Klo = Hours)
wtemp21 <- read_excel("01.5-Data_raw/Utsjoki2021.xlsx", skip = 1) %>% 
  mutate(`Date Time, GMT+03:00` = as.POSIXct(`Date Time, GMT+03:00`, format= "%d.%m.%y" ))
disc21 <- read_excel("01.5-Data_raw/Virtaama_Patoniva 1963-2021.xlsx")
wttr21 <- read_excel("datanpaikkaus/wttr_21.xlsx", sheet = "Havainnot")

data21 <- smdwrg_m(nls21, wtemp21, disc21, wttr21)


dat1721_all <- bind_rows(data17[[2]], data18[[2]]) %>% 
  bind_rows(data19[[2]]) %>% 
  bind_rows(data20[[2]]) %>% 
  bind_rows(data21[[2]])


dat1721 <- bind_rows(data17[[1]], data18[[1]]) %>% 
  bind_rows(data19[[1]]) %>% 
  bind_rows(data20[[1]]) %>% 
  bind_rows(data21[[1]])
#view(data21[[2]])
#view(dat1721_all)
#dat0216_all

#view(data17[[1]])
#view(data18[[1]])
#view(data19[[1]])
#view(data20[[1]])
#view(data21[[1]])

#table(is.na(data17[[1]]))#    has some na
#table(is.na(data18[[1]]))#    has some na
#table(is.na(data19[[1]]))
#table(is.na(data20[[1]]))
#table(is.na(data21[[1]]))

#table(is.na(wtemp17$`Temp, °C (LGR S/N: 10754831, SEN S/N: 10754831, LBL: Water)`))
#table(is.na(wtemp18$`Temp, °C (LGR S/N: 20155736, SEN S/N: 20155736)`))
#table(is.na(wtemp19$`Temp, °C (LGR S/N: 20155736, SEN S/N: 20155736)`))
#table(is.na(wtemp20$`Temp, °C (LGR S/N: 10754830, SEN S/N: 10754830, LBL: Temp)`))
#table(is.na(wtemp21$`Temp, °C (LGR S/N: 10754830, SEN S/N: 10754830, LBL: Temp)`))
