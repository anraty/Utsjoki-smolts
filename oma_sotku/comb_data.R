#   joining years from 02 to 21
library(tidyverse)

dat0216_all
dat1721_all

data0221 <- bind_rows(dat0216_all, dat1721_all)

data0221 %>% filter(schools < 1, schools !=0.001)


data0221 %>% filter(smolts == 0, schools != 0.001)
data0221 %>% filter(smolts == 1, schools != 1)
data0221 %>% filter(smolts >= 1, schools < 1)

data0221 <- data0221 %>% mutate(
  schools = if_else(smolts==0, 0.001, schools),
  schools = if_else(smolts==1, 1, schools)
  
)

data0221 %>% filter(smolts == 0, schools != 0)
data0221 %>% filter(smolts == 1, schools != 1)



view(data0221)

#save(data0221, file = "01-Data/smolts_weather0221.RData")
