#   making testdata for discmodel


d17 <- data17_all[1:47,]
n <- nrow(d17)
ids <- sample(1:n, n*0.3)

#   data with some hidden
testdat <- d17 %>% 
  mutate(
    id = row_number(),
    disc_m = if_else(id %in% ids, NA_real_, disc)
    
    
  )


testdata <- list(
  
  n = length(testdat$date),
  disc = testdat$disc_m,
  rainbf = testdat$rainbf,
  rain = testdat$rain,
  humi = testdat$humi,
  wind = testdat$wind,
  temp = testdat$temp,
  temp_air = testdat$temp_air
  
)

)

vseld <- list(
  n = length(d17$date),
  temp = d17$temp,
  disc = d17$disc,
  press = d17$press,
  rain = d17$rain,
  humi = d17$humi,
  temp_air = d17$temp_air,
  wind = d17$wind,
  rainbf = d17$rainbf
)
