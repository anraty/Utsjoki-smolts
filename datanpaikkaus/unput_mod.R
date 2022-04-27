library(runjags);library(rjags)

#   Water temp model

#   watertemperature is affected by
#   -   airtemp
#   -   rain

#   data for model
d_wte <- list(
  n = nrow(dat_all_m),
  wtemp = dat_all_m$meanTemp,
  temp_air = dat_all_m$temp_air,
  rain = dat_all_m$rain,
  flow = dat_all_m$flow,
  rbf = dat_all_m$rainbf
  
)

#dat_all_m %>%  filter(is.na(wind))

#   lets make a simple model

wtemp_m <- "model{

  for(i in 1:n){
    
    wtemp[i] ~ dnorm(mu_wt[i], sd_wt^-2)
    mu_wt[i] = a_wt + b_wt[1]*temp_air[i]

    #flow[i] ~ dnorm(mu_fl[i], sd_fl^-2)T(0,)
    #flow[i] ~ dlnorm( M_fl[i], S2_fl^-1 )
    flow[i] ~ dlnorm( log(mu_fl[i])-0.5*log(cv_fl*cv_fl+1), 1/log(cv_fl*cv_fl+1) )
    #M_fl[i] = log(mu_fl[i])-0.5*log(cv_fl*cv_fl+1)
    #S2_fl = log(cv_f*cv_fl+1)
    mu_fl_r[i] =  a_fl + b_fl[1]*wtemp[i] + b_fl[2]*rbf[i] + b_fl[3]*rain[i]
    mu_fl[i] =  ifelse(mu_fl_r[i]>=1, mu_fl_r[i], 1)
    #mu_fl[i] =  ifelse(a_fl + b_fl[1]*wtemp[i]>=1,a_fl + b_fl[1]*wtemp[i], 1  )
    
  }
  
  for(i in 1:1){
    b_wt[i] ~ dnorm(0, 100^-2)
  }

  for(i in 1:3){
    b_fl[i] ~ dnorm(0, 100^-2)
  }

  a_wt ~ dnorm(0, 100^-2)
  a_fl ~ dnorm(0, 100^-2)
  
  sd_wt ~ dnorm(0, 100^-2)T(0,)
  #sd_fl ~ dnorm(0, 100^-2)T(0,)
  cv_fl ~ dunif(0.01, 10)
  #S2_fl = log(cv_fl*cv_fl+1)
  
    

}"

para <- c("wtemp", "flow")
para <- c("a_wt","a_fl", "b_wt", "b_fl", "sd_wt", "cv_fl")

res <- run.jags(model = wtemp_m, data = d_wte, sample = 50000, 
                monitor = para,
                method = "parallel")

sres <- as.data.frame(summary(res))
sres


wtf<-sres %>% filter(SD>1)
view(wtf)

#summary(res)
