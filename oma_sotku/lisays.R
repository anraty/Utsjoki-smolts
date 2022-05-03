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



d20xtra <- data0221 %>% 
  filter(Year==2020) %>% 
  left_join(nls20xtra, by  = "date")

M1<-"
model{
  
  # Imputing missing values of flow and water temperature using weatherdata
  # ==========================================================================
  for(y in 1:nYears){
    for(i in 1:nDays ){
      #   Water temperature is estimated from air temperature
      Temp[i,y] ~ dnorm(mu_temp[i,y], sd_temp^-2)
      mu_temp[i,y] = a_temp + b_temp[1]*Temp_air[i,y]
      
      #   Flow is estimetetd using water temperature, rain and days since last rain 
      flow[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl*cv_fl+1), 1/log(cv_fl*cv_fl+1))
      mu_fl_r[i,y] =  a_fl + b_fl[1]*Temp[i,y] + b_fl[2]*Rain_bf[i,y] + b_fl[3]*Rain[i,y]
      mu_fl[i,y] =  ifelse(mu_fl_r[i,y]>=1, mu_fl_r[i,y], 1)
      
    }
  }
  #   uninformative priors
  a_temp ~ dnorm(0, 100^-2)
  a_fl ~ dnorm(0, 100^-2)
  
  b_temp[1] ~ dnorm(0, 100^-2)
  
  for(i in 1:3){
    b_fl[i] ~ dnorm(0, 100^-2)
  }
  
  #sd_temp ~ dnorm(0, 100^-2)T(0,)
  # mu = 1, sd  = 100
  sd_temp ~ dlnorm(-4.61, 0.11)
  cv_fl ~ dunif(0.01, 10)

  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){ # 61 days in June-July
      
      N[i,y] = N_mid[i,y]+N_side[i,y]

      # Observed number of fish in the middle
      Nobs_mid[i,y]~dpois(Nobsp_mid[i,y]*N_[i,y]*pmid)
      Nobsp_mid[i,y]~dbeta(muB_mid[i,y]*etaB_mid,(1-muB_mid[i,y])*etaB_mid)
       
      muB_mid[i,y]<-0.6*(exp(BB_mid[i,y])/(1+exp(BB_mid[i,y])))+0.3
      BB_mid[i,y]~dnorm(aB_mid-bB_mid*flow[i,y],1/pow(sdBB_mid,2))
      
      # Observed number of fish in the finnish side
      Nobs_F[i,y]~dpois(Nobsp_F[i,y]*N_F[i,y])
      Nobsp_F[i,y]~dbeta(muB_F[i,y]*etaB_F,(1-muB_F[i,y])*etaB_F)

      muB_F[i,y]<-0.6*(exp(BB_F[i,y])/(1+exp(BB_F[i,y])))+0.3
      BB_F[i,y]~dnorm(aB_F-bB_F*flow[i,y],1/pow(sdBB_F,2))
      
      # Smolt passing through both sides
      N_side[i,y] = round(N_F[i,y] + N_N[i,y])      
      
      # Fin-side
      #N_F[i,y] ~ dlnorm(log(mu_F[i,y])-0.5*log(cv_F*cv_F+1), 1/log(cv_F*cv_F+1))
      #mu_F[i,y] = ifelse(mu_F_r[i,y]>=1, mu_F_r[i,y], 1)
      #mu_F_r[i,y] = a_F + b_F*flow[i,y]
      
      # Nor_side (same or smaller)
      N_N[i,y] = propF*N_F[i,y]
      

    }
  }
  # priors for observation process in the middle
  aB_mid~dnorm(2.9,60)
  bB_mid~dlnorm(-2.6,984)
  sdBB_mid~dlnorm(-0.23,210)
  etaB_mid~dunif(5,1000)

  # priors for observation process in the Fin-side
  aB_F~dnorm(2.9,60)
  bB_F~dlnorm(-2.6,984)
  sdBB_F~dlnorm(-0.23,210)
  etaB_F~dunif(5,1000)
  
  # Smolt on Fin-side
  a_F ~ dnorm(0, 100^-2)
  b_F ~ dnorm(0, 100^-2)
  cv_F  ~ dunif(0.001, 10)

  # Smolt on Nor-side
  propF ~ dbeta(1,10)
  
  # priors for schooling
#  K~dlnorm(6.07,0.7)
#  slope~dlnorm(-1.94,66)
#  cvS~dunif(0.001,2)
#  cvmuS~dunif(0.001,2)
  
#  TmuS<-1/log(cvmuS*cvmuS+1)
#  TS<-1/log(cvS*cvS+1)


  # Abundance
  # ==============
  for(y in 1:nYears){
    Ntot[y]<-exp(LNtot[y])
    LNtot[y]~dunif(7,15) # total run size in year y
    
    #N[1:nDays,y]~dmulti(qN[1:nDays,y],Ntot[y]) # daily true number of fish
    for(i in 1:(nDays-1)){
      N[i,y]<-round(qN[i,y]*Ntot[y])
    }
    N[nDays,y]<-round(Ntot[y]*(1-sum(qN[1:(nDays-1),y])))    
  }
  
  # Timig of the smolt run
  # i.e. how total number of smolts passing the video site
  # is distributed between 61 days
  # =============================================
  for(y in 1:nYears){
    # qN: daily proportion of smolts
    # dirichlet-distribution approximated with lognormal
    qN[1:nDays,y]<-zN[1:nDays,y]/sum(zN[1:nDays,y])
    
    for(i in 1:nDays){
      zN[i,y]~dlnorm(MN[i,y], TauN[i,y])
    }
    
    alphaN[1:nDays,y]<-muqN[1:nDays,y]*eta_alphaN#+0.001
    MN[1:nDays,y]<-log(muqN[1:nDays,y])-0.5/TauN[1:nDays,y]
    TauN[1:nDays,y]<-1/log((1/alphaN[1:nDays,y])+1)  
  }
  
  # Process for departure
  # ===========================
  for(y in 1:nYears){
    for(i in 1:nDays){
      # p: probability to start migration at day t, if haven't done so earlier
      # departure probability depends on temperature
      logit(p[i,y])<-P[i,y]
      #P[i,y]~dnorm(aP+bP*Temp[i,y],1/pow(sdP,2))
      P[i,y]~dnorm(aP+bP*Temp[i,y],1000)
    }
  }
  
  # Migration speed (in days to video site)
  # ==============
  # probability to be at video site in day j, if departing at day i
  for(y in 1:nYears){
    for(i in 1:nDays){ 
      # i: day of departure
      # j: day of passing the video site
      #j==i
      qDx[i,i,y]<-phi((log(0.5)-MD[i,y])/SD)
      
      # j>i
      #   aikas kätsy indeksointi
      for(j in (i+1):(i+13)){ #13 
        qDx[i,j,y]<-phi((log(j-i+0.5)-MD[i,y])/SD)-phi((log(j-i-0.5)-MD[i,y])/SD)
      }

      for(j in i:(i+13)){
        qD[i,j,y]<-qDx[i,j,y]/(sum(qDx[i,i:(i+13),y])+0.0001)
      }
      
      MD[i,y]<-log(muD[i,y])-0.5/TD
      muD[i,y]~dlnorm(log(exp(aD-bD*flow[i,y]))-0.5/TmuD, TmuD)
    }
  }
  SD<-1/sqrt(TD)
  TmuD<-1/log(cvmuD*cvmuD+1)
  TD<-1/log(cvD*cvD+1)
  
  aD~dlnorm(0.52,14) # mu=1.75,cv=0.27
  bD~dlnorm(-4.6,25) # mu=0.01,cv=0.2
  #cvmuD~dunif(0.001,1)
  #cvD~dunif(0.001,2)
  cvmuD~dunif(0.001,100)
  cvD~dunif(0.001,100)
  
  
  # Proportion departing in each day  
  # ========================================
  for(y in 1:nYears){
    p2[1,y]<-p[1,y]
    for(i in 2:(nDays-1)){
      p2[i,y]<-(1-sum(p2[1:(i-1),y]))*p[i,y]
    }
    p2[nDays,y]<-1-sum(p2[1:(nDays-1),y])
  
    # Joint distribution of qD and p2
    for(i in 1:nDays){ # day of departure
      for(j in i:(i+13)){ # day of passing the video site
        qtmp[j,i,y]<-qD[i,j,y]*p2[i,y]
      }
    }
  
    # Expected proportion of smolts passing the video site each day
    for(j in 1:14){
      muqN[j,y]<-sum(qtmp[j,1:j,y])+0.0001
    }
    for(j in 15:nDays){
      muqN[j,y]<-sum(qtmp[j,(j-13):j,y])+0.0001
    }
  }
  eta_alphaN~dunif(0.001,100000)
  
  aP~dnorm(-20,1) #mu=-20
  bP~dlnorm(0.6,10) #mu=1.91
  sdP~dlnorm(0,1) #mu=1.6
  #sdPx~dbeta(3,7)
  #sdP<-sdPx*3

}"

res <- run.jags(M1, data = data, monitor = var_names, sample = 30000,
                method = "parallel", n.chains = 2)


years<-c(2020) # 4 years of data for testing  
n_days<-61
dat<-data0221 # all real data
df<-s_dat_jags(dat,years, n_days) # 61: only june & july

data<-list(
  s=df$Schools,
  flow=df$Flow,
  Nobs_mid=df$Smolts,
  Nobs_F = as.matrix(d20xtra$side[1:61]),
  Temp=df$Temp,
  nDays=n_days,
  nYears=length(years),
  Temp_air = df$Temp_air,
  Rain = df$Rain,
  Rain_bf = df$Rain_bf
)

initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))

var_names<-c(
  "a_temp", "b_temp", "sd_temp",
  
  "a_fl", "b_fl", "cv_fl",
  
  "aD","bD","cvD","cvmuD",
  "K","slope","cvS", "cvmuS",
  
  "aP","bP","sdP",
  
  "aB","bB","sdBB"
  
)

res <- run.jags(M1, data = data, monitor = var_names, sample = 30000,
                method = "parallel", n.chains = 2)

failed.jags()
summary(res)
plot(res)

mu = 1;sd = 100;cv = sd/mu
log(mu)-0.5*log(cv*cv+1)
1/log(cv*cv+1)
sqrt(log(cv*cv+1))
