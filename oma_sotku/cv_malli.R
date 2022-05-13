source("oma_sotku/wrng-functions.R")
library(runjags);library(rjags)

#   villi kokeilu cv suhteen



load("01-Data/dat0221.RData")

years<-c(2020)
n_days<-61

df<-s_dat_jags(dat,years, n_days) # 61: only june & july

data<-list(
  nYears=length(years),
  nDays = n_days,
  s=df$Schools,
  flow=df$Flow,
  Nobs=df$Smolts,
  Nobs_side=df$side,
  Temp = df$Temp,
  Temp_air = df$Temp_air,
  Rain = df$Rain,
  Rain_bf = df$Rain_bf
)


initials<-list(list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))),
               list(LNtot=rep(14,data$nYears),zN=array(1, dim=c(61,data$nYears))))

M1<-"
model{
  
  # Predicting missing values of flow and water temperature using weatherdata
  # ==========================================================================
  for(y in 1:nYears){
    for(i in 1:nDays ){
      #   Water temperature is estimated from air temperature
      #Temp[i,y] ~ dnorm(mu_temp[i,y], sd_temp^-2)
      Temp[i,y] ~ dlnorm(log(mu_temp[i,y])-0.5*log(cv_temp[i,y]*cv_temp[i,y]+1), 1/log(cv_temp[i,y]*cv_temp[i,y]+1))
      #   cv[i,y]!!
      mu_temp_r[i,y] = a_temp + b_temp[1]*Temp_air[i,y]
      mu_temp[i,y] =  ifelse(mu_temp_r[i,y]>=0.0001, mu_temp_r[i,y], 0.0001)
      #   Flow is estimetetd using water temperature, rain and days since last rain 
      flow[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl[i,y]*cv_fl[i,y]+1), 1/log(cv_fl[i,y]*cv_fl[i,y]+1))
      #   cv[i,y]!!
      mu_fl_r[i,y] =  a_fl + b_fl[1]*Temp[i,y] + b_fl[2]*Rain_bf[i,y] + b_fl[3]*Rain[i,y]
      mu_fl[i,y] =  ifelse(mu_fl_r[i,y]>=0.0001, mu_fl_r[i,y], 0.0001)
      
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
  #cv_fl ~ dunif(0.01, 10)
  
  upr_temp ~ dunif(0.2, 2)
  lwr_temp ~ dunif(0.0001, 0.2)
  #lwr_temp ~ dbeta(1.1, 10)

  upr_fl ~ dunif(0.2, 2)
  lwr_fl ~ dunif(0.0001, 0.2)
  #lwr_fl ~ dbeta(1.1, 10)

  for(y in 1:nYears){
    for(i in 1:nDays){
      cv_temp[i,y] ~ dunif(lwr_temp, upr_temp)
      cv_fl[i,y] ~ dunif(lwr_fl, upr_fl)
    }
  }


  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){ # 61 days in June-July
      
      # Observed number of fish
      
      # Observed on sides
      Nobs_side[i,y] ~ dbin(Nobsp_side[i,y]*rho[i,y]*pref, N[i,y])
      
      Nobsp_side[i,y]~dbeta(muB_side[i,y]*etaB_side,(1-muB_side[i,y])*etaB_side)
       
      muB_side[i,y]<-0.35*(exp(BB_side[i,y])/(1+exp(BB_side[i,y])))+0.6
      BB_side[i,y]~dnorm(aB_side-bB_side*flow[i,y],1/pow(sdBB_side,2))

      # Observed in the middle
      Nobs[i,y]~dbin(Nobsp[i,y]*(1-rho[i,y]), N[i,y])
      
      Nobsp[i,y]~dbeta(muB[i,y]*etaB,(1-muB[i,y])*etaB)
       
      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))

      rho[i,y] = 1*(1/(1+exp(-rhoe[i,y])))
      #rhoe[i,y] = a_rho + b_rho*flow[i,y]
      rhoe[i,y] ~ dnorm(a_rho + b_rho*flow[i,y], sd_rho^-2)
      #   cv
      #rhoe[i,y] ~ dnorm(a_rho + b_rho*flow[i,y], sd_rho^-2)

    }
  }
  
  # priors for smolt passing through extra 8 cams
  a_rho_cv = 1
  b_rho_cv = 1

  a_rho ~ dnorm(-3, 1/abs(-6*a_rho_cv))
  b_rho ~ dnorm(0.01, 1/abs(-0.05*b_rho_cv))
  #sd_rho ~ dlnorm(0, 100^-2)
  sd_rho ~ dnorm(0, 100^-2)T(0,)

  # priors for observation process
  # middle
  aB~dnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(5,1000)
  
  # side
  aB_side~dnorm(2.9,60)
  bB_side~dlnorm(-2.6,984)
  sdBB_side~dlnorm(-0.23,210)
  etaB_side~dunif(5,1000)

  pref ~ dbeta(50,40)
  

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

par <- c("a_rho", "b_rho", "sd_rho")
par <- c("upr_temp", "lwr_temp", "upr_fl", "lwr_fl")
par <- c("rho")
res <- run.jags(M1, data = data, monitor = par, sample = 20000,
                method = "parallel", n.chains = 2, thin = 2, inits = initials)

summary(res)
plot(res)


failed.jags('data')

sr <- as.data.frame(summary(res))

sr %>% filter(str_detect(rownames(.), "flow"))


srdf$

dat %>% filter(Year==2020) %>% select(smolts, side)

