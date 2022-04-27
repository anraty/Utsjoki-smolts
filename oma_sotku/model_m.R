#   mun oma hämmennys ja lisääykset malliin 



model{
  
  # Imputing missing values from flow and eater temperature using weatherdata
  # ==========================================================================
  for(y in 1:nYears){
    for(i in 1:nDays ){
      #   Water temperature is estimated from air temperature
      Temp[y,i] ~ dnorm(mu_temp[i], sd_temp^-2)
      mu_wt[i] = a_temp + b_temp[1]*Temp_air[i]
      
      #   Flow is estimetetd using water temperature, rain and days since last rain 
      Flow[i] ~ dlnorm(log(mu_fl[i])-0.5*log(cv_fl*cv_fl+1), 1/log(cv_fl*cv_fl+1))
      mu_fl_r[i] =  a_fl + b_fl[1]*Temp[i] + b_fl[2]*Rain_bf[i] + b_fl[3]*Rain[i]
      mu_fl[i] =  ifelse(mu_fl_r[i]>=1, mu_fl_r[i], 1)
      
    }
  }
  #   uninformative priors
  a_temp ~ dnorm(0, 100^-2)
  a_fl ~ dnorm(0, 100^-2)
  
  b_temp[1] ~ dnorm(0, 100^-2)
  
  for(i in 1:2){
    b_fl[i] ~ dnorm(0, 100^-2)
  }
  
  sd_temp ~ dnorm(0, 100^-2)T(0,)
  cv_fl ~ dunif(0.01, 10)
  
  # Process of departing
  # ===========================
  for(y in 1:nYears){
    for(i in 1:nDays){
      # p: probability to start migration at day t, if haven't done so earlier
      # t: daily mean temperature
      logit(p[i,y])<-p2[i,y]
      p2[i,y]~dnorm(aP+bP*t[i,y],1/pow(sdP,2))
    }
  }
  # P: expected proportion of smolts departing at day i 
  for(y in 1:nYears){
    P[1,y]<-p[1,y]
    for(i in 2:(nDays-1)){
      P[i,y]<-(1-sum(P[1:(i-1),y]))*p[i,y]
    }
    P[nDays,y]<-1-sum(P[1:60,y])
  }
  
  aP~dnorm(-20,1)
  bP~dlnorm(0.6,10)
  sdP~dlnorm(0,1) 
  
  # Process of travelling
  # ==============
  # qD: probability that a smolt that departs at day i passes the video site at day j
  for(y in 1:nYears){
    for(i in 1:nDays){ 
      # i: day of departure
      # j: day of passing the video site
      # f: daily mean flow velocity
      # j==i
      qDx[i,i,y]<-phi((log(0.5)-MD[i,y])/SD)
      # j>i
      for(j in (i+1):(i+13)){ 
        qDx[i,j,y]<-phi((log(j-i+0.5)-MD[i,y])/SD)-phi((log(j-i-0.5)-MD[i,y])/SD)
      }
      for(j in i:(i+13)){
        qD[i,j,y]<-qDx[i,j,y]/(sum(qDx[i,i:(i+13),y])+0.0001)
      }
      MD[i,y]<-log(psi[i,y])-0.5/TD
      psi[i,y]~dlnorm(log(exp(aD-bD*f[i,y]))-0.5/TmuD, TmuD)
    }
  }
  SD<-1/sqrt(TD)
  TmuD<-1/log(cvmuD*cvmuD+1)
  TD<-1/log(cvD*cvD+1)
  
  aD~dlnorm(0.52,14)
  bD~dlnorm(-4.6,25) 
  cvmuD~dunif(0.001,1)
  cvD~dunif(0.001,2)
  
  # Arrival distribution
  # =============================================
  # Joint distribution of qD and P: prob that a smolt departing day i arrives day j
  for(y in 1:nYears){
    for(i in 1:nDays){ # day of departure
      for(j in i:(i+13)){ # day of passing the video site
        qJ[j,i,y]<-qD[i,j,y]*P[i,y]
      }
    }
    
    # Expected proportion of smolts passing the video site each day
    for(j in 1:14){
      muqN[j,y]<-sum(qJ[j,1:j,y])+0.0001 # small constant added for numerical stability
    }
    for(j in 15:nDays){
      muqN[j,y]<-sum(qJ[j,(j-13):j,y])+0.0001
    }
  }
  
  for(y in 1:nYears){
    Ntot[y]<-exp(LNtot[y])
    LNtot[y]~dunif(7,15) # total run size in year y
    
    for(j in 1:(nDays-1)){
      N[j,y]<-round(qN[j,y]*Ntot[y]) # daily number of fish passing
    }
    N[nDays,y]<-round(Ntot[y]*(1-sum(qN[1:(nDays-1),y])))    
    
    # qN: daily proportion of smolts
    # dirichlet-distribution approximated with lognormal
    qN[1:nDays,y]<-zN[1:nDays,y]/sum(zN[1:nDays,y])
    
    for(i in 1:nDays){
      zN[j,y]~dlnorm(MN[j,y], TauN[j,y])
    }
    
    alphaN[1:nDays,y]<-muqN[1:nDays,y]*eta_alphaN
    MN[1:nDays,y]<-log(muqN[1:nDays,y])-0.5/TauN[1:nDays,y]
    TauN[1:nDays,y]<-1/log((1/alphaN[1:nDays,y])+1)  
  }
  
  eta_alphaN~dunif(0.001,100000)
  
  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){
      # Nobs: Observed number of smolts
      Nobs[i,y]~dbetabin(100,10,N[i,y])  # M1
      Nobs[i,y]~dbetabin(muB[i,y]*etaB,(1-muB[i,y])*etaB,N[i,y])  #M2
      
      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*f[i,y],1/pow(sdBB,2))
      
    }
  }
  # priors for observation process M2
  aB~dnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(5,1000)    
}
