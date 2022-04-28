
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
      Flow[i,y] ~ dlnorm(log(mu_fl[i,y])-0.5*log(cv_fl*cv_fl+1), 1/log(cv_fl*cv_fl+1))
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
  
  sd_temp ~ dnorm(0, 100^-2)T(0,)
  cv_fl ~ dunif(0.01, 10)

  # Observation process
  # ====================
  for(y in 1:nYears){
    for(i in 1:nDays){ # 61 days in June-July
      
      # Observed number of fish
       #Nobs[i,y]~dbetabin(100,10,N[i,y])  
       #Nobs[i,y]~dbetabin(muB[i,y]*etaB,(1-muB[i,y])*etaB,N[i,y])
       #Nobs[i,y]~dbinom(Nobsp[i,y], N[i,y])
       Nobs[i,y]~dpois(Nobsp[i,y]*N[i,y])
       #Nobsp[i,y] = ifelse(Nobsp_r[i,y]==0, 0.00001, Nobsp_r[i,y])
       #Nobsp_r[i,y]~dbeta(muB[i,y]*etaB,(1-muB[i,y])*etaB)
       Nobsp[i,y]~dbeta(muB[i,y]*etaB,(1-muB[i,y])*etaB)
       
      #Nobs[i,y]~dbetabin(muB[i,y]*etaStarB[i,y],(1-muB[i,y])*etaStarB[i,y],N[i,y])
      
      muB[i,y]<-0.6*(exp(BB[i,y])/(1+exp(BB[i,y])))+0.3
      BB[i,y]~dnorm(aB-bB*flow[i,y],1/pow(sdBB,2))
      
      #etaStarB[i,y]<-(N[i,y]-s[i,y])/(s[i,y]-1+0.01)+1

      #s[i,y]~dlnorm(log(muS[i,y])-0.5/TS,TS)
      #muS[i,y]~dlnorm(log((K*N[i,y])/((K/slope)+N[i,y])+0.0001)-0.5/TmuS,TmuS)
    }
  }
  # priors for observation process
  aB~dnorm(2.9,60)
  bB~dlnorm(-2.6,984)
  sdBB~dlnorm(-0.23,210)
  etaB~dunif(5,1000)
  
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
  cvmuD~dunif(0.001,1)
  cvD~dunif(0.001,2)
  
  # Proportion departing in each day  
  # ========================================
  for(y in 1:nYears){
    p2[1,y]<-p[1,y]
    for(i in 2:(nDays-1)){
      p2[i,y]<-(1-sum(p2[1:(i-1),y]))*p[i,y]
    }
    p2[nDays,y]<-1-sum(p2[1:60,y])
  
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

  # check sums (should be close to 1, otherwise fish is lost)
  for(i in 48:61){ # last 2 weeks of July 2006
    sums1[i]<-sum(qD[i,i:(i+13),1])
  }
  for(i in 48:61){ # last 2 weeks of July 2014
    sums2[i]<-sum(qD[i,i:(i+13),2])
  }
  
}"

years<-c(2005:2006,2008,2014) # 4 years of data for testing  
n_days<-61
dat<-data0221 # all real data
df<-s_dat_jags(dat,years, n_days) # 61: only june & july

data<-list(
  s=df$Schools,
  flow=df$Flow,
  Nobs=df$Smolts,                     
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
  "aD","bD","cvD","cvmuD",
  "K","slope","cvS", "cvmuS",
  
  "sums1","sums2",
  
  "aP","bP","sdP",
  #"etaB",
  "aB","bB","sdBB",
  "eta_alphaN",
  "Ntot","N"
)

res <- run.jags(M1, data = data, monitor = var_names, sample = 10000,
                method = "parallel", n.chains = 2)

summary(res)

Mname<-str_c("03-Model/","anthi", ".txt")
cat(M1,file=Mname)
jm<-jags.model(Mname,inits=initials, n.adapt=1000, data=data,n.chains=2)

data$Nobs[30,1]

