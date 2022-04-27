# Modify dat into input form for JAGS

#dat_all%>%group_by(Year)%>%
#  summarize(N_smolts=sum(smolts, na.rm=T),
#            ave_sch=mean(schools, na.rm=T),
#            ave_flow=mean(flow, na.rm=T),
#            ave_temp=mean(meanTemp, na.rm=T)
#  )

#View(filter(dat_all, Year==2006))
# These do the same
#df_smolts[,1]<-filter(dat_all, Year==y)$smolts
#df_smolts[,1]<-filter(dat_all, Year==y)[["smolts"]]

# This is a function to choose desired years and maximum number of days
# of Utsjoki smolts data and to provide that data in JAGS input format

# x: vector of years of data to include
# n: maximum number of days to include 
smolts_data_to_jags<-function(dat,x,n){ 
  #dat=dat
  #x=years
  #n=n_days
  
  nyears<-length(x)
  
  df_tmp<-array(NA, dim=c(n,nyears))
  
  df_smolts<-df_tmp
  df_schools<-df_tmp
  df_flow<-df_tmp
  df_temp<-df_tmp
  for(i in 1:nyears){
    df_smolts[,i]<-filter(dat, Year==x[i], day<=n)$smolts
    df_schools[,i]<-filter(dat, Year==x[i], day<=n)$schools
    df_flow[,i]<-filter(dat, Year==x[i], day<=n)$flow
    df_temp[,i]<-filter(dat, Year==x[i], day<=n)$meanTemp
  }
  colnames(df_smolts)<-x
  colnames(df_schools)<-x
  colnames(df_flow)<-x
  colnames(df_temp)<-x

  df<-list(df_smolts,
           df_schools,
           df_flow,
           df_temp)
  names(df)<-c("Smolts", "Schools", "Flow", "Temp")
  return(df)
}





