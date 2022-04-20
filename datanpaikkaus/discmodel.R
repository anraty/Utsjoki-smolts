library(nimble);library(runjags);library(rjags)
#   model for predictiong discharge

#   virtaamaan vaikuttaa
#   -sade
#   -kosteus
#   -aika edellisestä sateeesta
#   -ilmanpaine
#   -tuuli
#   -edellisen päivän virtaama

c <- list(n = length(data17$date))
d <- list(
  disc = data17$disc,
  discbf = c(data17$disc[1], data17$disc[-1]),
  rain = data17$rain,
  humi = data17$humi,
  wind = data17$wind
)

data <- list(
  
  n = length(data17_all$date),
  disc = data17_all$disc,
  rainbf = data17_all$rainbf,
  rain = data17_all$rain,
  humi = data17_all$humi,
  wind = data17_all$wind,
  temp = data17_all$temp,
  temp_air = data17_all$temp_air
  
)

disCode <- nimbleCode({
  
  for( i in 1:n){
    
    #   likelihood   
    
    disc_r[i] ~ dnorm(disc[i], sd = sd_disc)
    #   using exp to ge zerocentric like it shold be in random eff model
    disc[i] <- mu_lin[i] + rand[i]
    rand[i] ~ dexp(rate = 1/lam_rand[i])
     #    ~ randomeffectmodelish
    mu_lin[i] <- b[1] + b[2]*rain[i] + b[3]*humi[i] + b[4]*wind[i] 
    lam_rand[i] = a[1]*disc[i-1] + a[2]*rainbf[i] 
    lam_rand[i] <- a*discbf[i] 
  }
  
  #   priors
  for(i in 1:4){
    b[i] ~ dnorm(0, sd = 100) 
  }
  a ~ dnorm(0, sd = 100)
  #for(i in 1:2){
  #  a[i] ~ dnorm(0, sd = 100)
  #}
  sd_disc ~ dnorm(0, sd = 100) 
  
})




samp <- nimbleMCMC(code = disCode, constants = c, data = d, monitors = "disc_r")

samplesSummary(samp)



dmod <- "model{
    
    for( i in 1:n){
    
    #   likelihood   
    
    #    ~ randomeffectmodelish
    disc[i] ~ dnorm(disc_mu[i], sd_disc^-2)T(0,)
    
    disc_mu[i] <- mu_lin[i] + rand[i]
    #disc_mu[i] <- mu_lin[i]
    rand[i] ~ ddexp(lam_rand[i], sd_rand^-2)
    #rand[i] ~ dnorm(lam_rand[i], sd_rand^-2)
    #   using exp to ge zerocentric like it shold be in random eff model
    
    #linear part
    #mu_lin[i] <- b[1] + b[2]*rain[i] + b[3]*humi[i] + b[4]*wind[i] 
    #mu_lin[i] <- b[1] + b[2]*rain[i] + b[3]*rainbf[i] + b[4]*press[i]
    mu_lin[i] <- b[1]*temp[i]+b[2]*temp_air[i]+b[3]*wind[i]+b[4]*rainbf[i]
    #randomeff part
    #lam_rand[i] <- a*rainbf[i]

    }
    
    for(i in 2:n){
      #lam_rand[i] = a[1]*disc[i-1] + a[2]*rainbf[i] 
      lam_rand[i] <- a*disc[i-1]
    
    }
  
  #   priors
  
  a ~ dnorm(0, 100^-2)
  for(i in 1:4){
  #for(i in 1:5){
    b[i] ~ dnorm(0, 100^-2) 
  }
  
  #for(i in 1:2){
  #  a[i] ~ dnorm(0, 100^-2)
  #}
  cv = 1
  #cv ~ dunif(0.01, 1)
  sd_disc ~ dnorm(0, 100^-2)T(0,)
  sd_rand ~ dnorm(0, 100^-2)T(0,)
  lam_rand[1] = 1

}"



results <- run.jags(model = dmod, data = data, burnin = 5000, sample = 30000, 
                    monitor = c("disc"), n.chains = 2, thin = 10,
                    method = "parallel")

sumres <- summary(results)
cbind(testdat[sort(ids), "disc"], sumres[sort(ids), 2], sumres[sort(ids), 4])


sr3 <- cbind(testdat[sort(ids), "disc"], sumres[sort(ids), 2], sumres[sort(ids), 4])
sr2


