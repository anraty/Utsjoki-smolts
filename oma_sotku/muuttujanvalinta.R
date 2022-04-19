#   variable selection

mod <- "model{
    l1 ~ ddexp(0,1)
    l2 ~ ddexp(0, 10^-2)
    l3 ~ ddexp(0, 100^-2)
}"

llace <- run.jags(mod, monitor = c("l1", "l2", "l3"))
plot(llace)




vsel <- "model{

  for(i in 1:n){
    
    disc[i]~dnorm(mu[i], sd^-2)
    
    mu[i] <- b[1]+b[2]*temp[i]+b[3]*press[i]+b[4]*rain[i]+b[5]*humi[i]+
              b[6]*temp_air[i]+b[7]*wind[i]+b[8]*rainbf[i]
  }
  
  for(i in 1:8){
    b[i] ~ ddexp(0, 1^-2)
  }
  sd ~ dnorm(0, 100^-2)T(0,)

}"


vselres = run.jags(vsel, data = vseld, monitor = c("b", "sd"), sample = 30000, thin = 5)
summary(vselres)

# temp, temp_air, wind, rainbf
