#   testing model for extracameras


priormod <- "model{
for(i in 1:nFlow){
  p[i] = logit(alpha[i])
  alpha[i] ~ dnorm(a + b*flow[i], sd^-2)
}

a ~ dnorm(0,1)
b ~ dnorm(0,1)
sd ~ dnorm(0, 100^-2)T(0,)

}"

data <- list(
  nFlow = length(seq(min(na.omit(round(data0221$flow))), max(na.omit(round(data0221$flow))))),
  flow = as.matrix(seq(min(na.omit(round(data0221$flow))), max(na.omit(round(data0221$flow)))))

)

par <- c("p")
par2 <- c("N_f")
res <- run.jags(priormod, data = data, monitor = par, sample = 100000,
                method = "parallel", n.chains = 2, thin = 30)

summary(res)


#   function for plotting prior effect on p

flow <- data$flow
flow <- (flow-mean(flow))/sd(flow)

expit <- function(r){
  return( 1/(1+exp(-r))*0.3 )
}

tpar <- function(a, b){
  eff <- a + b*flow
  p <- expit(eff)
  plot(p, type = 'l')
}


tpar(1,2)


