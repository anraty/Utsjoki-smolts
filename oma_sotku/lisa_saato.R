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
load("01-Data/dat0221.RData")

data <- list(
  nFlow = length(seq(min(na.omit(round(data$flow))), max(na.omit(round(data$flow))))),
  flow = as.matrix(seq(min(na.omit(round(data$flow))), max(na.omit(round(data$flow)))))

)

par <- c("p")
par2 <- c("N_f")
#res <- run.jags(priormod, data = data, monitor = par, sample = 100000,
#                method = "parallel", n.chains = 2, thin = 30)

#summary(res)


#   function for plotting prior effect on p

flow <- data$flow
#flow <- (flow-mean(flow))/sd(flow)

expit <- function(r, max = 1){
  return( 1/(1+exp(-r))*max)
}

tpar <- function(a, b){
  eff <- a + b*flow
  p <- expit(eff)
  plot(p, type = 'l')
}


tpar(-6,0.05)


pp <- function(a, acv, b, bcv, sd, sdcv, max = 1){
  fl <- seq(0, 350, by = 10)
  n = 10000
  lenfl <- length(fl)
  a_val <- rnorm(n, a, sd = abs(a*acv))
  b_val <- rnorm(n, b, sd = abs(bcv*b))
  eff <- matrix(nrow = n, ncol = lenfl)
  for(i in 1:lenfl){
    eff[,i] <- a_val + b_val*fl[i]
  }
  p <- expit(eff, max = max)
  return(p)
}
  
  
boxplot(pp(-3,1,0.01,1,1,1, max = 1), xlab = "Virtaama / 10", 
        ylab = "Sivulla kulkevien smolttien osuus", outline = F)



