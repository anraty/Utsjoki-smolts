sqp <- c(680/53.5, 920/78.5, 970/64, 470/27.5, 560/32, 540/30 )
boxplot(sqp)

850/67



expit <- function(r, max = 1, min = 0){
  
  max_m <- max-min
  
  return( 1/(1+exp(-r))*max_m + min)
}



pp_vis <- function(a, asd, b, bsd, sd, sdsd, max = 1, min = 0, tau =F){
  fl <- seq(0, 350, by = 10)
  n = 1000
  
  if(tau){
    asd <- asd^(-1/2)
    bsd <- bsd^(-1/2)
    sdsd <- sdsd^(-1/2)
  }
  
  
  sd <- rlnorm(n, mean = sd, sd = sdsd)
  lenfl <- length(fl)
  a_val <- rnorm(n, a, sd = asd)
  b_val <- rlnorm(n, b, sd = bsd)
  mu_eff <- matrix(nrow = n, ncol = lenfl)
  sm <- n*lenfl
  for(i in 1:lenfl){
    mu_eff[,i] <- a_val + b_val*fl[i]
  }
  
  for(i in 1:length(sd)){
    if(i == 1){
      eff <- matrix(rnorm(sm, mu_eff, sd = sd[i]), ncol = ncol(mu_eff))
      colnames(eff) <- paste(fl)
    }
    else{
      efft <- matrix(rnorm(sm, mu_eff, sd = sd[i]), ncol = ncol(mu_eff))
      colnames(efft) <- paste(fl)
      eff <- rbind(eff, efft)
    }
  }
  
  
  
  
  
  #p <- expit(eff, max = max)
  p <- 0.6*( exp(eff)/( 1+exp(eff) ) ) + 0.3
  return(p)
}

test <- pp_vis(a = 2.9, asd = 60, b = -2.6, bsd = 984, sd = -0.23, sdsd = 210, max = 0.9, min = 0.3, tau = T)
boxplot(test)

