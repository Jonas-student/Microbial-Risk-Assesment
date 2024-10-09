################################################################################
# Project 1 - Microbial Risk Assessment                                        #
# Deckers Lode, Meijerink Jonas and Seid Adem Aragaw                           #
# 8/10/2024                                                                    #
################################################################################

### Define a seed for reproducibility ------------------------------------------
set.seed(485721023)


simulation <- function(N){

  ### Define all variables -----------------------------------------------------
  Ps <- rep(NA,N)
  Pr <- rep(NA,N)
  Cr <- rep(NA,N)
  Fcc <- rep(NA,N)
  Pc <- rep(NA,N)
  Sc <- rep(NA,N)  
  Rr <- rep(NA,N)
  Rf <- rep(NA,N)
  Ch <- rep(NA,N)
  Ccs <- rep(NA,N)
  C <- rep(NA,N)
  Pinf1 <- rep(NA,N)
  Pinf <- rep(NA,N)
  Pillinf <- rep(NA,N)
  Pill <- rep(NA,N)
  Nill <- rep(NA,N)
    
  ## simulate N times
  for(i in 1:N){
    
    ### Exposure assessment ----------------------------------------------------
    
    ## Prevalence of Campylobacter spp. in poultry carcasses at slaughterhouses
    Ps[i] <- rbeta(1 , 10 + 1, 30 - 10 + 1)
    
    
    ## Prevalence of Campylobacter spp. in poultry carcasses at retail
    Pr[i] <- rbeta(1 , 25 + 1, 30 - 25 + 1)
    
    
    ## Number of Campylobacter spp. in poultry meat at retail (log scale)
    
    # Identify each interval of the cumulative distribution
    x <- c(1,2,3,4,5,6,7)
    
    # Sample the intervals according to the cumulative distribution
    index <- sample(x , size = 1 , 
                 prob = c(0.325 , 0.15 , 0.175 , 0.05 , 0.15 , 0.125 , 0.025))
    
    # Define the piecewise boundaries
    Cmin <- c(-2.079 , 1.556 , 2.301 , 2.699 , 3    , 3.699, 4.255)
    Cmax <- c( 1.556 , 2.301 , 2.699 , 3     , 3.699, 4    , 4.255)
    
    # Generate a random value from this interval
    Cr[i] <- runif(n=1 , min = Cmin[index], max = Cmax[index])
    
    
    ## Cross-contamination factor
    Fcc[i] <- Pr[i]/Ps[i]
    
    
    ## Probability of infected carcass 
    Pc[i] <- Fcc[i] * Ps[i] / (1 - Ps[i] + Fcc[i] * Ps[i])
    
    
    ## Uncertainty about the storage condition
    Sc[i] <- rbinom(n = 1 , size = 1, prob = runif(1 , min = 0 , max = 1))
    # Refrigeration = 1
    # Freeze = 0
    
    
    ## Refrigeration reduction
    Rrmin <- rnorm(1 , 0.31 , 0.13)
    Rrmax <- rnorm(1 , 0.81 , 0.24)
    if (Rrmin < Rrmax){
      Rr[i] <- runif(n = 1 , min = Rrmin , max = Rrmax)
    }
    if (Rrmin > Rrmax){
      Rr[i] <- runif(n = 1 , min = Rrmax , max = Rrmin)
    }
    
    
    ## Freeze reduction
    Rf[i] <- runif(n = 1 , min = 0.6 , max = 2.87)
    
    
    if (Sc[i]==1){
      ## Number of Campylobacter spp. in poultry carcasses stored under refrigeration
      Ch[i] <- 10^(Cr[i] - Rr[i])
    }else{
      ## Number of Campylobacter spp. in poultry carcasses stored under freeze
      Ch[i] <- 10^(Cr[i] - Rf[i])
    }
    
    ## Four people
    Ns <- 4
    
    ## Number of Campylobacter spp. per serving of salad (C_salad)
    Ccs[i] <- rpois(1 , Ch[i] / Ns)
    
    
    ## Number of Campylobacter spp. per serving after cooking 
    Rc <- rbeta(n = 1 , shape1 = 10 , shape2 = 0.05)
    C[i] <- Ccs[i] * 10^(-Rc)
    
    
    ## (Cross-contamination during food preparation based)
    # not necessary for the assignment
    
    
    ### Dose response ----------------------------------------------------------
    
    ## Probability of infection for an individual consuming a meal given the dose
    Pinf1[i] <- rbeta(1 , 0.21 , 59.95)
    Pinf[i] <- 1 - (1 - Pinf1[i])^C[i]# this should be the concentration coming from salad
    
    
    ## Probability of illness
    Pillinf[i] <- rbeta(1 , 29 + 1, 89 + 1 -29)
    Pill[i] <- Pinf[i] * Pillinf[i]
    
    
    ### Risk characterization --------------------------------------------------
    
    ## Number of poultry carcasses produced in Argentina 
    Npc <- 495143444
    
    if(Pill[i]!=0){
      ## Number of human cases/year
      Nill[i] <- rbinom(n = 1 , size = Npc * Ns * 1, prob = Pill[i])
    }else{
      Nill[i] <- 0  
    }
  
  }
  return(matrix( data = c(Cr , Ch , C , Pill , Nill) , nrow = N , ncol = 5))
}

### Visualisation of the results -----------------------------------------------
out <- simulation(1000)

## Plot simular to Fig 2.
plot(ecdf((out[,1])),xlim=c(-4,4))
lines(ecdf(log10(out[,2])),col="red")


