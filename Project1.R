#########################################
# Project 1 - Microbial Risk Assessment #
# Jonas Meijerink ~ 1/10/2024           #
#########################################


### Exposure assessment --------------------------------------------------------

## Prevalence of Campylobacter spp. in poultry carcasses at slaughterhouses
Ps <- rbeta(1 , 10 + 1, 30 - 10 + 1)


## Prevalence of Campylobacter spp. in poultry carcasses at retail
Pr <- rbeta(1 , 25 + 1, 30 - 25 + 1)


## Number of Campylobacter spp. in poultry meat at retail (log scale)

# Identify each interval of the cumulative distribution
x <- c(1,2,3,4,5,6,7)

# Sample of the intervals according to the cumulative distribution
index <- sample(x , size = 1 , 
             prob = c(0.325 , 0.15 , 0.175 , 0.05 , 0.15 , 0.125 , 0.025))

# Define the piecewise boundaries
Cmin <- c(-2.079 , 1.556 , 2.301 , 2.699 , 3    , 3.699, 4.255)
Cmax <- c( 1.556 , 2.301 , 2.699 , 3     , 3.699, 4    , 4.255)

# Generate a random value from this interval
Cr <- runif(n=1 , min = Cmin[index], max = Cmax[index])


## Cross-contamination factor
Fcc <- Pr/Ps


## Probability of infected carcass 
Pc <- Fcc * Ps / (1 - Ps + Fcc * Ps)


## Uncertainty about the storage condition
Sc <- rbinom(n = 1 , size = 1, prob=0.5)
# Refrigeration = 1
# Freeze = 0


## Refrigeration reduction
Rrmin <- rnorm(1 , 0.31 , 0.13)
Rrmax <- rnorm(1 , 0.81 , 0.24)
Rr <- runif(1 , min = Rrmin , max = Rrmax)


## Freeze reduction
Rf <- runif(1 , 0.6 , 2.87)


## Number of Campylobacter spp. in poultry carcasses stored under refrigeration
Cref <- 10^(Cr - Rr)


## Number of Campylobacter spp. in poultry carcasses stored under freeze
Cfre <- 10^(Cr - Rf)


## Four people
Ns <- 4


## Number of Campylobacter spp. per serving of salad (C_salad)
Ccs <- Sc * rpois(1 , Cref / Ns) + (1 - Sc) * rpois(1 , Cfre / Ns) 


## (Cross-contamination during food preparation based)
# not necessary for the assignment


### Dose response --------------------------------------------------------------

## Probability of infection for an individual consuming a meal given the dose
Pinf1 <- rbeta(1 , 0.21 , 59.95)
Pinf <- 1 - (1 - Pinf1)^Ccs


## Probability of illness
Pillinf <- rbeta(1 , 29 + 1, 89 + 1 -29)
Pill <- Pinf * Pillinf


### Risk characterization ------------------------------------------------------

## Number of poultry carcasses produced in Argentina 
Npc <- 495143444

if(Pill!=0){
## Number of human cases/year (Ns-p?)
Nill <- rbinom(Npc * Ns * 1, prob = Pill)
}else{
Nill <- 0  
}

### Visualisation of the results -----------------------------------------------



