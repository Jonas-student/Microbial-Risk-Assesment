#########################################
# Project 1 - Microbial Risk Assessment #
# Jonas Meijerink ~ 1/10/2024           #
#########################################


### Exposure assessment --------------------------------------------------------

# Prevalence of Campylobacter spp. in poultry carcasses at slaughterhouses
Ps <- rbeta(10 + 1, 30 - 10 + 1)

# Prevalence of Campylobacter spp. in poultry carcasses at retail
Pr <- rbeta(25 + 1, 30 - 25 + 1)

# Number of Campylobacter spp. in poultry meat at retail
Cr <- 

# Cross-contamination factor
Fcc <- Pr/Ps

# Probability of infected carcass 
Pc <- Fcc * Ps / (1 - Ps + Fcc * Ps)

# Uncertainty about the storage condition
Sc <- rbinom(n = 1, size = 1, prob=0.5)
# Refrigeration = 1
# Freeze = 0

# Refrigeration reduction
Rrmin <- rnorm(0.31 , 0.13)
Rrmax <- rnorm(0.81 , 0.24)
Rr <- runif(Rrmin , Rrmax)

# Freeze reduction
Rf <- runif(0.6 , 2.87)

# Number of Campylobacter spp. in poultry carcasses stored under refrigeration
Cref <- 10^(Cr - Rr)

# Number of Campylobacter spp. in poultry carcasses stored under freeze
Cfre <- 10^(Cr - Rf)

# Four people
Ns <- 4

# Number of Campylobacter spp. per serving of salad (C_salad)
Ccs <- Sc * rpois(1 , Cref / Ns) + (1 - Sc) * rpois(1 , Cfre / Ns) 

## (cross-contamination during food preparation based)
# not necessary

### Dose response --------------------------------------------------------------

# probability of infection for an individual consuming a meal given the dose
Pinf1 <- rbeta(0.21 , 59.95)
Pinf <- 1 - (1 - Pinf1)^Ccs

# probability of illness
Pillinf <- rbeta(29 + 1, 89 + 1 -29)
Pill <- Pinf * Pillinf

### Risk characterization ------------------------------------------------------

# Number of poultry carcasses produced in Argentina 
Npc <- 495143444

# Number of human cases/year 
Nill <- rbinom(Npc * Ns * Ns-p , Pill)
