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
Sc <- rbinom(n, size, prob=)

#
Rr <- 


### Dose response --------------------------------------------------------------





### Risk characterization ------------------------------------------------------

# 
