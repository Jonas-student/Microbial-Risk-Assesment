################################################################################
# Project 1 - Microbial Risk Assessment                                        #
# Deckers Lode, Meijerink Jonas and Seid Adem Aragaw                           #
# 23/10/2024                                                                   #
################################################################################

### Define a seed for reproducibility ------------------------------------------
set.seed(485721023)


simulation <- function(N , Storage){
  ## This function requires the following input parameters:
  # - N : the total number of simulations
  # - Storage : 2 storage condition always fresh
  #             1 storage condition always freezing 
  #             0 storage condition has two options freeze/fresh
  
  ### Define all variables -----------------------------------------------------
  Ps <- rep(NA,N)       # Prevalence in poultry carcasses at slaughterhouses
  Pr <- rep(NA,N)       # Prevalence in poultry carcasses at retail
  Cr <- rep(NA,N)       # Number in poultry meat at retail (log scale)
  Fcc <- rep(NA,N)      # Cross-contamination factor
  Pc <- rep(NA,N)       # Probability of infected carcass 
  Sc <- rep(NA,N)       # Uncertainty about the storage condition
  Rr <- rep(NA,N)       # Refrigeration reduction
  Rf <- rep(NA,N)       # Freeze reduction
  Ch <- rep(NA,N)       # Number in poultry carcasses under storage condition
  Cc <- rep(NA,N)       # Number per serving
  Rc <- rep(NA,N)       # Cooking reduction %log MPN 
  Cps <- rep(NA,N)      # Number per serving after cooking 
  Pinf1 <- rep(NA,N)    # Probability of infection for a single campylobacter
  Pinf <- rep(NA,N)     # Probability of infection for a meal
  Pillinf <- rep(NA,N)  # Probability of illness after infection
  Pill <- rep(NA,N)     # Probability of illness
  Nill <- rep(NA,N)     # Number of human cases/year
  
  
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
    Cmin <- c(-2.079 , 1.556 , 2.301 , 2.699 , 3    , 3.699, 4    )
    Cmax <- c( 1.556 , 2.301 , 2.699 , 3     , 3.699, 4    , 4.255)
    
    # Generate a random value from this interval
    Cr[i] <- runif(n=1 , min = Cmin[index], max = Cmax[index])
    
    
    ## Cross-contamination factor
    Fcc[i] <- Pr[i]/Ps[i]
    
    
    ## Probability of infected carcass 
    Pc[i] <- Fcc[i] * Ps[i] / (1 - Ps[i] + Fcc[i] * Ps[i])
    
    
    ## Uncertainty about the storage condition
    if (Storage==0){
      Sc[i] <- rbinom(n = 1 , size = 1, prob = runif(1 , min = 0 , max = 1))
    }else if(Storage==1){
      Sc[i] <- 0
    }else{
      Sc[i] <- 1 
    }
    # Refrigeration = 1
    # Freeze = 0
    
    
    ## Refrigeration reduction
    Rrmin <- rnorm(1 , 0.31 , 0.13)
    Rrmax <- rnorm(1 , 0.81 , 0.24)
    while (Rrmin > Rrmax){
      Rrmin <- rnorm(1 , 0.31 , 0.13)
      Rrmax <- rnorm(1 , 0.81 , 0.24)
    }
    Rr[i] <- runif(n = 1 , min = Rrmin , max = Rrmax)
    
    ## Freeze reduction
    Rf[i] <- runif(n = 1 , min = 0.6 , max = 2.87)
    
    
    if (Sc[i]==1){
      ## Number of Campylobacter spp. in poultry carcasses stored under refrigeration
      Ch[i] <- 10^(Cr[i] - Rr[i])
    }else{
      ## Number of Campylobacter spp. in poultry carcasses stored under freeze
      Ch[i] <- 10^(Cr[i] - Rf[i])
    }
    
    
    ## Number of Campylobacter spp. per serving after cooking 
    Rc[i] <- rbeta(n = 1 , shape1 = 10 , shape2 = 0.05)
    Cc[i] <- Ch[i] * 10^(-Rc[i])
    
    
    ## Four people
    Ns <- 4
    
    
    ## Number of Campylobacter spp. per serving
    Cps[i] <- rpois(1 , Cc[i] / Ns)
    
    
    ## (Cross-contamination during food preparation based)
    # not necessary for the assignment
    
    
    ### Dose response ----------------------------------------------------------
    
    ## Probability of infection for an individual consuming a meal given the dose
    Pinf1[i] <- rbeta(1 , 0.21 , 59.95)
    Pinf[i] <- 1 - (1 - Pinf1[i])^Cps[i] 
    
    ## Probability of illness
    Pillinf[i] <- rbeta(1 , 29 + 1, 89 + 1 -29)
    Pill[i] <- Pinf[i] * Pillinf[i]
    
    
    ### Risk characterization --------------------------------------------------
    
    ## Number of poultry carcasses produced in Argentina 
    Nps <- 495143444
    
    if(Pill[i]!=0){
      ## Number of human cases/year
      Nill[i] <- rbinom(n = 1 , size = Nps * Ns * 1, prob = Pill[i])
    }else{
      Nill[i] <- 0  
    }
    
  }
  return(matrix( data = c(Ps , Pr , Sc , Rc ,  Cps , Pill , Nill) , nrow = N , ncol = 7))
}

### Visualisation of the results -----------------------------------------------

## Load libraries
library(ggplot2)
library(gridExtra)
library(ltm)
library(grid)


## Output from the simulation model
out <- data.frame(simulation(N = 100000 , Storage = 0))


## 3.1 What is the amount of Campylobacter spp. to which a consumer is exposed
## in a single serving of poultry meat? !!--> change y-axis!!

# Visualisations
plot1 <- ggplot(aes(x=X5) , data=out) + geom_histogram(aes(y = ..density..),
                                                       fill="#69b3a2",binwidth = 1 , alpha=1 ) +
  labs(y = "Density", x = "MPN per serving") + theme_minimal() +
  ylim(0,0.6)

plot2 <- ggplot(aes(x=X5) , data=out) + geom_histogram(aes(y = ..density..),
                                                       fill="#69b3a2", color="#e9ecef", binwidth = 1.07) +
  labs(y = "Density", x = "MPN per serving") + theme_minimal() + xlim(-1,48)+
  geom_vline(xintercept=10, color="gray1", linetype="dashed", size=0.75) +
  ylim(0,0.6)

title <- textGrob("Number of Campylobacter spp. per serving")
grid.arrange(plot1, plot2, ncol=2, top=title)


# Summary statistics 
sum(out$X5<48)/100000


## 3.2 How common is an exposure to doses > 10 MPN/meat serving?
sum(out$X5>10)/100000


## 3.3 What is the prevalence of meat contamination?
sum(out$X5>0)/100000
sum(out$X5==0)/100000


## 3.4 What is the probability of illness for a single meal of chicken?
mean(out$X6)
sum(out$X6<0.0276)/100000

ggplot(aes(x=X6) , data=out) +  
  geom_histogram(aes(y = ..density../100),fill="#69b3a2", color="#e9ecef",
                 alpha=0.8 , binwidth = 0.01) +
  ggtitle("Probability of illness per serving") + 
  labs(y = "Density", x = "Probability of illnes")+
  theme_minimal() + xlim(-0.01,0.3) + 
  theme(plot.title = element_text( hjust = 0.5))


## 3.5 Estimate the number of people who could suffer from campylobacteriosis
## related to poultry meat consumption.
mean(out$X7)
model<-lm(out$X7~1)
confint(model)

## 3.6 Investigate using Spearman’s rank correlation coefficients which factors 
## in the chain have the largest impact on the risk of illness.

correlation <- data.frame(coef = cor(out , method = "pearson")[1:5,6])
correlation$factores <- c("Prevalence in poultry carcasses at slaughterhouses", 
                          "Prevalence in poultry carcasses at retail", "Storage condition" , 
                          "Cooking reduction  " , "Number (MPN) of Campylobacter per serving")

correlation[3,1] <- biserial.cor(x=out$X6, y=out$X3, use = c("all.obs"),level=2)

ggplot(data=correlation , aes(x=coef , y=factores , fill=c("1","1","1","0","1"))) + 
  geom_bar(stat = "identity") +xlim(-0.01,1) + geom_col() +
  geom_text(aes(label = round(coef,5)), hjust = -0.1, size = 3) + 
  ggtitle("Correlation with factors that impact the risk of illness")+  theme_minimal() + 
  theme(legend.position="none" , plot.title = element_text(hjust = -0.8)) + 
  labs(x="Correlation*" , y="")


## 3.7 Investigate the impact of storage: assume that 100% of chickens were stored frozen, what
## would be the impact on the human campoylobacteriosis risk as compared to if none of
## the chicken were stored frozen?

out_freeze_only <- data.frame(simulation(N = 100000 , Storage = 1))
out_fridg_only <- data.frame(simulation(N = 100000 , Storage = 2))

a<-hist(x=out_fridg_only $X6)
b<-hist(x=out_freeze_only$X6)
a<-data.frame("mids"=a$mids , "counts"=a$counts ,"Fmids"  = c(b$mids,0.39,0.41,0.43,0.45) , "Fcounts" = c(b$counts,1,1,1,1))
a_long <- pivot_longer(a, 
                       cols = everything(),
                       names_to = c("type", ".value"),
                       names_pattern = "(F?)(mids|counts)")

ggplot(a_long, aes(x = as.factor(mids), y = counts, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_log10(breaks = c(10^1, 10^3, 10^5), labels = c(1, 3, 5)) +  # Custom breaks and labels for y-axis
  labs(y = "Frequency on the log10 scale", 
       x = "Probability of illness",
       fill="Storage condition") +
  scale_fill_discrete(labels = c("Refrigerator", "Freezer"))+
  scale_x_discrete(breaks = c("0.05", "0.15", "0.25", "0.35", "0.45")) +  # Set specific x-axis labels
  theme_minimal()



#------------------------------------------------------------------------------#

# Additional graph: Pearson vs Spearman


data <- data.frame(
  Campylobacter_per_serving = out$X5,  # Number of Campylobacter spp. per serving
  Probability_of_Illness = out$X6      # Probability of illness
)

# Scatter plot with a linear fit
ggplot(data, aes(x = Campylobacter_per_serving, y = Probability_of_Illness)) +
  geom_point(alpha = 0.5, color = "blue") +        
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  labs(
    title = "Relationship Between Campylobacter spp. per Serving and Probability of Illness",
    x = "MPN of Campylobacter spp. per Serving ",
    y = "Probability of Illness"
  ) +
  theme_minimal()


