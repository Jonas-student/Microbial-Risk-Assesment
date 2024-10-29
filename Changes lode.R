library(coda)


## Output from the simulation model
out <- data.frame(simulation(N = 100000 , FreezeOnly = 0))


## 3.1 What is the amount of Campylobacter spp. to which a consumer is exposed 
## in a single serving of poultry meat? !!--> change y-axis!!

# Visualisations
plot1 <- ggplot(aes(x=X5) , data=out) + geom_histogram(aes(y = ..density..),fill="#69b3a2",
                                                       binwidth = 1 , alpha=1 ) + 
  labs(y = "Density", x = "MPN per serving") + theme_minimal() + 
  ylim(0,0.6)

plot2 <- ggplot(aes(x=X5) , data=out) + geom_histogram(aes(y = ..density..),
                                                       fill="#69b3a2", color="#e9ecef", binwidth = 1  ) + 
  labs(y = "Density", x = "MPN per serving") + theme_minimal() + xlim(-1,80)+
  geom_vline(xintercept=10, color="gray", linetype="dashed", size=0.7) + 
  ylim(0,0.6)

grid.arrange(plot1 , plot2 , ncol=2 , top = "Number of Campylobacter spp. per serving")


# Summary statistics !!add aditional one (HPD)!! (Lode)
sum(out$X5<49)/100000
median(out$X5)
overall_summary <- summary(out)

mcmc_out <- as.mcmc(out)
HPDinterval(mcmc_out, prob= 0.95)

## 3.2 How common is an exposure to doses > 10 MPN/meat serving? (Lode)
sum(out$X5>10)/100000


## 3.3 What is the prevalence of meat contamination? (Lode)
sum(out$X5>0)/100000


## 3.4 What is the probability of illness for a single meal of chicken? (Lode)
mean(out$X6)

ggplot(aes(x=X6) , data=out) +  geom_histogram(fill="#69b3a2", color="#e9ecef",
                                               alpha=0.8 , binwidth = 0.01) +
  ggtitle("Probability of illness per serving") + labs(y = "Density", x = "Probability of illnes")+
  theme_minimal() + xlim(-0.01,1.01)
