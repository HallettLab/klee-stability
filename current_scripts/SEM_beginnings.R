## Structural Equation Modeling 
## Fit piecewise model with random effects

source("current_scripts/finalprep_postcalcs.R")

library(tidyverse)
library(piecewiseSEM)
library(multcompView)
library(lme4)
#library(ape) #Version 3.3
#library(caper)

## model with Treatment as a factor
model <- psem(
  lm(stability~Dscore+TREATMENT + classicVR + mean_popst + richness, data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  
  lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+TREATMENT + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+TREATMENT + (1|BLOCK), data = dmw10)
  
)

summary(model)

m1 <- lmer(stability~Dscore+TREATMENT + classicVR + mean_popst + richness + (1|BLOCK), data=dmw10)
summary(m1)
m2 <- lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK), data = dmw10)
m3 <- lmer(mean_popst~Dscore+TREATMENT + (1|BLOCK), data = dmw10)
m4 <- lmer(richness~Dscore+TREATMENT + (1|BLOCK), data = dmw10)
## model with herbivore groups as separate factors
model2 <- psem(
  lmer(stability~Dscore+cows+wildlife+mega + classicVR + mean_popst + (1|BLOCK) + (1|Unique_ID), data=dmw10),
  
  lmer(classicVR~Dscore+cows+wildlife+mega + richness + (1|BLOCK) + (1|Unique_ID), data = dmw10),
  
  lmer(mean_popst~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10),
  
  lmer(richness~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10)
  
)

summary(model2)

## extract coefficients for model 2
coefs(model2)
## this is giving coefficients for everything except categorical variables

summary.psem(model2)



