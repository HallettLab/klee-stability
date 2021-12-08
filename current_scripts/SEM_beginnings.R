## Structural Equation Modeling 
## Fit piecewise model with random effects

source("current_scripts/finalprep_postcalcs.R")

library(tidyverse)
library(piecewiseSEM)
library(multcompView)
library(lme4)


#library(ape) #Version 3.3
#library(caper)

model <- psem(
  lmer(stability~Dscore+TREATMENT + classicVR + mean_popst + (1|BLOCK) + (1|Unique_ID), data=dmw10),
  
  lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK) + (1|Unique_ID), data = dmw10),
  
  lmer(mean_popst~Dscore+TREATMENT + (1|BLOCK) + (1|Unique_ID), data = dmw10),
  
  lmer(richness~Dscore+TREATMENT + (1|BLOCK) + (1|Unique_ID), data = dmw10)
  
)

summary(model)


model2 <- psem(
  lmer(stability~Dscore+cows+wildlife+mega + classicVR + mean_popst + (1|BLOCK) + (1|Unique_ID), data=dmw10),
  
  lmer(classicVR~Dscore+cows+wildlife+mega + richness + (1|BLOCK) + (1|Unique_ID), data = dmw10),
  
  lmer(mean_popst~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10),
  
  lmer(richness~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10)
  
)

summary(model2)



