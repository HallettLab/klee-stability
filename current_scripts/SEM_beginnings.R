## Structural Equation Modeling 
## Fit piecewise model with random effects

source("current_scripts/finalprep_postcalcs.R")

library(tidyverse)
library(piecewiseSEM)
library(multcompView)
library(lme4)


## model with Treatment as a factor
stability_psem <- psem(
  
  lm(stability~Dscore+TREATMENT + classicVR + mean_popst + richness, data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+TREATMENT + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+TREATMENT + (1|BLOCK), data = dmw10)
  
)

## view the psem object
stability_psem

## derive the basis set 
    ## need to figure out what this means....
basisSet(stability_psem)
## looks like there are independence claims representing the missing paths from 
## mean_popst -> richness (conditional on drought & herbivory) and mean_popst -> the variance ratio
## conditional on drought, herbivory, and richness

## evaluate tests of directed separation
dSep(stability_psem, .progressBar = FALSE)
## it seems like the relationship between vr and pop stability might be significant?
## does this mean we need to include this relationship in the model to have a better fit?

summary(stability_psem)
## based on the VR ~ pop stability relationship having a significant p-val in test of directed
## separation AND Fisher's C having a p-val = 0... I don't think this model is quite right.
## Also get the following Error: In B * (sd.x/sd.y) : longer object length is not a multiple of shorter object length



AIC(lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK), data = dmw10))
AIC(lm(classicVR~Dscore+TREATMENT + richness, data = dmw10))
AIC(lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK), data = dmw10))



## Not sure the following is a good approach as Lefcheck's SEM book recommends to NOT
## iteratively re-add paths until adequate fit is achieved...

## try the model with a relationship between synchrony and dominant species population stability
stability_psem2 <- psem(
  
  lm(stability~Dscore+TREATMENT + classicVR + mean_popst + richness, data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+TREATMENT + richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+TREATMENT + classicVR + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+TREATMENT + (1|BLOCK), data = dmw10)
  
)

summary(stability_psem2)
## including VR as a predictor of dominant species population stability gave: 

    ## much lower AIC (80.74 compared to 118)

    ## tests of directed separation - only testing the relationship b/w pop stability 
    ## & richness now. The p-value is not significant.

## is this an informative claim though? what would the mechanistic meaning be?






## try the model without a relationship between synchrony and richness; 
## but WITH a relationship between synchrony and population stability
stability_psem3 <- psem(
  
  lm(stability~Dscore+TREATMENT + classicVR + mean_popst + richness, data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+TREATMENT + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+TREATMENT + classicVR + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+TREATMENT + (1|BLOCK), data = dmw10)
  
)

summary(stability_psem3)
## it seems like the richness -> synchrony path is not informative?






## Try models with separate herbivore categories
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






## Try Multi-Group Modeling... 

## removing treatment, will bring this piece in later
stability_psemMG <- psem(
  
  lmer(stability~Dscore + classicVR + mean_popst + richness, data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lm(classicVR~Dscore + richness, data = dmw10),
  
  lm(mean_popst~Dscore, data = dmw10),
  
  lm(richness~Dscore, data = dmw10)
)
## this approach only seems to work if you take out random effects
multigroup(stability_psemMG, group = "TREATMENT")







## Check correlations between variables
cor.test(dmw10$classicVR, dmw10$mean_popst, 
                   alternative = "two.sided", 
                   method = "pearson")
  ## cor = -0.159, p-val = 0.015

cor.test(dmw10$classicVR, dmw10$richness, 
         alternative = "two.sided", 
         method = "pearson")
## cor = -0.180, p-val = 0.006

cor.test(dmw10$richness, dmw10$mean_popst, 
         alternative = "two.sided", 
         method = "pearson")
## cor = -0.189, p-val = 0.004





