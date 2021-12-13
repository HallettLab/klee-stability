## Mixed Effects Models ##

## read in data
source("current_scripts/finalprep_postcalcs.R")

## load packages
library(tidyverse)
library(lme4)
library(MuMIn)

## MODEL for FIGURE 3A ##
## how is stability predicted by varying groups of herbivores?
fitsth <- lmer(stability~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth)
fitsth
dredge(fitsth)

## best fitting model for stability
fitsth_best <- lmer(stability~cows+wildlife + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth_best)


## MODEL FOR FIGURE 3B ##
### Do drought score & herbivore treatment predict stability or mechanisms?
fitdrst <- lmer(stability~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrst)
dredge(fitdrst)

## best fitting model for 10yr stability
fitdrst_best <- lmer(stability~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrst_best)




## MODEL FOR FIGURE 4A ##
## how is the variance ratio predicted by varying groups of herbivores?
fitcvrh <- lmer(classicVR~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitcvrh)
dredge(fitcvrh)

fitcvr_best <- lmer(classicVR~cows+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitcvr_best)


## MODEL FOR FIGURE 4B ## 
## how is the variance ratio (10 year) predicted by drought & herbivory?
fitdrcvr <- lmer(classicVR~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrcvr)
dredge(fitdrcvr)

fitdrcvr_best <- lmer(classicVR~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrcvr_best)


## MODEL FOR FIGURE 4C ##
## how is dominant species population stability predicted by varying groups of herbivores?
fitdpsh <- lmer(meanpopstab~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitdpsh)
dredge(fitdpsh)

fitdpsh_best <- lmer(meanpopstab~1 + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitdpsh_best)


## MODEL FOR FIGURE 4D ## 
## how is population stability (10 year) predicted by drought & herbivory?
fitdrps <- lmer(mean_popst~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrps)
dredge(fitdrps)

fitdrps_best <- lmer(mean_popst~cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrps_best)


## MODEL FOR FIGURE 4E ##
## how is richness predicted by varying groups of herbivores?
fitrih <- lmer(meanrich~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitrih)
dredge(fitrih)

fitrih_best <- lmer(meanrich~wildlife + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitrih_best)
## this one has singularity issues... not sure what to do... 


## MODEL FOR FIGURE 4F ##
## how is richness (10 year) predicted by drought and herbivory?
fitdrri <- lmer(richness~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrri)
dredge(fitdrri)

fitdrri_best <- lmer(richness~Dscore+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrri_best)
## this one has singularity issues... not sure what to do... 


## MODEL FOR FIGURE 5 ##
## Check whether any factors are highly correlated (above 0.7 wouldn't put in a multiple regression together)
cor.test(stability_mechanisms$meanpopstab, stability_mechanisms$classicVR) ## - 0.14
cor.test(stability_mechanisms$meanrich, stability_mechanisms$classicVR) ## 0.25
cor.test(stability_mechanisms$meanrich, stability_mechanisms$meanpopstab) ## -0.21

## Model with all 3 stability mechanisms included as predictors
fitstabmech <- lmer(stability~meanrich+classicVR+meanpopstab +(1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitstabmech)
dredge(fitstabmech)

## test again to see if any of the predictors are highly correlated using the variance inflation factor
vif(fitstabmech)
## reasonably low vif values

fitstabmech_best <- lmer(stability~classicVR + meanpopstab + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitstabmech_best)

