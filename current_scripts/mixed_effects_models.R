## Mixed Effects Models ##

## read in data
source("finalprep_postcalcs.R")

## load packages
library(tidyverse)
library(lme4)
library(MuMIn)

cor(stability_mechanisms$classicVR, stability_mechanisms$meanpopstab)
cor(stability_mechanisms$classicVR, stability_mechanisms$meanrich)
cor(stability_mechanisms$meanpopstab, stability_mechanisms$meanrich)


## MODEL for FIGURE 3A ##
## how is stability predicted by varying groups of herbivores?
fitsth <- lmer(stability~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
dredge(fitsth)

## best fitting, most parsimonious model for stability
fitsth_best <- lmer(stability~cows + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth_best)
## CODE EDIT: Here the most parsimonious model with delta AIC <2 is the one with only cows as a predictor, even though cows + wildlife is ranked first. I would present this slightly differently in text - let's chat about it.

## model diagnostics
dev.off()
#Default plot of standardised residuals versus fitted
plot(fitsth_best, resid(., scaled=TRUE) ~ fitted(.), abline = 0,pch=16, xlab="Fitted values", ylab="Standardised residuals")
## the points are concentrated at the ends of the figure... why??
## could this be a result of the categorical variable for herbivory?
## does the variance look constant across the fitted range?

## this is checking for heteroscedasticity: whether the standard deviations of a predicted variable, monitored over different values of the independent variable are non-constant 

#Plotting the residuals against each explanatory variable
plot(fitsth_best, resid(., scaled=TRUE) ~ cows, abline = 0,pch=16,xlab="Cows",ylab="Standardised residuals")
## this didn't work, covariate needs to be numeric. hmm. 

ggplot(data.frame(x1=stability_mechanisms$cows,pearson=residuals(fitsth_best,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()

## Normality of Residuals
dev.off()
qqnorm(resid(fitsth_best))
qqline(resid(fitsth_best))
## it looks like there are deviations from the line... 

#We can use a default plot of the standardised residuals versus fitted 
#But add | <random effect> to see it per level of the random effect
plot(fitsth_best, resid(., scaled=TRUE) ~ fitted(.)| BLOCK, abline = 0,pch=16,xlab="Fitted values",ylab="Standardised residuals")

#Calculate leverage
lev<-hat(model.matrix(fitsth_best))

#Plot leverage against standardised residuals
plot(resid(fitsth_best,type="pearson")~lev,las=1,ylab="Standardised residuals",xlab="Leverage")


#Calculate Cook's Distance
cd<-cooks.distance(fitsth_best)
#N.B. If Cook's distance is greater than 1 this highlights problematic datapoints

#Plot leverage and Cook's distance together
par(mfrow=c(1,1))
plot(lev,pch=16,col="red",ylim=c(0,1.2),las=1,ylab="Leverage/Cook's distance value")
points(cd,pch=17,col="blue")
points(x=150,y=1.1,pch=16,col="red")
points(x=150,y=0.9,pch=17,col="blue")
text(x=155,y=1.1,"Leverage",adj=c(0,0.5))
text(x=155,y=0.9,"Cook's distance",adj=c(0,0.5))


hist(as.vector(unlist(ranef(fitsth_best)$BLOCK)),breaks=seq(-2,1,0.25),border=NA)

lattice::dotplot(ranef(fitsth_best, condVar=TRUE))


fitsth_top_alt <- lmer(stability~cows + wildlife + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth_top_alt)

fitsth_top_alt2 <- lmer(stability~cows + mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth_top_alt2)



## MODEL FOR FIGURE 3B ##
### Do drought score & herbivore treatment predict stability or mechanisms?
fitdrst <- lmer(stability~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrst)
dredge(fitdrst)

## best fitting model for 10yr stability
fitdrst_best <- lmer(stability~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrst_best)

## model diagnostics
dev.off()
#Default plot of standardised residuals versus fitted
plot(fitdrst_best, resid(., scaled=TRUE) ~ fitted(.), abline = 0,pch=16, xlab="Fitted values", ylab="Standardised residuals")

#Plotting the residuals against each explanatory variable
plot(fitdrst_best, resid(., scaled=TRUE) ~ Dscore, abline = 0,pch=16,xlab="Drought Score",ylab="Standardised residuals")

## Normality of Residuals
dev.off()
qqnorm(resid(fitdrst_best))
qqline(resid(fitdrst_best))

fitdrst_best_alt <- lmer(stability~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrst_best_alt)



## MODEL FOR FIGURE 4A ##
## how is the variance ratio predicted by varying groups of herbivores?
fitcvrh <- lmer(classicVR~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
dredge(fitcvrh)

fitcvr_best <- lmer(classicVR~cows+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitcvr_best)


## MODEL FOR FIGURE 4B ## 
## how is the variance ratio (10 year) predicted by drought & herbivory?
fitdrcvr <- lmer(classicVR~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
dredge(fitdrcvr)

fitdrcvr_best <- lmer(classicVR~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrcvr_best)

plot(fitdrcvr_best)

## MODEL FOR FIGURE 4C ##
## how is dominant species population stability predicted by varying groups of herbivores?
fitdpsh <- lmer(meanpopstab~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
dredge(fitdpsh)

fitdpsh_best <- lmer(meanpopstab~1 + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitdpsh_best)


## MODEL FOR FIGURE 4D ## 
## how is population stability (10 year) predicted by drought & herbivory?
fitdrps <- lmer(mean_popst~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrps)
dredge(fitdrps)

fitdrps_best <- lmer(mean_popst~cows + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrps_best)
## CODE EDIT: Again, the model with just cows falls within <2 delta AIC of the best model, so that should be acknowledged in text.

fitdrpstop <- lmer(mean_popst~cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrpstop)

fitdrpstopo <- lmer(mean_popst~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrpstopo)




## MODEL FOR FIGURE 4E ##
## how is richness predicted by varying groups of herbivores?
fitrih <- lmer(meanrich~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
dredge(fitrih)

fitrih_best <- lm(meanrich~wildlife, data = stability_mechanisms, na.action = "na.fail")
summary(fitrih_best)
## this one has singularity issues... not sure what to do... 
## CODE EDIT: Re-run as a linear model without the random effect, you should get essentially the same answer because the data here aren't supporting the complexity of the random effects structure so it's estimating 0 random effects variance.

## MODEL FOR FIGURE 4F ##
## how is richness (10 year) predicted by drought and herbivory?
fitdrri <- lmer(richness~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
dredge(fitdrri)

fitdrri_best <- lmer(richness~Dscore+wildlife + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrri_best)
## this one has singularity issues... not sure what to do... 
## CODE EDIT: see suggestion above

fitdrritop <- lmer(richness~Dscore+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrritop)

fitdrritopo <- lmer(richness~Dscore+wildlife+cows + (1|BLOCK) + (1|Unique_ID), data = dmw10, na.action = "na.fail")
summary(fitdrritopo)

## MODEL FOR FIGURE 5 ##
## Check whether any factors are highly correlated (above 0.7 wouldn't put in a multiple regression together)
cor.test(stability_mechanisms$meanpopstab, stability_mechanisms$classicVR) ## - 0.14
cor.test(stability_mechanisms$meanrich, stability_mechanisms$classicVR) ## 0.25
cor.test(stability_mechanisms$meanrich, stability_mechanisms$meanpopstab) ## -0.21

## Model with all 3 stability mechanisms included as predictors
fitstabmech <- lmer(stability~meanrich+classicVR+meanpopstab +(1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
dredge(fitstabmech)

## test again to see if any of the predictors are highly correlated using the variance inflation factor
vif(fitstabmech)
## reasonably low vif values

fitstabmech_best <- lmer(stability~classicVR + meanpopstab + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitstabmech_best)

## diagnostic plots
ggplot(data.frame(x1=stability_mechanisms$classicVR,pearson=residuals(fitstabmech_best,type="pearson")),
       aes(x=x1,y=pearson)) +
  geom_point() +
  theme_bw()

ggplot(data.frame(x2=stability_mechanisms$meanpopstab,pearson=residuals(fitdrcvr_best,type="pearson")),
       aes(x=x2,y=pearson)) +
  geom_point() +
  theme_bw()
