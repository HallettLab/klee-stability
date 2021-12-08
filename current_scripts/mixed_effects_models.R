## Significance Testing


## Models for Stability, Total Cover, & Variance with Cow vs. Wildlife split
## MODELS for FIGURE 1 ##
## how is stability predicted by varying groups of herbivores?
fitsth <- lmer(stability~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth)
fitsth
dredge(fitsth)

## best fitting model for stability
fitsth_best <- lmer(stability~cows+wildlife + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitsth_best)



## how is total cover predicted by varying groups of herbivores?
fittch <- lmer(pinhits~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fittch)
fittch
dredge(fittch)

## how is variance predicted by varying groups of herbivores?
fitvarh <- lmer(variance~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitvarh)
fitvarh
dredge(fitvarh)


## Models for Biotic Mechanisms with Cow vs. Wildlife split
## how is the variance ratio predicted by varying groups of herbivores?
fitcvrh <- lmer(classicVR~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitcvrh)
fitcvrh
dredge(fitcvrh)

fitcvr_best <- lmer(classicVR~cows+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitcvr_best)


## how is dominant species population stability predicted by varying groups of herbivores?
fitdpsh <- lmer(meanpopstab~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitdpsh)
fitdpsh
dredge(fitdpsh)

fitdpsh_best <- lmer(meanpopstab~1 + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitdpsh_best)


## how is richness predicted by varying groups of herbivores?
fitrih <- lmer(meanrich~cows+wildlife+mega + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitrih)
fitrih
dredge(fitrih)

fitrih_best <- lmer(meanrich~wildlife + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitrih_best)


## Mixed Effects Models for Stability vs Mechanisms
```{r}
## Stability vs. Mechanisms model 

## Check whether any factors are highly correlated (above 0.7 wouldn't put in a multiple regression together)

## Test Correlations b/w variables
cor.test(stability_mechanisms$meanpopstab, stability_mechanisms$classicVR)
## - 0.14

cor.test(stability_mechanisms$meanrich, stability_mechanisms$classicVR)
## 0.25

cor.test(stability_mechanisms$meanrich, stability_mechanisms$meanpopstab)
## -0.21


## Model with all 3 stability mechanisms included as predictors
fitstabmech <- lmer(stability~meanrich+classicVR+meanpopstab +(1|BLOCK), data = stability_mechanisms, na.action = "na.fail")
summary(fitstabmech)
dredge(fitstabmech)

## test again to see if any of the predictors are highly correlated using the variance inflation factor
vif(fitstabmech)
## reasonably low vif values


fitstabmech_best <- lmer(stability~classicVR + meanpopstab + (1|BLOCK), data = stability_mechanisms, na.action = "na.fail")

summary(fitstabmech_best)

```
























### Do drought score & herbivore treatment predict stability or mechanisms?
```{r}
## join drought and stability moving window dataframes (10 year)
drst10yr <- left_join(dscore10, stabmw10, by = "timestep") %>%
  mutate(stability_diff = stability - lead(stability, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drst10yr$cows <- 0
drst10yr$cows[drst10yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drst10yr$cows <- factor(drst10yr$cows)

drst10yr$wildlife <- 0
drst10yr$wildlife[drst10yr$TREATMENT %in% c('W','WC', 'MW', 'MWC')] <- 1
#drst10yr$wildlife[drst10yr$TREATMENT %in% c('MW','MWC')] <- 2
drst10yr$wildlife <- factor(drst10yr$wildlife)

drst10yr$mega <- 0
drst10yr$mega[drst10yr$TREATMENT %in% c('MW','MWC')] <- 1
drst10yr$mega <- factor(drst10yr$mega)

## MODEL ##
## fitdrst <- lmer(mean_stability~Dscore*cows+ Dscore*wildlife+ Dscore*mega + (1|BLOCK), data = drst10yr, na.action = "na.fail")
fitdrst <- lmer(stability~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drst10yr, na.action = "na.fail")
summary(fitdrst)
fitdrst_dredge <- dredge(fitdrst)

fitdrst_dredge[1,] ## pull out first model somehow

fitdrst_best <- lmer(stability~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = drst10yr, na.action = "na.fail")
summary(fitdrst_best)


## join drought and stability moving window dataframes (5 year)
drst5yr <- left_join(dscore5, stabmw5, by = "timestep") %>%
  mutate(stability_diff = stability - lead(stability, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drst5yr$cows <- 0
drst5yr$cows[drst5yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drst5yr$cows <- factor(drst5yr$cows)

drst5yr$wildlife <- 0
drst5yr$wildlife[drst5yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drst5yr$wildlife[drst5yr$TREATMENT %in% c('MW','MWC')] <- 2
drst5yr$wildlife <- factor(drst5yr$wildlife)

drst5yr$mega <- 0
drst5yr$mega[drst5yr$TREATMENT %in% c('MW','MWC')] <- 1
drst5yr$mega <- factor(drst5yr$mega)

## MODEL ##
fitdrst5 <- lmer(stability~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drst5yr, na.action = "na.fail")
summary(fitdrst5)
dredge(fitdrst5)

fitdrst5_best <- lmer(stability~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = drst5yr, na.action = "na.fail")
summary(fitdrst5_best)

```

### Do drought and herbivore treatment predict biotic mechanisms?
```{r}
## Variance Ratio - 10 Years ##
drcvr10yr <- left_join(dscore10, cvrmw10, by = "timestep") %>%
  mutate(cVR_diff = classicVR - lead(classicVR, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drcvr10yr$cows <- 0
drcvr10yr$cows[drcvr10yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drcvr10yr$cows <- factor(drcvr10yr$cows)

drcvr10yr$wildlife <- 0
drcvr10yr$wildlife[drcvr10yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drcvr10yr$wildlife[drcvr10yr$TREATMENT %in% c('MW','MWC')] <- 2
drcvr10yr$wildlife <- factor(drcvr10yr$wildlife)

drcvr10yr$mega <- 0
drcvr10yr$mega[drcvr10yr$TREATMENT %in% c('MW','MWC')] <- 1
drcvr10yr$mega <- factor(drcvr10yr$mega)

## MODEL ##
fitdrcvr <- lmer(classicVR~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drcvr10yr, na.action = "na.fail")
summary(fitdrcvr)
dredge(fitdrcvr)

fitdrcvr_best <- lmer(classicVR~Dscore+cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = drcvr10yr, na.action = "na.fail")
summary(fitdrcvr_best)

## Variance Ratio - 5 Years ##
drcvr5yr <- left_join(dscore5, cvrmw5, by = "timestep") %>%
  mutate(cVR_diff = classicVR - lead(classicVR, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drcvr5yr$cows <- 0
drcvr5yr$cows[drcvr5yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drcvr5yr$cows <- factor(drcvr5yr$cows)

drcvr5yr$wildlife <- 0
drcvr5yr$wildlife[drcvr5yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drcvr5yr$wildlife[drcvr5yr$TREATMENT %in% c('MW','MWC')] <- 2
drcvr5yr$wildlife <- factor(drcvr5yr$wildlife)

drcvr5yr$mega <- 0
drcvr5yr$mega[drcvr5yr$TREATMENT %in% c('MW','MWC')] <- 1
drcvr5yr$mega <- factor(drcvr5yr$mega)

## MODEL ##
fitdrcvr5 <- lmer(classicVR~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drcvr5yr, na.action = "na.fail")
summary(fitdrcvr5)
dredge(fitdrcvr5)




## Population Stability - 10 Years ##
drpopst10yr <- left_join(dscore10, spstmw10, by = "timestep") %>%
  mutate(popst_diff = mean_popst - lead(mean_popst, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drpopst10yr$cows <- 0
drpopst10yr$cows[drpopst10yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drpopst10yr$cows <- factor(drpopst10yr$cows)

drpopst10yr$wildlife <- 0
drpopst10yr$wildlife[drpopst10yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drpopst10yr$wildlife[drpopst10yr$TREATMENT %in% c('MW','MWC')] <- 2
drpopst10yr$wildlife <- factor(drpopst10yr$wildlife)

drpopst10yr$mega <- 0
drpopst10yr$mega[drpopst10yr$TREATMENT %in% c('MW','MWC')] <- 1
drpopst10yr$mega <- factor(drpopst10yr$mega)

## MODEL ##
fitdrps <- lmer(mean_popst~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drpopst10yr, na.action = "na.fail")
summary(fitdrps)
dredge(fitdrps)

fitdrps_best <- lmer(mean_popst~cows+wildlife + (1|BLOCK) + (1|Unique_ID), data = drpopst10yr, na.action = "na.fail")
summary(fitdrps_best)



## Population Stability - 5 Years ##
drpopst5yr <- left_join(dscore5, spstmw5, by = "timestep") %>%
  mutate(popst_diff = mean_popst - lead(mean_popst, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drpopst5yr$cows <- 0
drpopst5yr$cows[drpopst5yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drpopst5yr$cows <- factor(drpopst5yr$cows)

drpopst5yr$wildlife <- 0
drpopst5yr$wildlife[drpopst5yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drpopst5yr$wildlife[drpopst5yr$TREATMENT %in% c('MW','MWC')] <- 2
drpopst5yr$wildlife <- factor(drpopst5yr$wildlife)

drpopst5yr$mega <- 0
drpopst5yr$mega[drpopst5yr$TREATMENT %in% c('MW','MWC')] <- 1
drpopst5yr$mega <- factor(drpopst5yr$mega)

## MODEL ##
fitdrps5 <- lmer(mean_popst~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drpopst5yr, na.action = "na.fail")
summary(fitdrps5)
dredge(fitdrps5)





## Species Richness - 10 Years ##
drrich10yr <- left_join(dscore10, richmw10, by = "timestep") %>%
  mutate(meanrich_diff = richness - lead(richness, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drrich10yr$cows <- 0
drrich10yr$cows[drrich10yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drrich10yr$cows <- factor(drrich10yr$cows)

drrich10yr$wildlife <- 0
drrich10yr$wildlife[drrich10yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drrich10yr$wildlife[drrich10yr$TREATMENT %in% c('MW','MWC')] <- 2
drrich10yr$wildlife <- factor(drrich10yr$wildlife)

drrich10yr$mega <- 0
drrich10yr$mega[drrich10yr$TREATMENT %in% c('MW','MWC')] <- 1
drrich10yr$mega <- factor(drrich10yr$mega)

## MODEL ##
fitdrri <- lmer(richness~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drrich10yr, na.action = "na.fail")
summary(fitdrri)
dredge(fitdrri)

fitdrri_best <- lmer(richness~Dscore+wildlife + (1|BLOCK) + (1|Unique_ID), data = drrich10yr, na.action = "na.fail")
summary(fitdrri_best)


## Species Richness - 5 Years ##
drrich5yr <- left_join(dscore5, richmw5, by = "timestep") %>%
  mutate(meanrich_diff = richness - lead(richness, default = NA, order_by = timestep))

## add columns identifying which herbivore groups were present
drrich5yr$cows <- 0
drrich5yr$cows[drrich5yr$TREATMENT %in% c('C','WC', 'MWC')] <- 1
drrich5yr$cows <- factor(drrich5yr$cows)

drrich5yr$wildlife <- 0
drrich5yr$wildlife[drrich5yr$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#drrich5yr$wildlife[drrich5yr$TREATMENT %in% c('MW','MWC')] <- 2
drrich5yr$wildlife <- factor(drrich5yr$wildlife)

drrich5yr$mega <- 0
drrich5yr$mega[drrich5yr$TREATMENT %in% c('MW','MWC')] <- 1
drrich5yr$mega <- factor(drrich5yr$mega)

## MODEL ##
fitdrri5 <- lmer(richness~Dscore+cows+wildlife+mega + (1|BLOCK) + (1|Unique_ID), data = drrich5yr, na.action = "na.fail")
dredge(fitdrri5)


### Do biotic mechanisms predict stability? 
## first, need to combine data into the one dataframe
j <- left_join(drst10yr, drcvr10yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought", "Unique_ID")) #%>%
#select(-SE, -SE_cVR)

jj <- left_join(j, drpopst10yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought", "Unique_ID")) #%>%
#select(-SE_popst)

stabmechmw10 <- left_join(jj, drrich10yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought", "Unique_ID")) ##select(-SE_rich)


fitstmechmw <- lmer(stability~classicVR+mean_popst+richness + (1|BLOCK) + (1|Unique_ID), data = stabmechmw10, na.action = "na.fail")
dredge(fitstmechmw)

fitstmechmw_best <- lmer(stability~classicVR+mean_popst + (1|BLOCK) + (1|Unique_ID), data = stabmechmw10, na.action = "na.fail")

summary(fitstmechmw_best)
