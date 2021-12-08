## Mixed Effects Models: Dom Sp Population Stability & Treatment
```{r}
## make wide dataframe first
popstabwide <- dompopstab %>%
  select(-temp_mean, -sdhits) %>%
  pivot_wider(names_from = SPECIES, values_from = sp_stability)

popstab <- left_join(popstabwide, stability, by = c("Unique_ID", "TREATMENT"))
colnames(popstab)

## B. lachnantha pop stability by Herbivore Treatment
fitgrBl <- lmer(Brachiaria_lachnantha~TREATMENT + (1|BLOCK), data = popstab)
summary(fitgrBl)

fitBlnull <- lmer(Brachiaria_lachnantha~1 + (1|BLOCK), data = popstab)
summary(fitBlnull)

AIC(fitgrBl)
AIC(fitBlnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrBl, specs = pairwise ~ TREATMENT)



## L. nutans pop stability by Herbivore Treatment
fitgrLn <- lmer(Lintonia_nutans~TREATMENT + (1|BLOCK), data = popstab)
summary(fitgrLn)

fitLnnull <- lmer(Lintonia_nutans~1 + (1|BLOCK), data = popstab)
summary(fitLnnull)

AIC(fitgrLn)
AIC(fitLnnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrLn, specs = pairwise ~ TREATMENT)



## P. mezianum pop stability by Herbivore Treatment
fitgrPm <- lmer(Pennisetum_mezianum~TREATMENT + (1|BLOCK), data = popstab)
summary(fitgrPm)

fitPmnull <- lmer(Pennisetum_mezianum~1 + (1|BLOCK), data = popstab)
summary(fitPmnull)

AIC(fitgrPm)
AIC(fitPmnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrPm, specs = pairwise ~ TREATMENT)




## P. stramineum pop stability by Herbivore Treatment
fitgrPs <- lmer(Pennisetum_stramineum~TREATMENT + (1|BLOCK), data = popstab)
summary(fitgrPs)

fitPsnull <- lmer(Pennisetum_stramineum~1 + (1|BLOCK), data = popstab)
summary(fitPsnull)

AIC(fitgrPs)
AIC(fitPsnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrPs, specs = pairwise ~ TREATMENT)




## T. triandra pop stability by Herbivore Treatment
fitgrTt <- lmer(Themeda_triandra~TREATMENT + (1|BLOCK), data = popstab)
summary(fitgrTt)

fitTtnull <- lmer(Themeda_triandra~1 + (1|BLOCK), data = popstab)
summary(fitTtnull)

AIC(fitgrTt)
AIC(fitTtnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrTt, specs = pairwise ~ TREATMENT)

```










## Mixed Effects Models: Herbivory & Stability/Mechanisms
```{r}
## How does herbivore treatment change stability & stability mechanisms?
## use block as a random effect

stability_mechanisms <- stability_mechanisms %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))

## Stability by Herbivory Treatment
fitgrst <- lmer(stability~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrst)

fitstnull <- lmer(stability~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitstnull)
AIC(fitstnull)
AIC(fitgrst)
## use AIC to compare to model where Tx is taken out
## diff b/w AIC vals > 2; & lower AIC score is better
## treatment makes a diff here.
## do this for each of the models

## for diff b/w tx -> use emmeans 
emmeans(fitgrst, specs = pairwise ~ TREATMENT)


## Variance by Herbivore Treatment
fitgrvar <- lmer(variance~TREATMENT + (1|BLOCK), data = stability_mechanisms)

summary(fitgrvar)

fitvarnull <- lmer(variance~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitvarnull)

AIC(fitgrvar)
AIC(fitvarnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrvar, specs = pairwise ~ TREATMENT)


## Total Cover by Herbivore Treatment
fitgrtc <- lmer(pinhits~TREATMENT + (1|BLOCK), data = stability_mechanisms)

summary(fitgrtc)

fittcnull <- lmer(pinhits~1 + (1|BLOCK), data = stability_mechanisms)
summary(fittcnull)

AIC(fitgrtc)
AIC(fittcnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrtc, specs = pairwise ~ TREATMENT)



## Classic Variance Ratio by Herbivory Treatment (ALL SPECIES)
fitgrcvr <- lmer(classicVR~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrcvr)

fitcvrnull <- lmer(classicVR~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitcvrnull)

AIC(fitgrcvr)
AIC(fitcvrnull)
## treatment does make a difference here

## difference between treatments
emmeans(fitgrcvr, specs = pairwise ~ TREATMENT)



## Classic Variance Ratio by Herbivory Treatment (DOMINANT SPECIES)
fitgrb5cvr <- lmer(b5classicVR~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrb5cvr)

fitb5cvrnull <- lmer(b5classicVR~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitb5cvrnull)

AIC(fitgrb5cvr)
AIC(fitb5cvrnull)
## treatment does make a difference here

## difference between treatments
emmeans(fitgrb5cvr, specs = pairwise ~ TREATMENT)


## Dominance by Herbivory Treatment 
#fitgrdo <- lmer(mean_dom~TREATMENT + (1|BLOCK), data = stability_mechanisms)
#summary(fitgrdo)

#fitdonull <- lmer(mean_dom~1 + (1|BLOCK), data = stability_mechanisms)
#summary(fitdonull)
#AIC(fitgrdo)
#AIC(fitdonull)
## treatment does make a difference here

## difference between treatments
#emmeans(fitgrdo, specs = pairwise ~ TREATMENT)


## Richness by Herbivory Treatment
fitgrri <- lmer(meanrich~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrri)

fitrinull <- lmer(meanrich~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitrinull)
AIC(fitgrri)
AIC(fitrinull)
## treatment slightly makes a difference 

## difference between treatments
emmeans(fitgrri, specs = pairwise ~ TREATMENT)


## CV Dominance by Treatment
fitgrcvdo <- lmer(CVdom~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrcvdo)

fitcvdonull <- lmer(CVdom~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitcvdonull)

AIC(fitgrcvdo)
AIC(fitcvdonull)
## treatment does make a difference here

## difference between treatments
emmeans(fitgrcvdo, specs = pairwise ~ TREATMENT)


## CV Richness by Treatment
fitgrcvri <- lmer(CVrich~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrcvri)

fitcvrinull <- lmer(CVrich~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitcvrinull)

AIC(fitgrcvri)
AIC(fitcvrinull)
## treatment does make a difference here

## difference between treatments
emmeans(fitgrcvri, specs = pairwise ~ TREATMENT)



## Dominant Species Stability by treatment
fitgrdsst <- lmer(meanpopstab~TREATMENT + (1|BLOCK), data = stability_mechanisms)
summary(fitgrdsst)

fitdsstnull <- lmer(meanpopstab~1 + (1|BLOCK), data = stability_mechanisms)
summary(fitdsstnull)

AIC(fitgrdsst)
AIC(fitdsstnull)
## treatment does make a difference here

## difference between treatments
emmeans(fitgrdsst, specs = pairwise ~ TREATMENT)
```

## Mixed Effects Models: Herbivory & TSVR
```{r}

tsVR <- tsVR %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments


## FULL COMMUNITY ##
## Do Long TSVR values vary by herbivore treatment?
fitgrLongVR <- lmer(longVR~TREATMENT + (1|BLOCK), data = tsVR)
summary(fitgrLongVR)

fitLongVRnull <- lmer(longVR~1 + (1|BLOCK), data = tsVR)

## Check AIC values
AIC(fitgrLongVR)
AIC(fitLongVRnull)

deltaAIC <- AIC(fitLongVRnull) - AIC(fitgrLongVR)
deltaAIC

## for diff b/w tx -> use emmeans 
emmeans(fitgrLongVR, specs = pairwise ~ TREATMENT)


## FULL COMMUNITY
## Do short TSVR values vary by herbivore treatment?
fitgrShortVR <- lmer(shortVR~TREATMENT + (1|BLOCK), data = tsVR)
summary(fitgrShortVR)

fitgrShortVRnull <- lmer(shortVR~1 + (1|BLOCK), data = tsVR)

## Check AIC values
AIC(fitgrShortVR)
AIC(fitgrShortVRnull)

## for diff b/w tx -> use emmeans 
emmeans(fitgrShortVR, specs = pairwise ~ TREATMENT)



## Do TSVR values vary by dominant species?
b5tsVR <- b5tsVR %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments


# DOMINANT SPECIES
## Do Long TSVR values vary by herbivore treatment?
fitb5grLongVR <- lmer(b5longVR~TREATMENT + (1|BLOCK), data = b5tsVR)
summary(fitb5grLongVR)

fitb5LongVRnull <- lmer(b5longVR~1 + (1|BLOCK), data = b5tsVR)

## Check AIC values
AIC(fitb5grLongVR)
AIC(fitb5LongVRnull)

## for diff b/w tx -> use emmeans 
emmeans(fitb5grLongVR, specs = pairwise ~ TREATMENT)



## DOMINANT SPECIES ##
## Do short TSVR values vary by herbivore treatment?
fitb5grShortVR <- lmer(b5shortVR~TREATMENT + (1|BLOCK), data = b5tsVR)
summary(fitb5grShortVR)

fitb5ShortVRnull <- lmer(b5shortVR~1 + (1|BLOCK), data = b5tsVR)

## Check AIC values
AIC(fitb5grShortVR)
AIC(fitb5ShortVRnull)

## for diff b/w tx -> use emmeans 
emmeans(fitb5grShortVR, specs = pairwise ~ TREATMENT)

```