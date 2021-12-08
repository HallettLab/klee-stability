### Linear regressions of drought score with stability 
```{r}
drst10yr <- left_join(dscore10, stabmw10, by = "timestep") %>%
  mutate(stability_diff = mean_stability - lead(mean_stability, default = NA, order_by = timestep),
         magdiff = abs(stability_diff))

Odr <- drst10yr %>%
  filter(TREATMENT == "O")

fitOdr <- lm(mean_stability~Dscore, data = Odr)
summary(fitOdr)

fitOdrdiff <- lm(stability_diff~Dscore, data = Odr)
summary(fitOdrdiff)
## not significant

Wdr <- drst10yr %>%
  filter(TREATMENT == "W")

fitWdr <- lm(mean_stability~Dscore, data = Wdr)
summary(fitWdr)

fitWdrdiff <- lm(stability_diff~Dscore, data = Wdr)
summary(fitWdrdiff)
## not significant

MWdr <- drst10yr %>%
  filter(TREATMENT == "MW")

fitMWdr <- lm(mean_stability~Dscore, data = MWdr)
summary(fitMWdr)

fitMWdrdiff <- lm(stability_diff~Dscore, data = MWdr)
summary(fitMWdrdiff)


Cdr <- drst10yr %>%
  filter(TREATMENT == "C")

fitCdr <- lm(mean_stability~Dscore, data = Cdr)
summary(fitCdr)

fitCdrdiff <- lm(stability_diff~Dscore, data = Cdr)
summary(fitCdrdiff)
## not significant

WCdr <- drst10yr %>%
  filter(TREATMENT == "WC")

fitWCdr <- lm(mean_stability~Dscore, data = WCdr)
summary(fitWCdr)

fitWCdrdiff <- lm(stability_diff~Dscore, data = WCdr)
summary(fitWCdrdiff)
## not significant

MWCdr <- drst10yr %>%
  filter(TREATMENT == "MWC")

fitMWCdr <- lm(mean_stability~Dscore, data = MWCdr)
summary(fitMWCdr)

fitMWCdrdiff <- lm(stability_diff~Dscore, data = MWCdr)
summary(fitMWCdrdiff)
## not significant
```






### Linear regressions of drought score with variance ratio
```{r}
## variance ratio
drcvr10yr <- left_join(dscore10, cvrmw10, by = "timestep") %>%
  mutate(cVR_diff = mean_cVR - lead(mean_cVR, default = NA, order_by = timestep),
         magdiff = abs(cVR_diff))

## O TREATMENT ##
Ocvr <- drcvr10yr %>%
  filter(TREATMENT == "O")

## mean VR vs. Drought score
fitOcvr <- lm(mean_cVR~Dscore, data = Ocvr)
summary(fitOcvr)
## not significant

## change in VR vs. Drought score
fitOcvrdiff <- lm(cVR_diff~Dscore, data = Ocvr)
summary(fitOcvrdiff)
## not significant


## W TREATMENT ## 
Wcvr <- drcvr10yr %>%
  filter(TREATMENT == "W")

## mean VR vs. Drought score
fitWcvr <- lm(mean_cVR~Dscore, data = Wcvr)
summary(fitWcvr)
## not significant 

## change in VR vs. Drought score
fitWcvrdiff <- lm(cVR_diff~Dscore, data = Wcvr)
summary(fitWcvrdiff)
## significant


## MW TREATMENT ## 
MWcvr <- drcvr10yr %>%
  filter(TREATMENT == "MW")

## mean VR vs. Drought score
fitMWcvr <- lm(mean_cVR~Dscore, data = MWcvr)
summary(fitMWcvr)
## significant

## change in VR vs. Drought score
fitMWcvrdiff <- lm(cVR_diff~Dscore, data = MWcvr)
summary(fitMWcvrdiff)
## not significant


## C TREATMENT ## 
Ccvr <- drcvr10yr %>%
  filter(TREATMENT == "C")

## mean VR vs. Drought score
fitCcvr <- lm(mean_cVR~Dscore, data = Ccvr)
summary(fitCcvr)
## marginally significant

## change in VR vs. Drought score
fitCcvrdiff <- lm(cVR_diff~Dscore, data = Ccvr)
summary(fitCcvrdiff)
## significant


## WC TREATMENT ##
WCcvr <- drcvr10yr %>%
  filter(TREATMENT == "WC")

## mean VR vs. Drought score
fitWCcvr <- lm(mean_cVR~Dscore, data = WCcvr)
summary(fitWCcvr)
## significant

## change in VR vs. Drought score
fitWCcvrdiff <- lm(cVR_diff~Dscore, data = WCcvr)
summary(fitWCcvrdiff)
## significant


## MWC TREATMENT ##
MWCcvr <- drcvr10yr %>%
  filter(TREATMENT == "MWC")

## mean VR vs. Drought score
fitMWCcvr <- lm(mean_cVR~Dscore, data = MWCcvr)
summary(fitMWCcvr)
## marginally significant

## change in VR vs. Drought score
fitMWCcvrdiff <- lm(cVR_diff~Dscore, data = MWCcvr)
summary(fitMWCcvrdiff)
## not significant
```



### Linear regressions of drought score with population stability
```{r}
## population stability
drpopst10yr <- left_join(dscore10, spstmw10, by = "timestep") %>%
  mutate(popst_diff = avgpopstab - lead(avgpopstab, default = NA, order_by = timestep),
         magdiff = abs(popst_diff))


Ops <- drpopst10yr %>%
  filter(TREATMENT == "O")

fitOps <- lm(avgpopstab~Dscore, data = Ops)
summary(fitOps)
## not significant

Wps <- drpopst10yr %>%
  filter(TREATMENT == "W")

fitWps <- lm(avgpopstab~Dscore, data = Wps)
summary(fitWps)
## not significant

MWps <- drpopst10yr %>%
  filter(TREATMENT == "MW")

fitMWps <- lm(avgpopstab~Dscore, data = MWps)
summary(fitMWps)
## not significant


Cps <- drpopst10yr %>%
  filter(TREATMENT == "C")

fitCps <- lm(avgpopstab~Dscore, data = Cps)
summary(fitCps)
## not significant


WCps <- drpopst10yr %>%
  filter(TREATMENT == "WC")

fitWCps <- lm(avgpopstab~Dscore, data = WCps)
summary(fitWCps)
## not significant

MWCps <- drpopst10yr %>%
  filter(TREATMENT == "MWC")

fitMWCps <- lm(avgpopstab~Dscore, data = MWCps)
summary(fitMWCps)
## not significant

```






## Fit linear models for 5-year cvr stability corr
```{r}
stdr5 <- left_join(stabmw5, dscore5, by = "timestep")

## separate out 5 year window for correlation figures
mw5cvrstabfig <- stab_cvr_mw %>%
  filter(window_size == "5")

corrfigVRstdr5 <- left_join(stdr5, mw5cvrstabfig, by = c("TREATMENT", "timestep", "window_size", "mean_stability", "SE"))


stcvrO5 <- corrfigVRstdr5 %>%
  filter(TREATMENT == "O")

## fit linear model
fitstcvrO5 <- lm(mean_stability~mean_cVR, data = stcvrO5)
summary(fitstcvrO5)

stcvrW5 <- corrfigVRstdr5 %>%
  filter(TREATMENT == "W")

fitstcvrW5 <- lm(mean_stability~mean_cVR, data = stcvrW5)
summary(fitstcvrW5)


stcvrMW5 <- corrfigVRstdr5 %>%
  filter(TREATMENT == "MW")

fitstcvrMW5 <- lm(mean_stability~mean_cVR, data = stcvrMW5)
summary(fitstcvrMW5)


stcvrC5 <- corrfigVRstdr5 %>%
  filter(TREATMENT == "C")

fitstcvrC5 <- lm(mean_stability~mean_cVR, data = stcvrC5)
summary(fitstcvrC5)


stcvrWC5 <- corrfigVRstdr5 %>%
  filter(TREATMENT == "WC")

fitstcvrWC5 <- lm(mean_stability~mean_cVR, data = stcvrWC5)
summary(fitstcvrWC5)


stcvrMWC5 <- corrfigVRstdr5 %>%
  filter(TREATMENT == "MWC")

fitstcvrMWC5 <- lm(mean_stability~mean_cVR, data = stcvrMWC5)
summary(fitstcvrMWC5)
```

## Fit linear models for 10-year cvr stability corr
```{r, echo=FALSE, fig.width=9, fig.height=3}
## separate out 10 year window for correlation figures
mw10cvrstabfig <- stab_cvr_mw %>%
  filter(window_size == "10")

## try to get BLOCK into the data frame
#stab_cvr_mw10 <- left_join(stab_mw_tx, cVR_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
# filter(window_size == 10)

stdr10 <- left_join(stabmw10, dscore10, by = "timestep")
stcvrmw10_models <- left_join(stdr10, cvrmw10, by = c("timestep", "TREATMENT", "window_size"))

corrfigVRstdr10 <- left_join(stdr10, mw10cvrstabfig, by = c("TREATMENT", "timestep", "window_size", "mean_stability", "SE"))

## Run linear models to get coefficients for line of best fit
fitstcvr <- lm(mean_stability~mean_cVR, data = corrfigVRstdr10)
summary(fitstcvr)


## O TREATMENT ##
stcvrO <- stcvrmw10_models %>%
  filter(TREATMENT == "O")
## linear model
fitstcvrO <- lm(mean_stability~mean_cVR, data = stcvrO)
summary(fitstcvrO)

## mixed effects model 
## want BLOCK as a random effect
## might need to redo moving window analysis to retain BLOCK data... 
fitOstvr10 <- lmer(mean_stability ~ mean_cVR + (1|BLOCK), data = stcvrO)
summary(fitOstvr10)
fitOstnull10 <- lmer(mean_stability~1 + (1|BLOCK), data = stcvrO)
summary(fitOstnull10)

## compare AIC values
AIC(fitOstvr10)
AIC(fitOstnull10)

## deltaAIC
AIC(fitOstvr10) - AIC(fitOstnull10)
## no diff b/w this and the null model


## W TREATMENT ##
stcvrW <- stcvrmw10_models %>%
  filter(TREATMENT == "W")
## linear model
fitstcvrW <- lm(mean_stability~mean_cVR, data = stcvrW)
summary(fitstcvrW)

## mixed effects model
fitWstvr10 <- lmer(mean_stability ~ mean_cVR + (1|BLOCK), data = stcvrW)
summary(fitWstvr10)
fitWstnull10 <- lmer(mean_stability~1 + (1|BLOCK), data = stcvrW)
summary(fitWstnull10)

## compare AIC values
AIC(fitWstvr10)
AIC(fitWstnull10)

## deltaAIC
AIC(fitWstvr10) - AIC(fitWstnull10)
## model with mean_cVR explains better than the null model


## MW TREATMENT ##
stcvrMW <- stcvrmw10_models %>%
  filter(TREATMENT == "MW")
## linear model
fitstcvrMW <- lm(mean_stability~mean_cVR, data = stcvrMW)
summary(fitstcvrMW)

## mixed effects model
fitMWstvr10 <- lmer(mean_stability ~ mean_cVR + (1|BLOCK), data = stcvrMW)
summary(fitMWstvr10)
fitMWstnull10 <- lmer(mean_stability~1 + (1|BLOCK), data = stcvrMW)
summary(fitMWstnull10)

## compare AIC values
AIC(fitMWstvr10)
AIC(fitMWstnull10)

## deltaAIC
AIC(fitMWstvr10) - AIC(fitMWstnull10)
## model with VR fits better than null


## C TREATMENT ## 
stcvrC <- stcvrmw10_models %>%
  filter(TREATMENT == "C")
## linear model
fitstcvrC <- lm(mean_stability~mean_cVR, data = stcvrC)
summary(fitstcvrC)

## mixed effects model
fitCstvr10 <- lmer(mean_stability ~ mean_cVR + (1|BLOCK), data = stcvrC)
summary(fitCstvr10)
fitCstnull10 <- lmer(mean_stability~1 + (1|BLOCK), data = stcvrC)
summary(fitCstnull10)

## compare AIC values
AIC(fitCstvr10)
AIC(fitCstnull10)

## deltaAIC
AIC(fitCstvr10) - AIC(fitCstnull10)
## model with VR fits better than null (barely)



## WC TREATMENT ##
stcvrWC <- stcvrmw10_models %>%
  filter(TREATMENT == "WC")
## linear model
fitstcvrWC <- lm(mean_stability~mean_cVR, data = stcvrWC)
summary(fitstcvrWC)

## mixed effects model
fitWCstvr10 <- lmer(mean_stability ~ mean_cVR + (1|BLOCK), data = stcvrWC)
summary(fitWCstvr10)
fitWCstnull10 <- lmer(mean_stability~1 + (1|BLOCK), data = stcvrWC)
summary(fitWCstnull10)

## compare AIC values
AIC(fitWCstvr10)
AIC(fitWCstnull10)

## deltaAIC
AIC(fitWCstvr10) - AIC(fitWCstnull10)
## model with VR fits better than null



## MWC TREATMENT ##
stcvrMWC <- stcvrmw10_models %>%
  filter(TREATMENT == "MWC")
## linear model
fitstcvrMWC <- lm(mean_stability~mean_cVR, data = stcvrMWC)
summary(fitstcvrMWC)

## mixed effects model
fitMWCstvr10 <- lmer(mean_stability ~ mean_cVR + (1|BLOCK), data = stcvrMWC)
summary(fitMWCstvr10)
fitMWCstnull10 <- lmer(mean_stability~1 + (1|BLOCK), data = stcvrMWC)
summary(fitMWCstnull10)

## compare AIC values
AIC(fitMWCstvr10)
AIC(fitMWCstnull10)

## deltaAIC
AIC(fitMWCstvr10) - AIC(fitMWCstnull10)
## model with VR fits better than null
```




## 5 year drought score & stability figs
```{r}

## 5 Years ## 
dscore5 <- dr_score_func(input_data = d_sever_prev, timestep = 5)

## stability
drst5yr <- left_join(dscore5, stabmw5, by = "timestep") %>%
  mutate(stability_diff = mean_stability - lead(mean_stability, default = NA, order_by = timestep),
         magdiff = abs(stability_diff))

ggplot(drst5yr, aes(x=timestep, y=mean_stability, color = Dscore)) +
  geom_point(size=3) +
  geom_line() +
  facet_wrap(~TREATMENT)

ggplot(drst5yr, aes(x=Dscore, y=mean_stability)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drst5yr, aes(x=Dscore, y=stability_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drst5yr, aes(x=Dscore, y=magdiff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")



## 7 Years ## 
dscore7 <- dr_score_func(input_data = d_sever_prev, timestep = 7)

stabmw7 <- stab_mw_tx %>%
  filter(window_size == "7")

## stability
drst7yr <- left_join(dscore7, stabmw7, by = "timestep") %>%
  mutate(stability_diff = mean_stability - lead(mean_stability, default = NA, order_by = timestep),
         magdiff = abs(stability_diff))

ggplot(drst7yr, aes(x=timestep, y=mean_stability, color = Dscore)) +
  geom_point(size=3) +
  geom_line() +
  facet_wrap(~TREATMENT)

ggplot(drst7yr, aes(x=Dscore, y=mean_stability)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drst7yr, aes(x=Dscore, y=stability_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drst7yr, aes(x=Dscore, y=magdiff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")




## variance ratio
drcvr5yr <- left_join(dscore5, cvrmw5, by = "timestep") %>%
  mutate(cVR_diff = mean_cVR - lead(mean_cVR, default = NA, order_by = timestep),
         magdiff = abs(cVR_diff))

ggplot(drcvr5yr, aes(x=timestep, y=mean_cVR, color = Dscore)) +
  geom_point(size=3) +
  geom_line() +
  facet_wrap(~TREATMENT)

ggplot(drcvr5yr, aes(x=Dscore, y=mean_cVR)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drcvr5yr, aes(x=Dscore, y=cVR_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drcvr5yr, aes(x=Dscore, y=magdiff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")




## population stability
drpopst10yr <- left_join(dscore10, spstmw10, by = "timestep") %>%
  mutate(popst_diff = avgpopstab - lead(avgpopstab, default = NA, order_by = timestep),
         magdiff = abs(popst_diff))

ggplot(drpopst10yr, aes(x=timestep, y=avgpopstab, color = Dscore)) +
  geom_point(size=3) +
  geom_line() +
  facet_wrap(~TREATMENT)


ggplot(drpopst10yr, aes(x=timestep, y=avgpopstab, color = Dscore)) +
  geom_point(size=3) +
  geom_line() #+
#facet_wrap(~TREATMENT)



ggplot(drpopst10yr, aes(x=Dscore, y=avgpopstab)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drpopst10yr, aes(x=Dscore, y=popst_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(drpopst10yr, aes(x=Dscore, y=magdiff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

```
### Do drought, herbivore treatment, or biotic mechanisms predict stability?
#### this is too much with all the needed interactions
#### this would be better as SEM style analysis
```{r}
## 10 Years ##
## first, need to combine data into the one dataframe
j <- left_join(drst10yr, drcvr10yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought")) %>%
  select(-SE, -SE_cVR)

jj <- left_join(j, drpopst10yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought")) %>%
  select(-SE_popst)

stabmechmw10 <- left_join(jj, drrich10yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought")) %>%
  select(-SE_rich)

## Model 
fitstall <- lmer(mean_stability ~ Dscore+mega+cows+wildlife+mean_cVR+avgpopstab+mean_rich+(1|BLOCK), data = stabmechmw10, na.action = "na.fail")
summary(fitstall)
dredge(fitstall)


## 5 Years ## 
## first, need to combine data into the one dataframe
j5 <- left_join(drst5yr, drcvr5yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought")) %>%
  select(-SE, -SE_cVR)

jj5 <- left_join(j5, drpopst5yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought")) %>%
  select(-SE_popst)

stabmechmw5 <- left_join(jj5, drrich5yr, by = c("TREATMENT", "timestep", "window_size", "BLOCK", "mega", "cows", "wildlife", "Dscore", "num_drought", "contains_drought")) %>%
  select(-SE_rich)


fitstall5 <- lmer(mean_stability ~ Dscore+mega+cows+wildlife+mean_cVR+avgpopstab+mean_rich+(1|BLOCK), data = stabmechmw5, na.action = "na.fail")
summary(fitstall)
dredge(fitstall)

rm(list = c("j", "jj", "j5", "jj5"))
```