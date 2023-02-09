### Verify Portfolio Effect
source("klee_allyears_cleaning.R")

## group by unique ID and species to calculate the mean and variance of each species
portfolio_effect <- klee_annual %>%
  filter(!is.na(SPECIES), !is.na(Pin_Hits)) %>% #filter out any NA values
  filter(Pin_Hits > 0) %>% #filter out abundances of 0
  group_by(Unique_ID, SPECIES, TREATMENT) %>% #group by unique_ID & species
  summarise(variance = var(Pin_Hits, na.rm = TRUE), meanhits = mean(Pin_Hits)) %>% #calculate mean & variance
  mutate(logvar = log(variance), logmean = log(meanhits)) #log transform mean and variance

## remove log values less than 0
#linear model doesn't seem to work with NA and 0 values
portfolio <- portfolio_effect %>%
  filter(logvar > 0, logmean > 0)

#run a linear model to calculate the slope
taylor <- lm(logvar~ logmean, data = portfolio)
summary(taylor)
plot(taylor)
AIC(taylor)
#check that slope is between 1-2
## slope is 1.90052 

## check model fits as relationship appears slightly sublinear.
model <- lm(logvar ~ poly(logmean,3), data = portfolio)
summary(model)

plot(model)
AIC(model)
AIC(model)- AIC(taylor)

## plot log variance and log mean
ggplot(portfolio, aes(x=logmean, y= logvar)) +
  geom_point() +
  geom_smooth(method = "lm")+
  geom_abline(slope = 1, intercept = 0) +
  theme_bw()
