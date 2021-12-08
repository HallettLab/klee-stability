## Structural Equation Modeling ##

## load packages
library(lavaan)
library(lattice)

## read in data
dat <- read.csv("mw_forSEM.csv")

## create model
model <- 'stability ~ classicVR + mean_popst + richness + Dscore + TREATMENT
richness ~ Dscore + TREATMENT
classicVR ~ Dscore + TREATMENT + richness
mean_popst ~ Dscore + TREATMENT'

fit <- sem(model, std.ov=T, missing = "ml", data = dat)
summary(fit, fit.measure=TRUE, rsquare = T)






