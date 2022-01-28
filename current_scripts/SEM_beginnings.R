## Structural Equation Modeling 
## Fit piecewise model with random effects

source("finalprep_postcalcs.R")

library(tidyverse)
library(piecewiseSEM)
library(multcompView)
library(lme4)
library(GGally)
library(car)

## Check for correlation; VIF; and multicolinearity
## with all data
ggpairs(dmw10,
        columns = 7:9,
        upper = list(continuous = "cor"))
ggsave("corrmatrixall.png", height = 6, width = 8)
## then with cows model
ggpairs(dmw10,
        columns = 7:9,
        upper = list(continuous = "cor"),
        mapping = aes(color = cows), 
        title = "Cattle")
#ggsave("corrmatrixcows.png", height = 6, width = 8)

## then with mesoherbivores
ggpairs(dmw10,
        columns = 7:9,
        upper = list(continuous = "cor"),
        mapping = aes(color = wildlife), 
        title = "Wild Mesoherbivores")
#ggsave("corrmatrixmeso.png", height = 6, width = 8)

## then with megaherbivores
ggpairs(dmw10,
        columns = 7:9,
        upper = list(continuous = "cor"),
        mapping = aes(color = mega), 
        title = "Megaherbivores")
#ggsave("corrmatrixmega.png", height = 6, width = 8)

## check correlations
cor(dmw10$classicVR, dmw10$mean_popst)
cor(dmw10$richness, dmw10$mean_popst)
cor(dmw10$richness, dmw10$classicVR)



## change herbivore columns to numeric. It is currently coded as a factor; have to change
## to character first then numeric otherwise it changes the 0 & 1 to 1 & 2
dmw10$cows <- as.character(dmw10$cows)
dmw10$cows <- as.numeric(dmw10$cows)
dmw10$mega <- as.character(dmw10$mega)
dmw10$mega <- as.numeric(dmw10$mega)
dmw10$wildlife <- as.character(dmw10$wildlife)
dmw10$wildlife <- as.numeric(dmw10$wildlife)


## Final SEM Models: 

## Cows: with richness -> synchrony pathway
cows_psem_w <- psem(
  
  lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+cows+richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(cows_psem_w)

vif(lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10))
vif(lmer(classicVR~Dscore+cows+richness + (1|BLOCK), data = dmw10))
vif(lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10))
vif(lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10))


## Mega: with richness -> synchrony pathway
mega_psem_w <- psem(
  
  lm(stability~Dscore+mega + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+mega+richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+mega + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+mega + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(mega_psem_w)

vif(lm(stability~Dscore+mega + classicVR + mean_popst + richness , data=dmw10))
vif(lmer(classicVR~Dscore+mega+richness + (1|BLOCK), data = dmw10))
vif(lmer(mean_popst~Dscore+mega + (1|BLOCK), data = dmw10))
vif(lmer(richness~Dscore+mega + (1|BLOCK), data = dmw10))



## Meso: with richness -> synchrony pathway
meso_psem_w <- psem(
  
  lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+wildlife+richness + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+wildlife + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+wildlife + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)
summary(meso_psem_w)

vif(lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=dmw10))
vif(lmer(classicVR~Dscore+wildlife+richness + (1|BLOCK), data = dmw10))
vif(lmer(mean_popst~Dscore+wildlife + (1|BLOCK), data = dmw10))
vif(lmer(richness~Dscore+wildlife + (1|BLOCK), data = dmw10))


















## OLD SEM MODELS ##
## model comparing cows to no cows
cows_psem <- psem(
  
  lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+cows + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(cows_psem)

## check vif
vif(lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=dmw10))
vif(lmer(classicVR~Dscore+cows + (1|BLOCK), data = dmw10))
vif(lmer(mean_popst~Dscore+cows + (1|BLOCK), data = dmw10))
vif(lmer(richness~Dscore+cows + (1|BLOCK), data = dmw10))






## model comparing mega to no mega
mega_psem <- psem(
  
  lm(stability~Dscore+mega + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+mega + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+mega + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+mega + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(mega_psem)

vif(lm(stability~Dscore+mega + classicVR + mean_popst + richness , data=dmw10))




## model comparing wildlife to no wildlife (meso)
meso_psem <- psem(
  
  lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+wildlife + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+wildlife + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+wildlife + (1|BLOCK), data = dmw10),
  
  data = dmw10
  
)

summary(meso_psem)

vif(lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=dmw10))



#### OLD #####

## model with Treatment as a factor
stability_psem <- psem(
  
  lm(stability~Dscore+TREATMENT + classicVR + mean_popst + richness , data=dmw10),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+TREATMENT + (1|BLOCK), data = dmw10),
  
  lmer(mean_popst~Dscore+TREATMENT + (1|BLOCK), data = dmw10),
  
  lmer(richness~Dscore+TREATMENT + (1|BLOCK), data = dmw10),
  
  classicVR %~~% mean_popst, ## include correlated error term
  
  data = dmw10
  
)

summary(stability_psem)



## Need to separate herbivore treatments
MWC.O <- dmw10 %>%
  filter(TREATMENT == "MWC" | TREATMENT == "O")

MWC.O.psem <- psem(
  
  lm(stability~Dscore+mega + classicVR + mean_popst + richness , data=MWC.O),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+mega + (1|BLOCK), data = MWC.O),
  
  lmer(mean_popst~Dscore+mega + (1|BLOCK), data = MWC.O),
  
  lmer(richness~Dscore+mega + (1|BLOCK), data = MWC.O),
  
  #classicVR %~~% richness, ## include correlated error term
  
  data = MWC.O
  
)

summary(MWC.O.psem)



WC.O <- dmw10 %>%
  filter(TREATMENT == "WC" | TREATMENT == "O")

WC.O.psem <- psem(
  
  lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=WC.O),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+wildlife + (1|BLOCK), data = WC.O),
  
  lmer(mean_popst~Dscore+wildlife + (1|BLOCK), data = WC.O),
  
  lmer(richness~Dscore+wildlife + (1|BLOCK), data = WC.O),
  
  #classicVR %~~% richness, ## include correlated error term
  
  data = WC.O
  
)

summary(WC.O.psem)



W.O <- dmw10 %>%
  filter(TREATMENT == "W" | TREATMENT == "O")

W.O.psem <- psem(
  
  lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=W.O),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+wildlife + (1|BLOCK), data = W.O),
  
  lmer(mean_popst~Dscore+wildlife + (1|BLOCK), data = W.O),
  
  lmer(richness~Dscore+wildlife + (1|BLOCK), data = W.O),
  
  #classicVR %~~% richness, ## include correlated error term
  
  data = W.O
  
)

summary(W.O.psem)



MW.O <- dmw10 %>%
  filter(TREATMENT == "MW" | TREATMENT == "O")

MW.O.psem <- psem(
  
  lm(stability~Dscore+wildlife + classicVR + mean_popst + richness , data=MW.O),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+wildlife + (1|BLOCK), data = MW.O),
  
  lmer(mean_popst~Dscore+wildlife + (1|BLOCK), data = MW.O),
  
  lmer(richness~Dscore+wildlife + (1|BLOCK), data = MW.O),
  
  #classicVR %~~% richness, ## include correlated error term
  
  data = MW.O
  
)

summary(MW.O.psem)



C.O <- dmw10 %>%
  filter(TREATMENT == "C" | TREATMENT == "O")

C.O.psem <- psem(
  
  lm(stability~Dscore+cows + classicVR + mean_popst + richness , data=C.O),
  ## removed block as a random effect from this model as it had a variance of 0
  ## it was raising issues about singularity when it stayed in.
  
  lmer(classicVR~Dscore+cows + (1|BLOCK), data = C.O),
  
  lmer(mean_popst~Dscore+cows + (1|BLOCK), data = C.O),
  
  lmer(richness~Dscore+cows + (1|BLOCK), data = C.O),
  
  #classicVR %~~% richness, ## include correlated error term
  
  data = C.O
  
)

summary(C.O.psem)
