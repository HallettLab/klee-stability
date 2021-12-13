### Final Data Cleaning after Calculations

library(tidyverse)

## read in data
source("current_scripts/movingwindow_calcs.R")
source("current_scripts/drought_score_calcs.R")
source("current_scripts/full_timeseries_calcs.R")

## join 10 year moving window data with drought score
dmw10 <- left_join(mw10all, dscore10, by = "timestep")
dmw5 <- left_join(mw5all, dscore5, by = "timestep")


## create cows, wildlife, & mega columns for models
## for 10 year mw
dmw10$cows <- 0
dmw10$cows[dmw10$TREATMENT %in% c('C','WC', 'MWC')] <- 1
dmw10$cows <- factor(dmw10$cows)

dmw10$wildlife <- 0
dmw10$wildlife[dmw10$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
dmw10$wildlife <- factor(dmw10$wildlife)

dmw10$mega <- 0
dmw10$mega[dmw10$TREATMENT %in% c('MW','MWC')] <- 1
dmw10$mega <- factor(dmw10$mega)

## for 5 year mw
dmw5$cows <- 0
dmw5$cows[dmw5$TREATMENT %in% c('C','WC', 'MWC')] <- 1
dmw5$cows <- factor(dmw5$cows)

dmw5$wildlife <- 0
dmw5$wildlife[dmw5$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
dmw5$wildlife <- factor(dmw5$wildlife)

dmw5$mega <- 0
dmw5$mega[dmw5$TREATMENT %in% c('MW','MWC')] <- 1
dmw5$mega <- factor(dmw5$mega)


## Change treatment to factor in all dataframes
dmw5$TREATMENT <- as.factor(dmw5$TREATMENT)
dmw10$TREATMENT <- as.factor(dmw10$TREATMENT)

## Change treatment to an ordered factor
dmw5 <- dmw5 %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))
dmw10 <- dmw10 %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))

## Prep for Plotting Figures
## prep dataframe for figures
## now that block is included, need to take the average of blocks
figdrst <- dmw10 %>%
  group_by(timestep, TREATMENT) %>%
  summarize(Dscore = mean(Dscore), stability = mean(stability))

## Make data frame of means first
figdrst5 <- dmw5 %>%
  group_by(timestep, TREATMENT) %>%
  summarize(Dscore = mean(Dscore), stability = mean(stability))

## prep dataframe for figures
## now that block is included, need to take the average of blocks
figdrcvr <- dmw10 %>%
  group_by(timestep, TREATMENT) %>%
  summarize(Dscore = mean(Dscore), classicVR = mean(classicVR))


## prep dataframe for figures
## now that block is included, need to take the average of blocks
figdrpopst <- dmw10 %>%
  group_by(timestep, TREATMENT) %>%
  summarize(Dscore = mean(Dscore), popstab = mean(mean_popst))


## prep dataframe for figures
## now that block is included, need to take the average of blocks
figdrri <- dmw10 %>%
  group_by(timestep, TREATMENT) %>%
  summarize(Dscore = mean(Dscore), richness = mean(richness))


## combine both VR dataframes into one in order to put all in the same panel
colnames(b5tsVR) <- c("Unique_ID", "TREATMENT", "BLOCK", "classicVR", "longVR", "shortVR")

## add column to differentiate community type
big5tsVR <- b5tsVR %>% 
  mutate(community_type = "Dominant") 
alltsVR <- tsVR %>% 
  mutate(community_type = "All Species")

## join data frames
tsVRalldom <- rbind(big5tsVR, alltsVR)

## set treatment as a factor
tsVRalldom$TREATMENT <- as.factor(tsVRalldom$TREATMENT)

## make one data frame with means calculated for graphing
tsVR_plot <- tsVRalldom %>%
  group_by(TREATMENT, community_type) %>%
  summarise(mean_cVR = mean(classicVR), SE_cVR = calcSE(classicVR), mean_lVR = mean(longVR), SE_lVR = calcSE(longVR), mean_sVR = mean(shortVR), SE_sVR = calcSE(shortVR)) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments


rm(list = c("mw10all", "mw5all"))