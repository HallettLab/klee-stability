setwd("~/Repositories/klee-stability")

source("KLEE_data_cleaning.R")
library(tidyverse)
library(codyn)


##########################################################
#### Explore Existing Total Cover Data ###################
##########################################################

## Get data ready to graph as a time series
meancov <- tcov_long %>%
  group_by(Treatment, Date_final) %>% #group by treatment & date
  summarize(Mean_Cov = mean(Tot_Cover)) #calculate the mean total cover at each treatment & date


#change variable formats to get this to work in geom_line
meancov$Treatment <- as.character(meancov$Treatment)
#meancov$Date <- as.numeric(meancov$Date)

## Graph! 
#graph the time series - all treatments in one plot
ggplot(meancov, aes(x=Date_final)) +
  geom_line(aes(y=Mean_Cov, group=Treatment, col=Treatment)) +
  theme_bw() +
  ylab("Total Cover") + xlab("Date (yyyymm)")
 
#separate by treatment to more easily view overlapping lines
ggplot(meancov, aes(x=Date_final)) +
  geom_line(aes(y=Mean_Cov, col=Treatment)) +
  theme_bw() +
  ylab("Total Cover") + xlab("Date (yyyymm)") +
  facet_wrap(~Treatment)

#graph as scatterplot
ggplot(meancov, aes(x=Date_final, y=Mean_Cov, col=Treatment)) + 
  geom_point() +
  facet_wrap(~Treatment) +
  theme_bw()


#################################################################
### Explore calculated total cover over time
#################################################################

## Get data ready to graph as a time series
meantotcov <- klee_totcov %>%
  group_by(TREATMENT, Date_final) %>% #group by treatment & date
  summarize(Mean_TotCov = mean(totcov)) #calculate the mean total cover at each treatment & date


#change variable formats to get this to work in geom_line
meancov$Treatment <- as.character(meancov$Treatment)
#meancov$Date <- as.numeric(meancov$Date)

## Graph! 
#graph the time series - all treatments in one plot
ggplot(meantotcov, aes(x=Date_final)) +
  geom_line(aes(y=Mean_TotCov, col=TREATMENT)) +
  theme_bw() +
  ylab("Total Cover") + xlab("Date")

ggsave("totalcov_calc_by_year.pdf")

#separate by treatment to more easily view overlapping lines
ggplot(meantotcov, aes(x=Date_final)) +
  geom_line(aes(y=Mean_TotCov, col=TREATMENT)) +
  theme_bw() +
  ylab("Mean Total Cover") + xlab("Date") +
  facet_wrap(~TREATMENT)

ggsave("totalcov_calc_sep_lines.pdf")

#graph as scatterplot
ggplot(meantotcov, aes(x=Date_final, y=Mean_TotCov, col=TREATMENT)) + 
  geom_point() +
  facet_wrap(~TREATMENT) +
  ylab("Mean Total Cover") + xlab("Date") +
  theme_bw()

ggsave("totalcov_calc_scatterplot.pdf")

################################################################################
############# Compare Existing & Calculated Total Cov Data #####################
################################################################################


names(meantotcov) <- c("Treatment",   "Date_final",  "MeanCov_Calculated")

# combine two total cover data frames in order to graph & compare whether they are the same
totcov_comparison <- left_join(meancov, meantotcov, by = c("Treatment", "Date_final"))


ggplot(totcov_comparison, aes(x=MeanCov_Calculated, y=Mean_Cov)) +
  geom_point()
#data match up very well besides 6 values... uncertain what these are or mean...



################################################################################
#### Explore Proportional Cover Data ###########################################
################################################################################

## Get data ready to graph as a time series
meancov <- propcov_long %>%
  group_by(Treatment, Date) %>% #group by treatment & date
  summarize(Mean_Cov = mean(Prop_Cover, na.rm = T)) #calculate the mean total cover at each treatment & date


#change variable formats to get this to work in geom_line
meancov$Treatment <- as.character(meancov$Treatment)
meancov$Date <- as.numeric(meancov$Date)


## Graph! 
#graph the time series - all treatments in one plot
ggplot(meancov, aes(x=Date)) +
  geom_line(aes(y=Mean_Cov, col=Treatment)) +
  theme_bw() +
  ylab("Proportional Cover") + xlab("Date (yyyymm)")

#separate by treatment to more easily view overlapping lines
ggplot(meancov, aes(x=Date)) +
  geom_line(aes(y=Mean_Cov, col=Treatment)) +
  theme_bw() +
  ylab("Proportional Cover") + xlab("Date (yyyymm)") +
  facet_wrap(~Treatment)

#graph as scatterplot
ggplot(meancov, aes(x=Date, y=Mean_Cov, col=Treatment)) + 
  geom_point() +
  facet_wrap(~Treatment) +
  ylab("Mean Proportional Cover") +
  theme_bw()




################################################################################
#### Calculating Stability Metrics #############################################
################################################################################

## calculate standard error for each plot in each year? could be useful
## calculate temporal standard deviation


##create generic treatment data frame to combine with metrics
treats <- klee_long %>%
  #select(BLOCK, TREATMENT, Unique_ID, Abundance) %>%
  group_by(Unique_ID, BLOCK, TREATMENT) %>%
  summarise(meanabund = mean(Abundance, na.rm = T))


## calculate community stability metric for each plot
commstab <- community_stability(klee_long, 
                                time.var = "Date_numeric", 
                                abundance.var = "Abundance", 
                                replicate.var = "Unique_ID")

## join community stability with treatment info
stability <- left_join(commstab, treats[c("Unique_ID", "BLOCK", "TREATMENT")], by = "Unique_ID")


## calculate CV for each plot by taking the inverse of stability (should be sd/mean)
stabilitycv <- stability %>%
  mutate(CV = 1/(stability))


## graph stability with treatment info (boxplot)
ggplot(stabilitycv, aes(x=TREATMENT, y=stability)) +
  geom_boxplot() +
  ylab("Stability of Total Cover") + xlab("Herbivore Treatment") +
  theme_bw()


## graph CV with treatment info (boxplot)
ggplot(stabilitycv, aes(x=TREATMENT, y=CV)) +
  geom_boxplot() +
  ylab("CV") + xlab("Herbivore Treatment") +
  theme_bw()

### NEED TO CALCULATE HERBIVORE GUILD & GRAZING INTENSITY VARIABLES SOMEHOW?
##calculate # herbivore guilds
herb <- stabilitycv %>%
  mutate(ifelse())



################################################################################
### RICHNESS ###################################################################
################################################################################

## calculate richness for each plot (total species richness across the time series)
#may need to eventually use presence-absence data (accounts for a lot more rare species)
#Still has bare ground observations in this.
totalrich <- klee_long %>%
  group_by(Unique_ID, TREATMENT) %>%
  filter(Abundance > 0) %>%
  mutate(totalrich = n(unique(klee_long$SPECIES)))

kleesp <- n(unique(klee_long$SPECIES))

count(unique(klee_long$SPECIES))

## number of unique species in a plot over time

## calculate some sort of annual diversity metric -> see codyn package for ideas


### Compensatory Dynamics ######################################################
## calculate VR & synchrony metrics
## calculate timescale specific VR



### Stable Dominance ###########################################################
## rank each species by abundance
## calculate rank abundance curve difference & change