setwd("~/Repositories/klee-stability")

source("KLEE_data_cleaning.R")

library(tidyverse)
library(codyn)
library(zoo)
library(tsvr)
library(RColorBrewer)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


################################################################################
### Calculate Stability over a moving window  ##################################
################################################################################

#I'm wondering if a data frame is not the right thing to feed into these functions?
#here I'm just removing unnecessary components from the data frame in case these 
#are confusing the function
#I will eventually move some of this to the data cleaning script.



#the stability function that I want to apply over the moving window
community_stability(klee.ts, 
                    time.var = "Date_numeric", 
                    abundance.var = "Abundance", 
                    replicate.var = "Unique_ID")


## my attempts to apply this over a moving window
mw_stability <- rollapply(klee.ts, width = 5, FUN = community_stability, klee.ts,
                                          time.var = "Date_numeric", 
                                          abundance.var = "Abundance", 
                                          replicate.var = "Unique_ID")




mw_stability <- rollapply(klee.ts, width = 20, FUN = function(x) {community_stability(klee.ts,
                                                                                   time.var = "Date_numeric",
                                                                                   abundance.var = "Abundance",
                                                                                   replicate.var = "Unique_ID")})

mw_stability <- as.data.frame(mw_stability)





################################################################################
### Explore calculated total cover over time ###################################
################################################################################

##Calculate Total Cover 1999-2015 

#group data by block, treatment, and date before summing species abundances to get total cover
klee_totcov <- klee_long %>%
  group_by(BLOCK, TREATMENT, Date_final) %>% #group by block, treatment, and date to get total cov at each unique location & time
  summarise(totcov = sum(Pin_Hits, na.rm=T)) #sum abundances to calculate tot cover


## Get data ready to graph as a time series
meantotcov <- klee_totcov %>%
  group_by(TREATMENT, Date_final) %>% #group by treatment & date
  summarize(Mean_TotCov = mean(totcov)) #calculate the mean total cover at each treatment & date

## Graph! 

## FINAL GRAPH #################################################################
#graph the time series - all treatments in one plot
ggplot(meantotcov, aes(x=Date_final, y=Mean_TotCov, col=TREATMENT)) +
  geom_line(size = 0.5) +
  geom_point(aes(y=Mean_TotCov)) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c("1")) +
  theme_classic() +
  ylab("Total Cover") + xlab("Date")
  

ggsave("totalcov_calc_by_year.pdf", width = 10, height = 6)
ggsave("totalcov_calc_by_year.png", width = 10, height = 6)


## OTHER VISUALIZATIONS
#separate by treatment to more easily view overlapping lines
ggplot(meantotcov, aes(x=Date_final)) +
  geom_line(aes(y=Mean_TotCov, col=TREATMENT)) +
  theme_bw() +
  ylab("Mean Total Cover") + xlab("Date") +
  facet_wrap(~TREATMENT)

ggsave("totalcov_calc_sep_lines.pdf", width = 10, height = 6)

#graph as scatterplot
ggplot(meantotcov, aes(x=Date_final, y=Mean_TotCov, col=TREATMENT)) + 
  geom_point() +
  facet_wrap(~TREATMENT) +
  ylab("Mean Total Cover") + xlab("Date") +
  theme_bw()

ggsave("totalcov_calc_scatterplot.pdf", width = 10, height = 6)


################################################################################
#### Explore Existing Total Cover Data #########################################
################################################################################

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


################################################################################
############# Compare Existing & Calculated Total Cov Data #####################
################################################################################

names(meantotcov) <- c("Treatment",   "Date_final",  "MeanCov_Calculated")

# combine two total cover data frames in order to graph & compare whether they are the same
totcov_comparison <- left_join(meancov, meantotcov, by = c("Treatment", "Date_final"))


ggplot(totcov_comparison, aes(x=MeanCov_Calculated, y=Mean_Cov)) +
  geom_point() +
  ylab("Mean Cover Existing") + xlab("Mean Cover Calculated") +
  theme_bw()
#data match up very well besides 6 values... uncertain what these are or mean...

ggsave("Calc_Existing_cover_comparison.pdf", height = 10, width = 10)



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

## calculate community stability metric for each plot
commstab <- community_stability(klee_long, 
                                time.var = "Date_numeric", 
                                abundance.var = "Pin_Hits", 
                                replicate.var = "Unique_ID")


stability <- left_join(commstab, treats, by = "Unique_ID") %>% #join community stability with treatment info
  mutate(CV = 1/stability) #calculate the CV by taking the inverse of stability

## graph stability with treatment info (boxplot)
ggplot(stability, aes(x=TREATMENT, y=stability)) +
  geom_boxplot() +
  ylab("Stability of Total Cover") + xlab("Herbivore Treatment") +
  theme_bw()


## graph CV with treatment info (boxplot)
ggplot(stability, aes(x=TREATMENT, y=CV)) +
  geom_boxplot() +
  ylab("CV") + xlab("Herbivore Treatment") +
  theme_bw()

## create a dataframe with mean & standard error of stability by treatment
mean_stability <- stability %>%
  group_by(TREATMENT) %>%
  summarize(mean_st = mean(stability), SEst = calcSE(stability), mean_cv = mean(CV), SECV = calcSE(CV))

## visualize mean stability & standard error
ggplot(mean_stability, aes(x=TREATMENT, y=mean_st)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_st-SEst, ymax=mean_st+SEst), width = 0.2) +
  ylab("Stability") + xlab("Treatment") +
  theme_classic()

### QUESTION HERE ##############################################################
### NEED TO CALCULATE HERBIVORE GUILD & GRAZING INTENSITY VARIABLES SOMEHOW?
##calculate # herbivore guilds
herb <- stabilitycv %>%
  mutate(ifelse())



################################################################################
### Compensatory Dynamics ######################################################
################################################################################

#variance_ratio from codyn package (Hallett et al. 2014)
#takes a reeeeeeally long time to calculate! Save results to data file perhaps?
#vr <- variance_ratio(
 # klee_long,
#  time.var = "Date_numeric",
#  species.var = "SPECIES",
#  abundance.var = "Pin_Hits",
#  bootnumber = 10000, #used this as large bootnumber was recommended
#  replicate.var = "Unique_ID",
#  average.replicates = FALSE,
#  level = 0.95,
#)

#save as a .csv file and read in. Don't want to have to recalculate this unless
#more cleaning is needed.
#write.csv(vr, "klee-vr.csv", row.names = FALSE)

vr_klee <- read.csv("klee-vr.csv")

## Visualize classic VR 
#calculate mean & sd of VR values
vr_mean <- inner_join(vr_klee, treats, by = "Unique_ID") %>%
  group_by(TREATMENT) %>%
  summarise(meanVR = mean(VR), SEVR = calcSE(VR))

#plot VR by individual plot
ggplot(vr_klee, aes(x=Unique_ID, y=VR)) +
  geom_point() +
  theme_classic()

#plot VR means
ggplot(vr_mean, aes(x=TREATMENT, y=meanVR)) +
  geom_point() +
  theme_classic() +
  geom_errorbar(aes(ymin = meanVR-SEVR, ymax = meanVR+SEVR), width = 0.3) +
  ylab("Variance Ratio (Classic)") + xlab("") +
  geom_hline(yintercept = 1, color = "grey", lty = "dashed")


#Loreau synchrony metric
loreau_synchrony <- synchrony(
  klee_long,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  metric = "Loreau",
  replicate.var = "Unique_ID"
)

colnames(loreau_synchrony) <- c("Unique_ID", "loreau_synchrony") #rename columns

#Gross synchrony metric
gross_synchrony <- synchrony(
  klee_long,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  replicate.var = "Unique_ID",
  metric="Gross")

colnames(gross_synchrony) <- c("Unique_ID", "gross_synchrony") #rename columns

synchrony <- left_join(loreau_synchrony, gross_synchrony, by = "Unique_ID") #join two synchrony metrics into one data frame

#create one data frame of all 3 synchrony metrics from codyn package
temp <- left_join(vr_klee, synchrony, by = "Unique_ID")

#calculate mean & SE of metrics to get ready for graphing
klee_synchrony <- left_join(temp, treats, by = "Unique_ID") %>%
  pivot_longer(cols = VR:gross_synchrony, names_to = "Metric_Name", values_to = "VR_Synchrony") %>%
  group_by(TREATMENT, Metric_Name) %>%
  summarise(mean_value = mean(VR_Synchrony), SE_value = calcSE(VR_Synchrony))


### Graph Synchrony Metric by Treatment ########################################
#graph all three metrics together
ggplot(klee_synchrony, aes(x=TREATMENT, y=mean_value, col = Metric_Name)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_value-SE_value, ymax = mean_value+SE_value), width= 0.075) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Mean Value") + xlab("Treatment") +
  theme_bw()

ggsave("synchrony_metrics_grazing.pdf", height = 10, width = 10)

## calculate timescale specific VR #############################################

#set up data frame to put tsvr into
outnames <- c("Unique_ID", "TREATMENT", "classicVR", "longVR", "shortVR")
siteout <- as.data.frame(matrix(nrow=0, ncol = 5)) 
names(siteout) <- outnames
plots <- unique(klee_long$Unique_ID)


for (i in 1:length(plots)) {
  
  #subset by replicate (gives all observations from one plot over time)
  plot <- subset(klee_long, Unique_ID == plots[i]) %>%
    tbl_df()
  
  #select species and fill 0's 
  plot2 <- plot %>%
    select(Date_numeric, SPECIES, Pin_Hits) %>% #selecting only these three columns
    spread(SPECIES, Pin_Hits, fill = 0) #changing to wide format
  
  #transpose the data 
  dat <- t(as.matrix(plot2[,2:dim(plot2)[2]]))
  
  #create a dataframe with replicate info to attach VR metrics to
  VR_plots <- plot %>%
    select(Unique_ID, TREATMENT, BLOCK) %>%
    unique()
  
  #calculate tsvr
  res0 <- vreq_classic(dat)
  VR_plots$classicVR <- res0[[3]]
  
  #attach classic VR?
  res <- tsvreq_classic(dat)
  
  #aggregate short vs. long
  resLong <- aggts(res, res$ts[res$ts>=4]) #grabbing tsvr with time period >= 4 years
  resShort <- aggts(res, res$ts[res$ts<4]) #grabbing tsvr with time period <4 years
  
  #attach short & long
  VR_plots$longVR <- resLong[[3]]
  VR_plots$shortVR <- resShort[[3]]
  
  #append to external dataframe
  siteout<-rbind(siteout, VR_plots)
}
#rename 
tsVR <- siteout

## calculate the mean and sd of tsvr
meantsVR <- tsVR %>%
  pivot_longer(cols = classicVR:shortVR, names_to = "VR_type", values_to = "VR_value" ) %>% 
  group_by(TREATMENT, VR_type) %>%
  summarise(meanVR = mean(VR_value), SEVR = calcSE(VR_value))


## Visualize time specific variance ratio
#VR by VR type, faceted by treatment
ggplot(meantsVR, aes(x=VR_type, y=meanVR, col = TREATMENT)) +
  geom_hline(yintercept = 1, color = "grey", lty = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanVR-SEVR, ymax = meanVR+SEVR), width = 0.3) +
  theme_classic() +
  facet_wrap(~TREATMENT) +
  ylab("Variance Ratio") + xlab("")

#variance ratio vs. VR type, grouped by treatment
ggplot(meantsVR, aes(x=VR_type, y=meanVR, col = TREATMENT)) +
  geom_hline(yintercept = 1, color = "grey", lty = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanVR-SEVR, ymax = meanVR+SEVR), width = 0.1) +
  theme_classic() +
  ylab("Variance Ratio") + xlab("") 

## Best way to visualize this, so far. 
#variance ratio vs. treatment, grouped by VR type
ggplot(meantsVR, aes(x=TREATMENT, y=meanVR, col = VR_type)) +
  geom_hline(yintercept = 1, color = "grey", lty = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanVR-SEVR, ymax = meanVR+SEVR), width = 0.1) +
  theme_classic() +
  #facet_wrap(~TREATMENT) +
  ylab("Variance Ratio") + xlab("Treatment")

ggsave("timespecific_VR.pdf", width = 6, height = 4)



### Compare Synchrony & Stability ##############################################

#join stability & synchrony data frames. Not a clean, perfect data frame, will work for initial visualizations
tsvr_stab <- inner_join(mean_stability, meantsVR, by = "TREATMENT") 


ggplot(tsvr_stab, aes(x=meanVR, y=mean_st, col = TREATMENT)) +
  geom_point() +
  facet_wrap(~VR_type) +
  ylab("Stability") + xlab("Variance Ratio") +
  geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst), width = 0.2) +
  geom_errorbarh(aes(xmin = meanVR-SEVR, xmax=meanVR+SEVR)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

ggsave("stability_synchrony.pdf", width = 15, height = 5)


################################################################################
### Stable Dominance ###########################################################
################################################################################

## rank each species by abundance
## calculate rank abundance curve difference & change

RAC_timestep <- RAC_change(df = klee_long,
           species.var = "SPECIES",
           abundance.var = "Pin_Hits",
           replicate.var = "Unique_ID",
           time.var = "Date_numeric",
           )

RAC_reference <- RAC_change(df = klee_long,
                  species.var = "SPECIES",
                  abundance.var = "Pin_Hits",
                  replicate.var = "Unique_ID",
                  time.var = "Date_numeric",
                  reference.time = 19990101 
)



#join evenness info with treatment data. calculate mean change in evenness
RAC_treats <- inner_join(RAC_reference, treats, by = "Unique_ID") %>%
  mutate(Date = ymd(Date_numeric2)) %>%
  group_by(Date, TREATMENT) %>%
  summarise(mean_even = mean(evenness_change), evenSE = calcSE(evenness_change))

#unsure how to visualize this. perhaps as a time series that shows changes?
##### QUESTION HERE ############################################################
## would it make sense to put a trend line over this? It looks like although there 
## is variation, some of these plots are trending in a positive direction 
## particularly MW, MWC, W, and WC

#Change in evenness compared to 1999
ggplot(RAC_treats, aes(x=Date, y=mean_even, col= TREATMENT)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  ylab("Change in Evenness from 1999") + xlab("") +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~TREATMENT) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_even-evenSE, ymax=mean_even+evenSE))


ggsave("evenness_1999reference.pdf", width = 10, height = 6)
ggsave("evenness_1999reference.png", width = 10, height = 6)

## graph the time steps
RAC_time <- inner_join(RAC_timestep, treats, by = "Unique_ID") %>%
  mutate(Date = ymd(Date_numeric2)) %>%
  group_by(Date, TREATMENT) %>%
  summarise(mean_even = mean(evenness_change), evenSE = calcSE(evenness_change))

## graph year to year changes in richness
ggplot(RAC_time, aes(x=Date, y=mean_even, col= TREATMENT)) +
  #geom_line() +
  geom_point() +
  theme_classic() +
  ylab("Change in Evenness Year to Year") + xlab("") +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  #geom_smooth(method = "lm") +
  facet_wrap(~TREATMENT) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_even-evenSE, ymax=mean_even+evenSE))

#try out RAC difference function
RACdiff <- RAC_difference()

################################################################################
### RICHNESS ###################################################################
################################################################################

## Species richness compared to reference
  ## calculated above with RAC_change functions

## calculate mean richness change from 1999
RAC_sr <- inner_join(RAC_reference, treats, by = "Unique_ID") %>%
  mutate(Date = ymd(Date_numeric2)) %>%
  group_by(Date, TREATMENT) %>%
  summarise(mean_SR = mean(richness_change), SRSE = calcSE(richness_change))

## graph changes in richness from 1999
ggplot(RAC_sr, aes(x=Date, y=mean_SR, col= TREATMENT)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  ylab("Change in Richness from 1999") + xlab("") +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~TREATMENT) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_SR-SRSE, ymax=mean_SR+SRSE))


## calculate mean annual changes in richness
RAC_sr_time <- inner_join(RAC_timestep, treats, by = "Unique_ID") %>%
  mutate(Date = ymd(Date_numeric2)) %>%
  group_by(Date, TREATMENT) %>%
  summarise(mean_SR = mean(richness_change), SRSE = calcSE(richness_change))

## graph annual changes in richness
ggplot(RAC_sr_time, aes(x=Date, y=mean_SR, col= TREATMENT)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  ylab("Change in Richness Year to Year") + xlab("") +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~TREATMENT) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_SR-SRSE, ymax=mean_SR+SRSE))




## calculate total species richness across the time series

#may need to eventually use presence-absence data (accounts for a lot more rare species)
totalrich <- klee_long %>%
  group_by(Unique_ID) %>%
  #filter(Pin_Hits > 0) %>%
  unique() 
  summarise(totalrich = n())


## number of unique species in a plot over time

## calculate some sort of annual diversity metric -> see codyn package for ideas
