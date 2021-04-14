setwd("~/Repositories/klee-stability")

source("KLEE_data_cleaning.R")

library(tidyverse)
library(codyn)
library(zoo)
library(tsvr)
library(RColorBrewer)
library(forcats)
library(ggrepel)

calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


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
  summarize(meancov = mean(totcov), SEcov = calcSE(totcov)) #calculate the mean total cover at each treatment & date

## Graph! 

## FINAL GRAPH #################################################################
#graph the time series - all treatments in one plot
ggplot(meantotcov, aes(x=Date_final, y=meancov, col=TREATMENT)) +
  geom_line(size = 0.5) +
  geom_point(aes(y=meancov)) +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c("1")) +
  theme_classic() +
  ylab("Total Cover") + xlab("Date")
  

ggsave("totalcov_calc_by_year.pdf", width = 10, height = 6)
ggsave("totalcov_calc_by_year.png", width = 10, height = 6)


## OTHER VISUALIZATIONS
#separate by treatment to more easily view overlapping lines
ggplot(meantotcov, aes(x=Date_final)) +
  geom_line(aes(y=meancov, col=TREATMENT)) +
  theme_bw() +
  ylab("Mean Total Cover") + xlab("Date") +
  facet_wrap(~TREATMENT)

ggsave("totalcov_calc_sep_lines.pdf", width = 10, height = 6)

#graph as scatterplot
ggplot(meantotcov, aes(x=Date_final, y=meancov, col=TREATMENT)) + 
  geom_point() +
  facet_wrap(~TREATMENT) +
  ylab("Mean Total Cover") + xlab("Date") +
  theme_bw()

ggsave("totalcov_calc_scatterplot.pdf", width = 10, height = 6)



################################################################################
### Calculate Stability over a moving window  ##################################
################################################################################

#I'm wondering if a data frame is not the right thing to feed into these functions?
#here I'm just removing unnecessary components from the data frame in case these 
#are confusing the function
#I will eventually move some of this to the data cleaning script.

uni <- unique(klee_long$Unique_ID) #make vector of unique plots


for (i in 1:length(uni)) {
  
  
  
  mean()
  
}

#mean/sd
mean(klee_long$SPECIES)

#the stability function that I want to apply over the moving window
s <- community_stability(klee_long, 
                         time.var = "Date_numeric", 
                         abundance.var = "Pin_Hits", 
                         replicate.var = "Unique_ID")


## my attempts to apply this over a moving window
mw_stability <- rollapply(klee_long, width = 10, FUN = community_stability,
                          time.var = "Date_numeric", 
                          abundance.var = "Pin_Hits", 
                          replicate.var = "Unique_ID")




mw_stability <- rollapply(klee_long, width = 10, FUN = function(x) {community_stability(x,
                                                                                        time.var = "Date_numeric",
                                                                                        abundance.var = "Pin_Hits",
                                                                                        replicate.var = "Unique_ID")})

mw_stability <- as.data.frame(mw_stability)




################################################################################
### Explore standing biomass & grazing pressure ################################
################################################################################

#total biomass by grazing treatment.
avg_biomass <- meantotcov %>%
  group_by(TREATMENT) %>%
  summarize(avgbio = mean(meancov), SEbio = calcSE(meancov))

avg_biomass$TREATMENT <- as.factor(avg_biomass$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
avg_biomass$TREATMENT <- factor(avg_biomass$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))

png("./figures/totcover_by_grazing.png", height = 550, width = 800)

ggplot(avg_biomass, aes(x=TREATMENT, y=avgbio)) +
  geom_point() +
  theme_bw() +
  geom_errorbar(aes(ymin=avgbio-SEbio, ymax=avgbio+SEbio), width = 0.2) +
  ylab("Total Pin Hits (Biomass Proxy)") + xlab("Treatment")

ggsave("./figures/totcover_by_grazing.png", height = 4, width = 6)

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

mean_stability$TREATMENT <- as.factor(mean_stability$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
mean_stability$TREATMENT <- factor(mean_stability$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))


################################################################################
#### STABILITY VS. GRAZING PRESSURE. FINAL GRAPH ###############################
## visualize mean stability & standard error
ggplot(mean_stability, aes(x=TREATMENT, y=mean_st)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_st-SEst, ymax=mean_st+SEst), width = 0.2) +
  ylab("Stability (mean/sd)") + xlab("") +
  theme_classic()

ggsave("stability_by_treat.pdf", width = 6, height = 4)
ggsave("stability_by_treat.png", width = 6, height = 4)
################################################################################


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
###NOT SURE VR SHOULD BE JOINED W/SYNCHRONY METRICS.
#temp <- left_join(vr_klee, synchrony, by = "Unique_ID")

#calculate mean & SE of metrics to get ready for graphing
mean_synchrony <- left_join(synchrony, treats, by = "Unique_ID") %>%
  pivot_longer(cols = loreau_synchrony:gross_synchrony, names_to = "Metric_Name", values_to = "Synchrony") %>%
  group_by(TREATMENT, Metric_Name) %>%
  summarise(mean_value = mean(Synchrony), SE_value = calcSE(Synchrony))



mean_synchrony$TREATMENT <- as.factor(mean_synchrony$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
mean_synchrony$TREATMENT <- factor(mean_synchrony$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))


################################################################################
### SYNCHRONY & GRAZING FINAL FIGURE ###########################################
#graph synchrony metrics together
ggplot(mean_synchrony, aes(x=TREATMENT, y=mean_value, col = Metric_Name)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_value-SE_value, ymax = mean_value+SE_value), width= 0.075) +
  scale_color_brewer(palette = "Dark2") +
  ylab("Synchrony") + xlab("Treatment") +
  theme_bw()

ggsave("synchrony_metrics_grazing.pdf", height = 6, width = 10)
ggsave("synchrony_metrics_grazing.png", height = 6, width = 10)

################################################################################


## calculate timescale specific VR #############################################

### 4 YEARS ####################################################################
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

################################################################################


### 2 YEARS ####################################################################
#set up data frame to put tsvr into
outnames2 <- c("Unique_ID", "TREATMENT", "classicVR", "longVR", "shortVR")
siteout2 <- as.data.frame(matrix(nrow=0, ncol = 5)) 
names(siteout2) <- outnames2
plots.2 <- unique(klee_long$Unique_ID)


for (i in 1:length(plots.2)) {
  
  #subset by replicate (gives all observations from one plot over time)
  plot.2 <- subset(klee_long, Unique_ID == plots.2[i]) %>%
    tbl_df()
  
  #select species and fill 0's 
  plot2.2 <- plot.2 %>%
    select(Date_numeric, SPECIES, Pin_Hits) %>% #selecting only these three columns
    spread(SPECIES, Pin_Hits, fill = 0) #changing to wide format
  
  #transpose the data 
  dat.2 <- t(as.matrix(plot2.2[,2:dim(plot2.2)[2]]))
  
  #create a dataframe with replicate info to attach VR metrics to
  VR_plots.2 <- plot.2 %>%
    select(Unique_ID, TREATMENT, BLOCK) %>%
    unique()
  
  #calculate tsvr
  res0.2 <- vreq_classic(dat.2)
  VR_plots.2$classicVR <- res0.2[[3]]
  
  #attach classic VR?
  res.2 <- tsvreq_classic(dat.2)
  
  #aggregate short vs. long
  resLong.2 <- aggts(res.2, res.2$ts[res.2$ts>=3]) #grabbing tsvr with time period >= 3 years
  resShort.2 <- aggts(res.2, res.2$ts[res.2$ts<3]) #grabbing tsvr with time period < 3 years
  
  #attach short & long
  VR_plots.2$longVR <- resLong.2[[3]]
  VR_plots.2$shortVR <- resShort.2[[3]]
  
  #append to external dataframe
  siteout2 <- rbind(siteout2, VR_plots.2)
}
#rename 
tsVR.2 <- siteout2



## calculate the mean and sd of tsvr
meantsVR.2 <- tsVR.2 %>%
  pivot_longer(cols = classicVR:shortVR, names_to = "VR_type", values_to = "VR_value" ) %>% 
  group_by(TREATMENT, VR_type) %>%
  summarise(meanVR = mean(VR_value), SEVR = calcSE(VR_value))

################################################################################

meantsVR.2$TREATMENT <- as.factor(meantsVR.2$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
meantsVR.2$TREATMENT <- factor(meantsVR.2$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))


#variance ratio vs. VR type, grouped by treatment
ggplot(meantsVR.2, aes(x=TREATMENT, y=meanVR, col = VR_type)) +
  geom_hline(yintercept = 1, color = "grey", lty = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanVR-SEVR, ymax = meanVR+SEVR), width = 0.1) +
  theme_classic() +
  ylab("Variance Ratio") + xlab("")
  #facet_wrap(~TREATMENT)



meantsVR$TREATMENT <- as.factor(meantsVR$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
meantsVR$TREATMENT <- factor(meantsVR$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))



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

### TSVR & GRAZING TREATMENT FINAL FIGURE ######################################
## Best way to visualize this, so far. 
#variance ratio vs. treatment, grouped by VR type
ggplot(meantsVR, aes(x=TREATMENT, y=meanVR, col = VR_type)) +
  geom_hline(yintercept = 1, color = "grey", lty = "dashed") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = meanVR-SEVR, ymax = meanVR+SEVR), width = 0.1) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  ylab("Variance Ratio") + xlab("Treatment")

ggsave("timespecific_VR_grazing.pdf", width = 6, height = 4)
ggsave("timespecific_VR_grazing.png", width = 6, height = 4)
################################################################################




### Compare VR, Synchrony, & Stability #########################################

#change data frame to long format
tsvr_long <- tsVR %>%
  pivot_longer(cols = classicVR:shortVR, names_to = "VR_type", values_to = "VR_value" )
  
#join TSVR with stability data frame
tsvr_stab_all <- inner_join(stability, tsvr_long, by = "Unique_ID")

#plot stability vs. VR to see if they are correlated.
ggplot(tsvr_stab_all, aes(x=VR_value, y=stability)) +
  geom_point() +
  facet_wrap(~VR_type) +
  ylab("Stability") + xlab("Variance Ratio") +
  geom_smooth(method = "lm") +
  theme_bw()
  

#join mean stability & mean tsVR data frames. Not a clean, perfect data frame, will work for initial visualizations
tsvr_stab <- inner_join(mean_stability, meantsVR, by = "TREATMENT") 

################################################################################
### TSVR & STABILITY FINAL FIGURE ##############################################
ggplot(tsvr_stab, aes(x=meanVR, y=mean_st, col = TREATMENT)) +
  geom_point() +
  facet_wrap(~VR_type) +
  ylab("Stability") + xlab("Variance Ratio") +
  geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst), width = 0.2) +
  geom_errorbarh(aes(xmin = meanVR-SEVR, xmax=meanVR+SEVR)) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

ggsave("stability_tsvr.pdf", width = 15, height = 5)
ggsave("stability_tsvr.png", width = 15, height = 5)
################################################################################





#synchrony vs. stability
syn_stab <- inner_join(mean_synchrony, mean_stability, by = "TREATMENT")

syn_stab2 <- syn_stab %>%
  filter(Metric_Name == "loreau_synchrony")

################################################################################
### SYNCHRONY & STABILITY FINAL FIGURE #########################################
ggplot(syn_stab, aes(x=mean_value , y=mean_st, col = TREATMENT, shape = Metric_Name)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst), width = 0.005) +
  geom_errorbarh(aes(xmin = mean_value-SE_value, xmax=mean_value+SE_value), height = 0.1) +
  ylab("Stability") + xlab("Synchrony") +  
  scale_shape_manual(values = c(0,16))

ggsave("stability_synchrony.pdf", width = 10, height = 6)
ggsave("stability_synchrony.png", width = 10, height = 6)
################################################################################


################################################################################
### SYNCHRONY & STABILITY FINAL FIGURE #########################################
ggplot(syn_stab2, aes(x=mean_value , y=mean_st, col = TREATMENT)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst), width = 0.005) +
  geom_errorbarh(aes(xmin = mean_value-SE_value, xmax=mean_value+SE_value), height = 0.1) +
  ylab("Stability") + xlab("Synchrony") +  
  scale_shape_manual(values = c(0,16))

#ggsave("stability_synchrony.pdf", width = 10, height = 6)
ggsave("stability_loreau_synchrony.png", width = 6, height = 4)
################################################################################


################################################################################
### SYNCHRONY & Grazing Tx FINAL FIGURE ########################################
ggplot(syn_stab2, aes(x=TREATMENT , y=mean_value)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  #geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst), width = 0.005) +
  geom_errorbar(aes(ymin = mean_value-SE_value, ymax=mean_value+SE_value), width = 0.3) +
  ylab("Synchrony") + xlab("") +  
  scale_shape_manual(values = c(0,16))

#ggsave("stability_synchrony.pdf", width = 10, height = 6)
ggsave("loreau_synchrony_treatment.png", width = 6, height = 4)
################################################################################


################################################################################
### Stable Dominance ###########################################################
################################################################################

### Calculate evenness metrics #################################################

#calculate simpsons evenness metric (Avolio 2019)
simps_even <- community_structure(
  df = klee_long,
  time.var = "Date_final",
  abundance.var = "Pin_Hits",
  replicate.var = "Unique_ID",
  metric = "SimpsonEvenness"
)

#calculate Evar evenness metric (Avolio 2019)
evar_even <- community_structure(
  df = klee_long,
  time.var = "Date_final",
  abundance.var = "Pin_Hits",
  replicate.var = "Unique_ID",
  metric = "Evar"
)

#calculate EQ evenness metric (Avolio 2019)
eq_even <- community_structure(
  df = klee_long,
  time.var = "Date_final",
  abundance.var = "Pin_Hits",
  replicate.var = "Unique_ID",
  metric = "EQ"
)


### Visualize Evenness metrics #################################################

##Simpson's Evenness Index
#join simpson's evenness with treatment data frame
even_sim <- inner_join(simps_even, treats, by="Unique_ID")

meansimp_even <- even_sim %>%
  group_by(TREATMENT) %>%
  summarise(mean_simp = mean(SimpsonEvenness), SEsimp = calcSE(SimpsonEvenness))

meansimp_even$TREATMENT <- as.factor(meansimp_even$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
meansimp_even$TREATMENT <- factor(meansimp_even$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))

#graph simpson's evenness over time.
ggplot(even_sim, aes(x=Date_final, y=SimpsonEvenness, col=TREATMENT)) +
  geom_line() +
  facet_wrap(~TREATMENT)
#initial decline in evenness in first few years - could be artifact or treatment response
#particularly sharp drops in O & C treatments

#graph mean simp even
ggplot(meansimp_even, aes(x=TREATMENT, y=mean_simp, col=TREATMENT)) +
  geom_point() +
  theme_bw() +
  geom_errorbar(aes(ymin=mean_simp-SEsimp, ymax=mean_simp+SEsimp), width = 0.2)

 

## Evar Evenness metric
even_evar <- inner_join(evar_even, treats, by="Unique_ID")

meanevar_even <- even_evar %>%
  group_by(TREATMENT) %>%
  summarise(mean_evar = mean(Evar), SE_evar = calcSE(Evar))

meanevar_even$TREATMENT <- as.factor(meanevar_even$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
meanevar_even$TREATMENT <- factor(meanevar_even$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))

#graph Evar evenness over time.
ggplot(even_evar, aes(x=Date_final, y=Evar, col=TREATMENT)) +
  geom_line() + 
  facet_wrap(~TREATMENT)

#
ggplot(meanevar_even, aes(x=TREATMENT, y=mean_evar, col=TREATMENT)) +
  geom_point() +
  theme_bw() +
  geom_errorbar(aes(ymin=mean_evar-SE_evar, ymax=mean_evar+SE_evar), width = 0.2)




### EQ Evenness metric
even_eq <- inner_join(eq_even, treats, by="Unique_ID")

meaneq_even <- even_eq %>%
  group_by(TREATMENT) %>%
  summarise(mean_eq = mean(EQ), SE_eq = calcSE(EQ))

meaneq_even$TREATMENT <- as.factor(meaneq_even$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
meaneq_even$TREATMENT <- factor(meaneq_even$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))

#graph EQ evenness over time.
ggplot(even_eq, aes(x=Date_final, y=EQ, col=TREATMENT)) +
  geom_line() + 
  facet_wrap(~TREATMENT)
#both EQ & Evar evenness... O lacks the longer scale fluctuations over time. Interesting.

ggplot(meaneq_even, aes(x=TREATMENT, y=mean_eq, col=TREATMENT)) +
  geom_point() +
  theme_bw() +
  geom_errorbar(aes(ymin=mean_eq-SE_eq, ymax=mean_eq+SE_eq), width = 0.2)



## EQ & Simpson's evenness metrics show same patterns across treatments. Evar does not. 
## EQ & Simpson's suggest that more grazing = higher evenness
## evenness seems to be lowest in O & W treatments 


### Evenness and Stability #####################################################

simpeven_stab <- inner_join(meansimp_even, mean_stability, by="TREATMENT")

ggplot(simpeven_stab, aes(x=mean_simp, y=mean_st, col = TREATMENT)) +
  geom_point() +
  geom_errorbarh(aes(xmin=mean_simp-SEsimp, xmax=mean_simp+SEsimp)) +
  geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst)) +
  ylab("Stability") + xlab("Evenness (Simpson's)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()


################################################################################

### Calculate Berger-Parker Dominance ##########################################

#BP dominance: d = Nmax/N (relative abundance of most dominant species)
BP_dom <- klee_long %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>%
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) %>% #calculate total abundance
  filter(rank == max(rank)) %>% #only include most abundant species in each plot
  summarise(BP_dominance = Pin_Hits/tot_abund) %>% #calculate Berger-Parker dominance index
  group_by(TREATMENT, Date_final) %>%
  summarise(mean_dom = mean(BP_dominance), SEdom = calcSE(BP_dominance))

#reorder treatments to match grazing pressure.
BP_dom$TREATMENT <- as.factor(BP_dom$TREATMENT) #change treatment to factor
BP_dom$TREATMENT <- factor(BP_dom$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))




## Graph dominance over time ###################################################
ggplot(BP_dom, aes(x=Date_final, y=mean_dom, col=TREATMENT)) +
  geom_line() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  ylab("Berger-Parker Dominance") + xlab("Date")
ggsave("dominance_over_time.pdf", width = 6, height = 4)
ggsave("dominance_over_time.png", width = 8, height = 4)

avg_BPdom <- BP_dom %>%
  group_by(TREATMENT) %>%
  summarise(avg_dom = mean(mean_dom), SE = calcSE(mean_dom))


ggplot(avg_BPdom, aes(x=TREATMENT, y=avg_dom)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  ylab("Berger-Parker Dominance") + xlab("Treatment") +
  geom_errorbar(aes(ymin=avg_dom-SE, ymax=avg_dom+SE), width=0.2)
ggsave("dominance_treat.pdf", width = 4, height = 3)
ggsave("dominance_treat.png", width = 4, height = 3)


dom_stab <- inner_join(mean_stability, avg_BPdom, by = "TREATMENT")

ggplot(dom_stab, aes(x=avg_dom, y=mean_st, col=TREATMENT)) +
  geom_point() +
  theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  ylab("Stability") + xlab("Berger-Parker Dominance") +
  geom_errorbarh(aes(xmin=avg_dom-SE, xmax=avg_dom+SE)) +
  geom_errorbar(aes(ymin=mean_st-SEst, ymax=mean_st+SEst))
ggsave("dominance_stability.pdf", width = 6, height = 4)
ggsave("dominance_stability.png", width = 6, height = 4)




################################################################################
### Calculate ranks & dominant species stability & compensatory dynamics #######

## rank species in order of abundance with 1 being most abundant.
rankneg <- klee_long %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>%
  mutate(rank = rank(-Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) #calculate total abundance




### O Treatment ################################################################
#filter for species with top 5 highest abundance in any year in 0 treatment
rank5O <- rankneg %>%
  filter(rank  < 6, TREATMENT == "0")

#make a time series of the dominant species in control plots
#make a list of species
O.species <- unique(rank5O$SPECIES)

#filter to include only these species in the 0 treatment
O <- rankneg %>%
  filter(SPECIES %in% O.species, TREATMENT == "0")

meanO <- O %>%
  group_by(Date_final, SPECIES) %>%
  summarise(meanhits = mean(Pin_Hits))

#visualize species over time
ggplot(meanO, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Unique_ID) +
  theme_classic()

## calculate species stability 
O.spstab <- O %>%
  group_by(BLOCK, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits)

## Plot species stability of any species that was in top 5 most abundant species throughout time series
ggplot(O.spstab, aes(x=SPECIES, y=sp_stability, col=SPECIES)) +
  geom_boxplot() +
  theme_classic() +
  ylab("Species Stability")
  

## calculate compensatory dynamics among species
O.vr <- variance_ratio(
  O,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  bootnumber = 10000, #used this as large bootnumber was recommended
  replicate.var = "Unique_ID",
  average.replicates = FALSE,
  level = 0.95,
)
  
head(O.vr)



### W Treatment ################################################################
rank5W <- rankneg %>%
  filter(rank  < 6, TREATMENT == "W")

ggplot(rank5W, aes(x=Date_final, y=Pin_Hits, col=SPECIES)) +
  geom_point() 

#make a time series of the dominant species in control plots
#make a list of species
W.species <- unique(rank5W$SPECIES)

W <- rankneg %>%
  filter(SPECIES %in% W.species, TREATMENT == "W")

ggplot(W, aes(x=Date_final, y=Pin_Hits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Unique_ID) +
  theme_classic()


meanW <- W %>% 
  group_by(Date_final, SPECIES) %>%
  summarise(meanhits = mean(Pin_Hits))

ggplot(meanW, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  theme_classic()


## calculate species stability 
W.spstab <- W %>%
  group_by(BLOCK, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits)

ggplot(W.spstab, aes(x=SPECIES, y=sp_stability, col = SPECIES)) +
  geom_boxplot()


## calculate compensatory dynamics among species
W.vr <- variance_ratio(
  W,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  bootnumber = 10000, #used this as large bootnumber was recommended
  replicate.var = "Unique_ID",
  average.replicates = FALSE,
  level = 0.95,
)

head(W.vr)


### MW Treatment ###############################################################
rank5MW <- rankneg %>%
  filter(rank  < 6, TREATMENT == "MW")

ggplot(rank5MW, aes(x=Date_final, y=Pin_Hits, col=SPECIES)) +
  geom_point() 

#make a time series of the dominant species in control plots
#make a list of species
MW.species <- unique(rank5MW$SPECIES)

MW <- rankneg %>%
  filter(SPECIES %in% MW.species, TREATMENT == "MW")

ggplot(MW, aes(x=Date_final, y=Pin_Hits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Unique_ID) +
  theme_classic()

meanMW <- MW %>% 
  group_by(Date_final, SPECIES) %>%
  summarise(meanhits = mean(Pin_Hits))

ggplot(meanMW, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  theme_classic()


## calculate species stability 
MW.spstab <- MW %>%
  group_by(BLOCK, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits)

ggplot(MW.spstab, aes(x=SPECIES, y=sp_stability, col = SPECIES)) +
  geom_boxplot()


## calculate compensatory dynamics among species
MW.vr <- variance_ratio(
  MW,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  bootnumber = 10000, #used this as large bootnumber was recommended
  replicate.var = "Unique_ID",
  average.replicates = FALSE,
  level = 0.95,
)

head(MW.vr)


### C Treatment ################################################################

rank5C <- rankneg %>%
  filter(rank  < 6, TREATMENT == "C")

ggplot(rank5C, aes(x=Date_final, y=Pin_Hits, col=SPECIES)) +
  geom_point() 

#make a time series of the dominant species in control plots
#make a list of species
C.species <- unique(rank5C$SPECIES)

C <- rankneg %>%
  filter(SPECIES %in% C.species, TREATMENT == "C")

ggplot(C, aes(x=Date_final, y=Pin_Hits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Unique_ID) +
  theme_classic()

meanC <- C %>% 
  group_by(Date_final, SPECIES) %>%
  summarise(meanhits = mean(Pin_Hits))

ggplot(meanC, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  theme_classic()


## calculate species stability 
C.spstab <- C %>%
  group_by(BLOCK, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits)

ggplot(C.spstab, aes(x=SPECIES, y=sp_stability, col = SPECIES)) +
  geom_boxplot()


## calculate compensatory dynamics among species
C.vr <- variance_ratio(
  C,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  bootnumber = 10000, #used this as large bootnumber was recommended
  replicate.var = "Unique_ID",
  average.replicates = FALSE,
  level = 0.95,
)

head(C.vr)


### WC Treatment ###############################################################
rank5WC <- rankneg %>%
  filter(rank  < 6, TREATMENT == "WC")

ggplot(rank5WC, aes(x=Date_final, y=Pin_Hits, col=SPECIES)) +
  geom_point() 

#make a time series of the dominant species in control plots
#make a list of species
WC.species <- unique(rank5WC$SPECIES)

WC <- rankneg %>%
  filter(SPECIES %in% WC.species, TREATMENT == "WC")

ggplot(WC, aes(x=Date_final, y=Pin_Hits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Unique_ID) +
  theme_classic()

meanWC <- WC %>% 
  group_by(Date_final, SPECIES) %>%
  summarise(meanhits = mean(Pin_Hits))

ggplot(meanWC, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  theme_classic()


## calculate species stability 
WC.spstab <- WC %>%
  group_by(BLOCK, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits)

ggplot(WC.spstab, aes(x=SPECIES, y=sp_stability, col = SPECIES)) +
  geom_boxplot()


## calculate compensatory dynamics among species
WC.vr <- variance_ratio(
  WC,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  bootnumber = 10000, #used this as large bootnumber was recommended
  replicate.var = "Unique_ID",
  average.replicates = FALSE,
  level = 0.95,
)

head(WC.vr)


### MWC Treatment ##############################################################

rank5MWC <- rankneg %>%
  filter(rank  < 6, TREATMENT == "MWC")

ggplot(rank5MWC, aes(x=Date_final, y=Pin_Hits, col=SPECIES)) +
  geom_point() 

#make a time series of the dominant species in control plots
#make a list of species
MWC.species <- unique(rank5MWC$SPECIES)

MWC <- rankneg %>%
  filter(SPECIES %in% MWC.species, TREATMENT == "MWC")

ggplot(MWC, aes(x=Date_final, y=Pin_Hits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  #facet_wrap(~Unique_ID) +
  theme_classic()

meanMWC <- MWC %>% 
  group_by(Date_final, SPECIES) %>%
  summarise(meanhits = mean(Pin_Hits))

ggplot(meanMWC, aes(x=Date_final, y=meanhits, col = SPECIES)) +
  geom_point() +
  geom_line() +
  theme_classic()


## calculate species stability 
MWC.spstab <- MWC %>%
  group_by(BLOCK, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits)

ggplot(MWC.spstab, aes(x=SPECIES, y=sp_stability, col = SPECIES)) +
  geom_boxplot()


## calculate compensatory dynamics among species
MWC.vr <- variance_ratio(
  MWC,
  time.var = "Date_numeric",
  species.var = "SPECIES",
  abundance.var = "Pin_Hits",
  bootnumber = 10000, #used this as large bootnumber was recommended
  replicate.var = "Unique_ID",
  average.replicates = FALSE,
  level = 0.95,
)

head(MWC.vr)














### Make Rank Abundance Curves? ################################################
## rank each species by abundance
RAC <- klee_long %>%
  group_by(Unique_ID, Date_final) %>%
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average"))


top5 <- RAC %>%
  filter(rank < 6, TREATMENT == "0")

top3 <- RAC %>%
  filter(rank < 3, TREATMENT == "0")

top <- RAC %>%
  filter(rank < 2, TREATMENT == "0")

# create a rank abundance curve

ggplot(RAC_O_1st, aes(x=rank, y=Pin_Hits)) +
  geom_point() +
  geom_text(aes(label=SPECIES),hjust=0, vjust=0)


## calculate rank abundance curve difference & change

## RAC Differences by Treatment ################################################
RACdiff <- RAC_difference(df = klee_long,
                          species.var = "SPECIES",
                          abundance.var = "Pin_Hits",
                          replicate.var = "Unique_ID",
                          treatment.var = "TREATMENT",
                          time.var = "Date_numeric", 
                          reference.treatment = "MWC"
                
)

treats2 <- treats
names(treats2) <- c("BLOCK", "TREATMENT2", "Unique_ID2")


RACdiff_tx <- inner_join(RACdiff, treats2, by = "Unique_ID2") %>%
  mutate(Date = ymd(Date_numeric)) %>%
  group_by(Date, TREATMENT2.x) %>%
  summarise(mean_even = mean(evenness_diff), evenSE = calcSE(evenness_diff))

RACdiff_tx$TREATMENT2.x <- as.factor(RACdiff_tx$TREATMENT2.x) #change treatment to factor

#reorder treatments to match grazing pressure.
RACdiff_tx$TREATMENT2.x <- factor(RACdiff_tx$TREATMENT2.x, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))


ggplot(RACdiff_tx, aes(x=Date, y=mean_even, col= TREATMENT2.x)) +
  geom_line() +
  geom_point() +
  theme_classic() +
  ylab("Evenness Compared to MWC") + xlab("") +
  geom_hline(yintercept = 0, color = "grey", lty = "dashed") +
  geom_smooth(method = "lm") +
  facet_wrap(~TREATMENT2.x) +
  scale_color_brewer(palette = "Dark2") +
  geom_errorbar(aes(ymin=mean_even-evenSE, ymax=mean_even+evenSE))



## RAC Changes over time #######################################################
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

## graph year to year changes in evenness
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




## calculate total species richness across the time series #####################
#may need to eventually use presence-absence data (accounts for a lot more rare species)
totalrich <- klee_long %>%
  group_by(Unique_ID) %>%
  select(Unique_ID, SPECIES) %>%
  unique() %>%
  summarise(totalrich = n())



rich_stab <- left_join(stability, totalrich, by = "Unique_ID")
meanrich_stab <- rich_stab %>%
  group_by(TREATMENT) %>%
  summarise(mean_st = mean(stability), SEst = calcSE(stability), mean_rich = mean(totalrich), SErich = calcSE(totalrich))

rich_stab$TREATMENT <- as.factor(rich_stab$TREATMENT) #change treatment to factor

#reorder treatments to match grazing pressure.
rich_stab$TREATMENT <- factor(rich_stab$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))

#plot stability vs. total richness
ggplot(rich_stab, aes(x=totalrich, y=stability, col = TREATMENT)) +
  geom_point()


ggplot(rich_stab, aes(x=TREATMENT, y=totalrich, col = TREATMENT)) +
  geom_point()

ggplot(meanrich_stab, aes(x=TREATMENT, y=mean_rich)) +
  geom_point() +
  geom_errorbar(aes(ymin=mean_rich-SErich, ymax=mean_rich+SErich), width = 0.2) +
  ylab("Total Richness Across Time Series") + xlab("Treatment") +
  theme_bw()
#grazing treatments don't appear to produce differences in the total richness across time.


ggplot(meanrich_stab, aes(x=mean_rich, y=mean_st, col=TREATMENT)) +
  geom_point() +
  theme_bw() +
  geom_errorbarh(aes(xmin=mean_rich-SErich, xmax=mean_rich+SErich))



## Calculate Richness at each Time step ########################################
#calculate richness by grouping by unique ID and date and counting # of observations.
ann_rich <- klee_long %>%
  group_by(Unique_ID, Date_final) %>%
  summarise(richness = n())

ann_rich <- inner_join(ann_rich, treats, by="Unique_ID")

ggplot(ann_rich, aes(x=Date_final, y=richness, col= TREATMENT)) +
  geom_line() +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  facet_wrap(~TREATMENT)

#calculate the average species richness for each plot then each treatment. Average by plot first then by treatment
avg_rich <- ann_rich %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarise(meanSR_plot = mean(richness)) %>%
  group_by(TREATMENT) %>%
  summarise(meanSR_tx = mean(meanSR_plot), SESR_tx = calcSE(meanSR_plot))

ggplot(avg_rich, aes(x=TREATMENT, y=meanSR_tx)) +
  geom_point() +
  geom_errorbar(aes(ymin=meanSR_tx-SESR_tx, ymax=meanSR_tx+SESR_tx)) +
  ylab("Average Species Richness") + xlab("") +
  theme_bw()
ggsave("avg_rich_treatment.png", width = 6, height=4)


avg_rich2 <- ann_rich %>%
  group_by(TREATMENT) %>%
  summarise(meanSR=mean(richness), SESR = calcSE(richness))

################################################################################
### Verifying the Portfolio Effect #############################################

#subset by unique_ID & species

portfolio_effect <- klee_long %>%
  filter(!is.na(SPECIES), !is.na(Pin_Hits)) %>%
  filter(Pin_Hits > 0) %>%
  group_by(Unique_ID, SPECIES) %>%
  summarise(variance = var(Pin_Hits, na.rm = TRUE), meanhits = mean(Pin_Hits)) %>%
  mutate(logvar = log(variance), logmean = log(meanhits))
#this gives some NA values for variance. Unsure why this is or if this is a problem?

#need to filter out 0 values before linear regression will work.
portfolio <- portfolio_effect %>%
  #filter(!is.na(variance), !is.na(logvar), !is.na(logmean)) %>%
  filter(logvar > 0, logmean > 0)

#flipped variance and mean around, slope is now in the correct range.
taylor <- lm(logvar~logmean, data = portfolio)

summary(taylor)
#check that slope b/w 1-2
#slope is 1.90052

ggplot(portfolio, aes(x= logmean, y= logvar)) +
  geom_point() +
  geom_smooth(method = "lm")
