## set working directory
setwd("~/Repositories/klee-stability")

## Load packages
library(tidyverse)
library(codyn)
library(tsvr)

## source cleaned data
source("klee_allyears_cleaning.R")

# Calculations
## Calculate Community Stability for all years
commstab <- community_stability(klee_annual, 
                                time.var = "Date_numeric", 
                                abundance.var = "Pin_Hits", 
                                replicate.var = "Unique_ID")

## Calculate Variance of the full community
v <- klee_annual %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarise(variance = var(Pin_Hits))

## Calculate Total Cover
pinhits <- totcov %>% #this dataframe used in models
  group_by(TREATMENT, Unique_ID) %>%
  summarize(pinhits = mean(totcov))


## join stability measures with treatment info & calculate CV
tstab <- left_join(commstab, treats, by = "Unique_ID") %>% #join comm stability with treatment info
  mutate(CV = 1/stability) #calculate the CV as inverse of stability

variance <- left_join(v, pinhits, by = c("TREATMENT", "Unique_ID"))

stability <- left_join(tstab, variance, by = c("TREATMENT", "Unique_ID"))

rm(list = c("v", "commstab", "variance", "tstab"))

## Calculate TSVR
### for full community
## Calculate Timescale Specific VR ##
#set up data frame to put tsvr into
outnames <- c("Unique_ID", "TREATMENT", "classicVR", "longVR", "shortVR") #column names
siteout <- as.data.frame(matrix(nrow=0, ncol = 5)) #make empty dataframe
names(siteout) <- outnames #set names for empty dataframe
plots <- unique(klee_annual$Unique_ID) #make vector of unique plots

## Use for loop to calculate TSVR for each plot
for (i in 1:length(plots)) {
  
  #subset by replicate (gives all observations from one plot over time)
  plot <- subset(klee_annual, Unique_ID == plots[i]) %>%
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
  
  #calculate classic VR
  res0 <- vreq_classic(dat)
  VR_plots$classicVR <- res0[[3]] #extracting classic VR
  
  #calculate tsvr 
  res <- tsvreq_classic(dat)
  
  #aggregate short vs. long variance ratios
  resLong <- aggts(res, res$ts[res$ts>=4]) #grabbing tsvr with time period >= 4 years
  resShort <- aggts(res, res$ts[res$ts<4]) #grabbing tsvr with time period <4 years
  
  #attach short & long variance ratios
  VR_plots$longVR <- resLong[[3]]
  VR_plots$shortVR <- resShort[[3]]
  
  #append to external dataframe
  siteout<-rbind(siteout, VR_plots)
}

#rename data frame
tsVR <- siteout #%>%
# mutate(community_type = "All_Species")

## Calculate the mean and standard error of VR metrics
meantsVR <- tsVR %>%
  pivot_longer(cols = classicVR:shortVR, names_to = "VR_type", values_to = "VR_value" ) %>% #change to long format
  group_by(TREATMENT, VR_type) %>%
  summarise(meanVR = mean(VR_value), SEVR = calcSE(VR_value))

#reorder treatments to match grazing pressure.
meantsVR$TREATMENT <- as.factor(meantsVR$TREATMENT) #change treatment to factor
meantsVR$TREATMENT <- factor(meantsVR$TREATMENT, levels = c("O", "W",  "MW",  "C", "WC", "MWC"))

### for dominant 5 species
## Calculate Timescale Specific VR ##
#set up data frame to put tsvr into
b5outnames <- c("Unique_ID", "TREATMENT", "classicVR", "longVR", "shortVR") #column names
b5siteout <- as.data.frame(matrix(nrow=0, ncol = 5)) #make empty dataframe
names(b5siteout) <- b5outnames #set names for empty dataframe
b5plots <- unique(big5annual$Unique_ID) #make vector of unique plots

## Use for loop to calculate TSVR for each plot
for (i in 1:length(b5plots)) {
  
  #subset by replicate (gives all observations from one plot over time)
  plot <- subset(big5annual, Unique_ID == b5plots[i]) %>%
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
  
  #calculate classic VR
  res0 <- vreq_classic(dat)
  VR_plots$b5classicVR <- res0[[3]] #extracting classic VR
  
  #calculate tsvr 
  res <- tsvreq_classic(dat)
  
  #aggregate short vs. long variance ratios
  resLong <- aggts(res, res$ts[res$ts>=4]) #grabbing tsvr with time period >= 4 years
  resShort <- aggts(res, res$ts[res$ts<4]) #grabbing tsvr with time period <4 years
  
  #attach short & long variance ratios
  VR_plots$b5longVR <- resLong[[3]]
  VR_plots$b5shortVR <- resShort[[3]]
  
  #append to external dataframe
  b5siteout<-rbind(b5siteout, VR_plots)
}

#rename data frame
b5tsVR <- b5siteout #%>%
# mutate(community_type = "Dominant")


## Calculate the mean and standard error of VR metrics
b5meantsVR <- b5tsVR %>%
  pivot_longer(cols = b5classicVR:b5shortVR, names_to = "VR_type", values_to = "VR_value" ) %>% #change to long format
  group_by(TREATMENT, VR_type) %>%
  summarise(meanVR = mean(VR_value), SEVR = calcSE(VR_value))

#reorder treatments to match grazing pressure.
b5meantsVR$TREATMENT <- as.factor(b5meantsVR$TREATMENT) #change treatment to factor
b5meantsVR$TREATMENT <- factor(b5meantsVR$TREATMENT, levels = c("O", "W",  "MW",  "C", "WC", "MWC"))


## Calculate Dominance
## Full Time Series
## calculate Berger-Parker Dominance for each unique ID at each date
BP_dominance <- klee_annual %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment, plot, and date
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) %>% #create column for total abundance in each plot
  filter(rank == max(rank)) %>% #only include most abundant species in each plot
  summarise(BP_dominance = Pin_Hits/tot_abund) %>% #calculate Berger-Parker dominance index
  mutate(date = "all")

## Calculate the mean Berger-Parker dominance by: 
## TREATMENT and UNIQUE ID - to use eventually for linear models
meanplot_BPdom <- BP_dominance %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarize(mean_dom = mean(BP_dominance), SEdom = calcSE(BP_dominance), vardom = var(BP_dominance),
            CVdom = mean_dom/sd(BP_dominance))

dominance <- BP_dominance %>%
  group_by(TREATMENT, Date_final) %>%
  summarize(mean_dom = mean(BP_dominance), SE_dom = calcSE(BP_dominance))

## Calculate Dominant species Pop Stability
## Calculate population stability of individual dominant species
dompopstab <- big5annual %>%
  group_by(TREATMENT, Unique_ID, SPECIES) %>%
  summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), sp_stability = temp_mean/sdhits) 

meandompopstab <- dompopstab %>%
  #ungroup() %>%
  group_by(TREATMENT, SPECIES) %>%
  summarise(popstability = mean(sp_stability), SEpop = calcSE(sp_stability)) #%>%

dompopstab$TREATMENT <- as.factor(dompopstab$TREATMENT)

dompopstab <- dompopstab %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))


## Calculate aggregate stability of dominant 5 species
## Calculate Community Stability for all years
domsp_agg_stab <- community_stability(big5annual, 
                                      time.var = "Date_numeric", 
                                      abundance.var = "Pin_Hits", 
                                      replicate.var = "Unique_ID")

colnames(domsp_agg_stab) <- c("Unique_ID", "Domsp_stability")

## I don't think this is the right way to do this... this is then just looking at does the stability of a subset of the community correlate with the stability of the full community - which is highly likely especially if picking the most abundant species... 


## Could try the average population stability of dominant species or pick the most dominant species across the time series

## Average population stability of dominant species
meanpopstab <- dompopstab %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarize(meanpopstab = mean(sp_stability))

## Calculate Richness
## Calculate species richness for each plot at each point in time
rich <- klee_annual %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% ## group by each plot at each date
  summarise(richness = n())
## use this dataframe for the ANOVA?

## Calculate the mean richness by: 
## TREATMENT and DATE
meanannrich <- rich %>%
  group_by(TREATMENT, Date_final) %>%
  summarise(meanrich = mean(richness), SErich = calcSE(richness)) %>%
  mutate(date = "all")
## TREATMENT and UNIQUE_ID
meanrichplot <- rich %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarise(meanrich = mean(richness, SErich = calcSE(richness)), varrich = var(richness), 
            CVrich = (meanrich/sd(richness))) 

## BIG 5 SPECIES 
## Calculate species richness for each plot at each point in time
big5rich <- big5annual %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% ## group by each plot at each date
  summarise(richness = n())

## Calculate the mean richness by: 
## TREATMENT and DATE
big5meanannrich <- big5rich %>%
  group_by(TREATMENT, Date_final) %>%
  summarise(meanrich = mean(richness), SErich = calcSE(richness))
## TREATMENT and UNIQUE_ID
big5meanrichplot <- big5rich %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarise(meanrich = mean(richness, SErich = calcSE(richness)), varrich = var(richness))

## NON-DOMINANT SPECIES
## Calculate species richness for each plot at each point in time
nondomrich <- nondom %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% ## group by each plot at each date
  summarise(richness = n())

## Calculate the mean richness by: 
## TREATMENT and DATE
nondommeanannrich <- nondomrich %>%
  group_by(TREATMENT, Date_final) %>%
  summarise(meanrich = mean(richness), SErich = calcSE(richness))
## TREATMENT and UNIQUE_ID
nondommeanrichplot <- nondomrich %>%
  group_by(TREATMENT, Unique_ID) %>%
  summarise(meanrich = mean(richness, SErich = calcSE(richness)), varrich = var(richness))

## Create combined Stability & Mechanisms Data Frames
## ALL YEARS
## Create data frame with one value of stability & mechanisms for each plot
ssdom <- left_join(stability, meanplot_BPdom, by = c("Unique_ID", "TREATMENT"))
tsvrssdom <- left_join(ssdom, tsVR, by = c("Unique_ID", "TREATMENT", "BLOCK"))
b5tsvrdom <- left_join(tsvrssdom, b5tsVR, by = c("Unique_ID", "TREATMENT", "BLOCK"))
domspstsvr <- left_join(b5tsvrdom, meanpopstab, by = c("Unique_ID", "TREATMENT"))
#ndtsvrdom <- left_join(b5tsvrdom, ndtsVR, by = c("Unique_ID", "TREATMENT", "BLOCK"))
stability_mechanisms <- left_join(domspstsvr, meanrichplot, by = c("Unique_ID", "TREATMENT"))

## Create data frame with one value of stability & mechanisms for each treatment
meanstab_mech <- stability_mechanisms %>%
  group_by(TREATMENT) %>%
  summarise(mean_st = mean(stability), SE_st = calcSE(stability), mean_classicVR = mean(classicVR),
            SEclassicVR = calcSE(classicVR), b5meanclassicVR = mean(b5classicVR), 
            SEb5classicVR = calcSE(b5classicVR), SE_dom = calcSE(mean_dom), mean_dom = mean(mean_dom),
            mean_rich = mean(meanrich), SE_rich = calcSE(meanrich), mean_vardom = mean(vardom), 
            SE_vardom = calcSE(vardom), mean_varrich = mean(varrich), SE_varrich = calcSE(varrich),
            mean_CV = mean(CV), SE_CV = calcSE(CV), meanlongVR = mean(longVR), SElVR = calcSE(longVR),
            meanshortVR = mean(shortVR), SEsVR = calcSE(shortVR), b5meanlongVR = mean(b5longVR), 
            SEb5lVR = calcSE(b5longVR), b5meanshortVR = mean(b5shortVR), SEb5sVR = calcSE(b5shortVR),
            meanCV_dom = mean(CVdom), SECV_dom = calcSE(CVdom), meanCV_rich = mean(CVrich), 
            SECV_rich = calcSE(CVrich), avgpopstab = mean(meanpopstab), 
            SEpopstab = calcSE(meanpopstab), mean_bio = mean(pinhits), SE_bio = calcSE(pinhits),
            mean_var = mean(variance), SE_var = calcSE(variance)) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) #reorder treatments

## clean up environment
rm(list = c("ssdom",  "tsvrssdom", "b5tsvrdom", "domspstsvr"))


## create cows & wildlife columns 
stability_mechanisms$cows <- 0
stability_mechanisms$cows[stability_mechanisms$TREATMENT %in% c('C','WC', 'MWC')] <- 1
stability_mechanisms$cows <- factor(stability_mechanisms$cows)

stability_mechanisms$wildlife <- 0
stability_mechanisms$wildlife[stability_mechanisms$TREATMENT %in% c('W','WC','MW','MWC')] <- 1
#stability_mechanisms$wildlife[stability_mechanisms$TREATMENT %in% c('MW','MWC')] <- 2
stability_mechanisms$wildlife <- factor(stability_mechanisms$wildlife)

stability_mechanisms$mega <- 0
stability_mechanisms$mega[stability_mechanisms$TREATMENT %in% c('MW','MWC')] <- 1
stability_mechanisms$mega <- factor(stability_mechanisms$mega)


rm(list = c("avg_biomass", "b5meantsVR", "b5siteout", "b5tsVR", "big5annual",
            "big5meanannrich", "big5meanrichplot", "big5rich", "BP_dominance",
            "dominance", "dompopstab", "domsp_agg_stab", "klee_annual", "klee_long",
            "meanannrich", "meandompopstab", "meanplot_BPdom", "meanpopstab", "meanrichplot",
            "meantsVR", "nondom", "nondommeanannrich", "nondommeanrichplot", "nondomrich",
            "pinhits", "plot", "plot2", "res", "res0", "resLong", "resShort",
            "rich", "siteout", "stability", "VR_plots", "tsVR"))
