

## PRC/RDA Analysis w/full time series (RELATIVE COVER)

## Create Reference Treatment values using Jan 1999 data averaged across all treatments and plots - one average value per block 
date <- as.factor(unique(klee_rel$Date_final)) ## make vector of dates 

refyear <- data.frame(BLOCK = NA, SPECIES = NA, relcov = NA, Pin_Hits = NA, TREATMENT = NA, Unique_ID = NA, Date_final = NA, Date_numeric = NA) ## create empty data frame

## calculate the reference year values in a for-loop so that it populates the same set of reference values for all dates in the time series
for (i in 1:length(date)) {
  
  temp <- klee_rel %>%
    filter(Date_final == "1999-06-01") %>% #filter to reference date
    group_by(BLOCK, SPECIES) %>% #group by block and species to calc avg conditions in each block
    summarize(relcov = mean(relcov), Pin_Hits = mean(Pin_Hits)) %>% # take the mean cover of each sp in each block
    mutate(TREATMENT = "REF", Unique_ID = "REF", Date_final = date[i], Date_numeric = paste(year(Date_final), month(Date_final), day(Date_final), sep = "0")) # add values for treatment, unique ID, etc.
  
  refyear <- rbind(refyear, temp) #add to the reference year dataframe
  
}

refyear <- refyear[-1,] #remove row of NAs

## Get the data frames ready to combine... 
klee_rel$Date_numeric <- as.numeric(klee_rel$Date_numeric)
refyear$Date_numeric <- as.numeric(refyear$Date_numeric)
klee_rel$Date_final <- as.Date.factor(klee_rel$Date_final)
refyear$Date_final <- as.Date.factor(refyear$Date_final)
klee_rel$TREATMENT <- as.factor(klee_rel$TREATMENT)
refyear$TREATMENT <- as.factor(refyear$TREATMENT)

## TRYING PRC WITH UP TO 2013 DATA; NOT LOG TRANSFORMED----
#1 add reference point as a new sampling point to species data
relcovprc <- rbind(klee_rel, refyear) %>%
  pivot_wider(names_from = "SPECIES", values_from = "relcov") %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "REF", "O", "W", "MW", "C", "WC", "MWC")) %>%
  filter(Date_numeric < 20140601)
## important that REF is ordered first so that it becomes the comparison

relcovprc[is.na(relcovprc)] <- 0 ## replace NA values with 0

## create vectors of the year and treatment
year <- as.factor(relcovprc$Date_numeric)
TRT <- as.factor(relcovprc$TREATMENT)

## remove the non-species columns
sp_only <- relcovprc %>%
  ungroup() %>%
  select(!(1:6))

## Principal Response Curve Analysis
prc_relcov <- prc(response = sp_only, treatment = TRT, time = year)
summary(prc_relcov)

prcsum_relcov <- summary(prc_relcov) ## save the summary of the PRC analysis

prc_relresults <- data.frame(coeff = prcsum_relcov$coefficients) %>% ## add the coefficients to a dataframe
  rownames_to_column("TREATMENT") %>%
  pivot_longer(cols = 2:16, names_to = "year", values_to = "coeff") %>%
  mutate(Date_numeric = str_remove(year, "coeff."), Date_final = ymd(Date_numeric)) %>%
  select(!year) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) %>%
  mutate(coeff = coeff*(-100))

prc_relresults$TREATMENT <- as.factor(prc_relresults$TREATMENT)

## plot coefficients over time
ggplot(prc_relresults, aes(x=Date_final, y=coeff, color = TREATMENT)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = green_scale) +
  xlab("Date") + ylab("PRC Coefficient")


## TRY WITH FULL TIME SERIES ----
#1 add reference point as a new sampling point to species data
relcovprc_all <- rbind(klee_rel, refyear) %>%
  pivot_wider(names_from = "SPECIES", values_from = "relcov") %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "REF", "O", "W", "MW", "C", "WC", "MWC"))
## important that REF is ordered first so that it becomes the comparison

relcovprc_all[is.na(relcovprc_all)] <- 0 ## replace NA values with 0

## create vectors of the year and treatment
year_all <- as.factor(relcovprc_all$Date_numeric)
TRT_all <- as.factor(relcovprc_all$TREATMENT)

## remove the non-species columns
sp_only_all <- relcovprc_all %>%
  ungroup() %>%
  select(!(1:6))

## Principal Response Curve Analysis
prc_relcov_all <- prc(response = sp_only_all, treatment = TRT_all, time = year_all)
summary(prc_relcov_all)

prcsum_relcov_all <- summary(prc_relcov_all) ## save the summary of the PRC analysis

prc_relresults_all <- data.frame(coeff = prcsum_relcov_all$coefficients) %>% ## add the coefficients to a dataframe
  rownames_to_column("TREATMENT") %>%
  pivot_longer(cols = 2:23, names_to = "year", values_to = "coeff") %>%
  mutate(Date_numeric = str_remove(year, "coeff."), Date_final = ymd(Date_numeric)) %>%
  select(!year) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC")) %>%
  mutate(coeff = coeff*(-1))

prc_relresults_all$TREATMENT <- as.factor(prc_relresults_all$TREATMENT)





## PCR/RDA Analysis w/full time series (TOTAL COVER)

## Create Reference Treatment values using Jan 1999 data averaged across all treatments and plots - one average value per block 
date <- as.factor(unique(klee_annual$Date_final)) ## make vector of dates 

refyear <- data.frame(BLOCK = NA, SPECIES = NA, Pin_Hits = NA, TREATMENT = NA, Unique_ID = NA, Date_final = NA, Date_numeric = NA) ## create empty data frame

## calculate the reference year values in a for-loop so that it populates the same set of reference values for all dates in the time series
for (i in 1:length(date)) {
  
  temp <- klee_annual %>%
    filter(Date_final == "1999-06-01") %>% #filter to reference date
    group_by(BLOCK, SPECIES) %>% #group by block and species to calc avg conditions in each block
    summarize(Pin_Hits = mean(Pin_Hits)) %>% # take the mean cover of each sp in each block
    mutate(TREATMENT = "REF", Unique_ID = "REF", Date_final = date[i], Date_numeric = paste(year(Date_final), month(Date_final), day(Date_final), sep = "0")) # add values for treatment, unique ID, etc.
  
  refyear <- rbind(refyear, temp) #add to the reference year dataframe
  
}

refyear <- refyear[-1,] #remove row of NAs



#1 add reference point as a new sampling point to species data
kleeprc <- rbind(klee_annual, refyear) %>%
  pivot_wider(names_from = "SPECIES", values_from = "Pin_Hits") %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "REF", "O", "W", "MW", "C", "WC", "MWC")) %>%
  filter(Date_numeric < 20140601)
## important that REF is ordered first so that it becomes the comparison

kleeprc[is.na(kleeprc)] <- 0 ## replace NA values with 0

## create vectors of the year and treatment
year <- as.factor(kleeprc$Date_numeric)
TRT <- as.factor(kleeprc$TREATMENT)

## remove the non-species columns
sp_only <- kleeprc %>%
  select(!(1:5)) #%>%
#log(+1)

## log transform the data, since it is count data
sp_only_log <- log(sp_only + 1)



## Principal Response Curve Analysis
prc_klee <- prc(response = sp_only_log, treatment = TRT, time = year)
summary(prc_klee)

prcsum <- summary(prc_klee) ## save the summary of the PRC analysis

prcresults <- data.frame(coeff = prcsum$coefficients) %>% ## add the coefficients to a dataframe
  rownames_to_column("TREATMENT") %>%
  pivot_longer(cols = 2:23, names_to = "year", values_to = "coeff") %>%
  mutate(Date_numeric = str_remove(year, "coeff."), Date_final = ymd(Date_numeric)) %>%
  select(!year) %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))

prcresults$TREATMENT <- as.factor(prcresults$TREATMENT)

## plot coefficients over time
ggplot(prcresults, aes(x=Date_final, y=coeff, color = TREATMENT)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = green_scale) +
  xlab("Date") + ylab("PRC Coefficient")

## need the change in PRC Axis 1 score

plot(prc_klee)




## plot
plot(prc_klee, xlim = c(19990601,20130601),  scaling = 0, species = FALSE)


#3 carry out RDA of modified species with indicator variables as explanatory (environmental) data

#4 create plot from RDA results by plotting the destandardized canonical coefficients against time and adding access with the species scores
