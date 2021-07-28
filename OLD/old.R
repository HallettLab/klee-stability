## Moving Window Correlation Analysis
```{r}
stab_syn_mw$TREATMENT <- as.factor(stab_syn_mw$TREATMENT) #change treatment to factor
stab_syn_mw$TREATMENT <- factor(stab_syn_mw$TREATMENT, levels = c("O", "W",  "MW",  "C", "MWC", "WC"))

## create function
syncorrmw5_func <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$timestep)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  corr_syn <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA, corr_window = NA, corr_timestep = NA) ## create empty dataframe to contain output
  
  sample_points <- sort(unique(input_data$timestep)) ## create ordered list of sample points
  
  ## create vector of treatments
  trt <- unique(input_data$TREATMENT)
  
  ## create vector of window size
  wsize <- unique(input_data$window_size)
  
  for (k in 1:n_windows){
    
    temp_samplepts <- sample_points[k:(k+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>%
      filter(timestep %in% temp_samplepts) ## filter the correct sample points from input data to run through the community stability function
    
    for (i in 1:length(trt)) {
      
      ## select the treatment
      tx <- trt[i]
      
      ## filter by treatment
      temptx <- temp %>%
        filter(TREATMENT %in% tx)
      
      for (j in 1:length(wsize)){
        
        ## select window size
        wind <- 5
        
        ## filter by window size
        tempw <- temptx %>%
          filter(window_size %in% wind)
        
        ## feed through cor.test function
        cortstl <- cor.test(tempw$meanloreau, tempw$mean_stability, 
                            alternative = "two.sided", 
                            method = "pearson")
        
        t <- tempw %>%
          group_by(TREATMENT) %>%
          summarise(window_size = mean(window_size)) %>%
          mutate(corr_coefficient = cortstl$estimate, p_val = cortstl$p.value) #%>%
        #mutate(corr_timestep = )
        
        t$corr_window <- timestep
        t$corr_timestep <- k ## create a column for timestep in the temporary data frame
        
        corr_syn <- rbind(corr_syn, t)
      }
      
    }
    
    
  }
  
  return(corr_syn) ## retrieve data frame at the end
  
}


syncorrmw7_func <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$timestep)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  corr_syn <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA, corr_window = NA, corr_timestep = NA) ## create empty dataframe to contain output
  
  sample_points <- sort(unique(input_data$timestep)) ## create ordered list of sample points
  
  ## create vector of treatments
  trt <- unique(input_data$TREATMENT)
  
  ## create vector of window size
  wsize <- unique(input_data$window_size)
  
  for (k in 1:n_windows){
    
    temp_samplepts <- sample_points[k:(k+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>%
      filter(timestep %in% temp_samplepts) ## filter the correct sample points from input data to run through the community stability function
    
    for (i in 1:length(trt)) {
      
      ## select the treatment
      tx <- trt[i]
      
      ## filter by treatment
      temptx <- temp %>%
        filter(TREATMENT %in% tx)
      
      for (j in 1:length(wsize)){
        
        ## select window size
        wind <- 7
        
        ## filter by window size
        tempw <- temptx %>%
          filter(window_size %in% wind)
        
        ## feed through cor.test function
        cortstl <- cor.test(tempw$meanloreau, tempw$mean_stability, 
                            alternative = "two.sided", 
                            method = "pearson")
        
        t <- tempw %>%
          group_by(TREATMENT) %>%
          summarise(window_size = mean(window_size)) %>%
          mutate(corr_coefficient = cortstl$estimate, p_val = cortstl$p.value) #%>%
        #mutate(corr_timestep = )
        
        t$corr_window <- timestep
        t$corr_timestep <- k ## create a column for timestep in the temporary data frame
        
        corr_syn <- rbind(corr_syn, t)
      }
      
    }
    
    
  }
  
  return(corr_syn) ## retrieve data frame at the end
  
}
## the loop only works with one window size at a time (although not for 10 at all)

mwcorr55 <- syncorrmw5_func(input_data = stab_syn_mw, timestep = 5)
mwcorr55 <- mwcorr5[-1,]

mwcorr75 <- syncorrmw7_func(input_data = stab_syn_mw, timestep = 5)
mwcorr75 <- mwcorr75[-1,]

mwcorr57 <- syncorrmw5_func(input_data = stab_syn_mw, timestep = 7)
mwcorr57 <- mwcorr57[-1,]

mwcorr77 <- syncorrmw7_func(input_data = stab_syn_mw, timestep = 7)
mwcorr77 <- mwcorr77[-1,]

mwcorr_analysis5 <- rbind(mwcorr55, mwcorr75) %>%
  mutate(significance = ifelse(p_val <= 0.05, "significant", 
                               ifelse(p_val <0.07 && p_val > 0.05, "marginally", "not significant")))
mwcorr_analysis7 <- rbind(mwcorr77, mwcorr57) %>%
  mutate(significance = ifelse(p_val <= 0.05, "significant", 
                               ifelse(p_val <0.07 && p_val > 0.05, "marginally", "not significant")))

mwcorr_analysis7$significance <- as.factor(mwcorr_analysis7$significance)
mwcorr_analysis7$window_size <- as.factor(mwcorr_analysis7$window_size)

ggplot(mwcorr_analysis7, aes(x=corr_timestep, y=corr_coefficient, color = significance)) +
  geom_point(size = 2)+
  #geom_line() +
  facet_wrap(~TREATMENT+window_size) +
  ylab("Correlation Coeff of Loreau Synchrony and Stability") + xlab("Time Step") +
  ggtitle("Moving Window Correlational Analysis (7 year window)")


```




```{r}

domcorrmw7_func <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$timestep)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  corr_dom <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA, corr_window = NA, corr_timestep = NA) ## create empty dataframe to contain output
  
  sample_points <- sort(unique(input_data$timestep)) ## create ordered list of sample points
  
  ## create vector of treatments
  trt <- unique(input_data$TREATMENT)
  
  ## create vector of window size
  wsize <- unique(input_data$window_size)
  
  for (k in 1:n_windows){
    
    temp_samplepts <- sample_points[k:(k+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>%
      filter(timestep %in% temp_samplepts) ## filter the correct sample points from input data to run through the community stability function
    
    for (i in 1:length(trt)) {
      
      ## select the treatment
      tx <- trt[i]
      
      ## filter by treatment
      temptx <- temp %>%
        filter(TREATMENT %in% tx)
      
      for (j in 1:length(wsize)){
        
        ## select window size
        wind <- 7
        
        ## filter by window size
        tempw <- temptx %>%
          filter(window_size %in% wind)
        
        ## feed through cor.test function
        cortstl <- cor.test(tempw$mean_dominance, tempw$mean_stability, 
                            alternative = "two.sided", 
                            method = "pearson")
        
        t <- tempw %>%
          group_by(TREATMENT) %>%
          summarise(window_size = mean(window_size)) %>%
          mutate(corr_coefficient = cortstl$estimate, p_val = cortstl$p.value) #%>%
        #mutate(corr_timestep = )
        
        t$corr_window <- timestep
        t$corr_timestep <- k ## create a column for timestep in the temporary data frame
        
        corr_dom <- rbind(corr_dom, t)
      }
      
    }
  }
  
  return(corr_dom) ## retrieve data frame at the end
  
}

mwcorrdom7 <- domcorrmw7_func(input_data = stab_dom_mw, timestep = 7) 
mwcorrdom7 <- mwcorrdom7[-1,]

mwcorrdom7 <- mwcorrdom7 %>%
  group_by(TREATMENT, corr_timestep) %>%
  summarise(corr_coefficient = mean(corr_coefficient), p_val = mean(p_val)) %>%
  mutate(significance = ifelse(p_val <= 0.05, "significant", 
                               ifelse(p_val <0.07 && p_val > 0.05, "marginally", "not significant")))


ggplot(mwcorrdom7, aes(x=corr_timestep, y=corr_coefficient, color = significance)) +
  geom_point(size = 2)+
  #geom_line() +
  facet_wrap(~TREATMENT) +
  ylab("Correlation Coeff of Dominance and Stability") + xlab("Time Step") +
  ggtitle("Moving Window Correlational Analysis (7 year window)")


```





## Calculate Gross synchrony metric
#gross_synchrony <- synchrony(
# klee_annual,
# time.var = "Date_numeric",
# species.var = "SPECIES",
# abundance.var = "Pin_Hits",
#  replicate.var = "Unique_ID",
#  metric="Gross")
#colnames(gross_synchrony) <- c("Unique_ID", "gross_synchrony") #rename columns

#big5gross_synchrony <- synchrony(
#  big5annual,
# time.var = "Date_numeric",
#  species.var = "SPECIES",
# abundance.var = "Pin_Hits",
#  replicate.var = "Unique_ID",
#  metric="Gross")
#colnames(big5gross_synchrony) <- c("Unique_ID", "gross_synchrony") #rename columns

#nondomgross_synchrony <- synchrony(
# nondom,
# time.var = "Date_numeric",
#  species.var = "SPECIES",
#  abundance.var = "Pin_Hits",
#  replicate.var = "Unique_ID",
#  metric="Gross")
#colnames(nondomgross_synchrony) <- c("Unique_ID", "gross_synchrony") #rename columns

## join two synchrony metrics into one data frame
#synchrony <- left_join(loreau_synchrony, gross_synchrony, by = "Unique_ID") 

#big5synchrony <- left_join(big5loreau_synchrony, big5gross_synchrony, by = "Unique_ID") 

#nondomsynchrony <- left_join(nondomloreau_synchrony, nondomgross_synchrony, by = "Unique_ID") 






#BP dominance: d = Nmax/N (relative abundance of most dominant species)
big5BP_dom <- big5annual %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment, plot, and date
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) %>% #calculate total abundance
  filter(rank == max(rank)) %>% #only include most abundant species in each plot
  summarise(BP_dominance = Pin_Hits/tot_abund) %>% #calculate Berger-Parker dominance index
  group_by(TREATMENT, Date_final) %>% #group by treatment and date
  summarise(mean_dom = mean(BP_dominance), SEdom = calcSE(BP_dominance)) #calculate mean and SE for each treatment at each date

big5BP_dom2 <- big5annual %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment, plot, and date
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) %>% #calculate total abundance
  filter(rank == max(rank)) %>% #only include most abundant species in each plot
  summarise(BP_dominance = Pin_Hits/tot_abund) %>% #calculate Berger-Parker dominance index
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment and date
  summarise(mean_dom = mean(BP_dominance), SEdom = calcSE(BP_dominance))

#BP dominance: d = Nmax/N (relative abundance of most dominant species)
nondomBP_dom <- nondom %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment, plot, and date
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) %>% #calculate total abundance
  filter(rank == max(rank)) %>% #only include most abundant species in each plot
  summarise(BP_dominance = Pin_Hits/tot_abund) %>% #calculate Berger-Parker dominance index
  group_by(TREATMENT, Date_final) %>% #group by treatment and date
  summarise(mean_dom = mean(BP_dominance), SEdom = calcSE(BP_dominance)) #calculate mean and SE for each treatment at each date

nondomBP_dom2 <- nondom %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment, plot, and date
  mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
  mutate(tot_abund = sum(Pin_Hits)) %>% #calculate total abundance
  filter(rank == max(rank)) %>% #only include most abundant species in each plot
  summarise(BP_dominance = Pin_Hits/tot_abund) %>% #calculate Berger-Parker dominance index
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment and date
  summarise(mean_dom = mean(BP_dominance), SEdom = calcSE(BP_dominance))



## calculate the mean dominance index by treatment
big5avg_BPdom <- big5BP_dom %>%
  group_by(TREATMENT) %>% #group by treatment
  summarise(avg_dom = mean(mean_dom), SE = calcSE(mean_dom)) #take mean across time for each treatment

big5dominance <- big5BP_dom2 %>%
  group_by(Unique_ID) %>%
  summarize(dominance = mean(mean_dom))

big5dominance <- left_join(big5dominance, treats, by = "Unique_ID")
big5dominance <- left_join(big5dominance, stability, by = c("Unique_ID", "BLOCK", "TREATMENT"))

## calculate the mean dominance index by treatment
nondomavg_BPdom <- nondomBP_dom %>%
  group_by(TREATMENT) %>% #group by treatment
  summarise(avg_dom = mean(mean_dom), SE = calcSE(mean_dom)) #take mean across time for each treatment

nondomdominance <- nondomBP_dom2 %>%
  group_by(Unique_ID) %>%
  summarize(dominance = mean(mean_dom))

nondomdominance <- left_join(nondomdominance, treats, by = "Unique_ID")
nondomdominance <- left_join(nondomdominance, stability, by = c("Unique_ID", "BLOCK", "TREATMENT"))




## Dominant Species, Rank, & Stability
```{r, echo=FALSE, fig.width =12, fig.height=6, fig.cap='**Figure 11:** Species stability (mean/sd) for species that were in the top 2 most abundant species in a given treatment at any point in the time series. Bars indicate one standard error above and below the mean. The mean rank of a species was obtained by ranking their abundance in a treatment and taking the mean rank of each species across the time series. '}

#dominant.sp <- dominant.sp[-40,]
#dominant.sp$TREATMENT <- as.factor(dominant.sp$TREATMENT) #change treatment to factor
#dominant.sp$TREATMENT <- factor(dominant.sp$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))


#ggplot(dominant.sp, aes(x=rank, y=stability, col=SPECIES)) +
# geom_point(size=2) +
# ylab("Species Stability") + xlab("Species Mean Rank over Time") +
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
# scale_color_manual(values = colors) +
# geom_errorbar(aes(ymin=stability-SE, ymax=stability+SE), width=0.75) +
#theme(legend.title = element_text(size=12)) +
#theme(text = element_text(size = 14)) +
# labs(col = "Species") + #change legend title
#facet_wrap(~TREATMENT) 
```