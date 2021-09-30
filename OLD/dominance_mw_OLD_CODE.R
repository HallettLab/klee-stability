## Create Dominance over a Moving Window Function
#```{r, echo=FALSE} 
dominancemw_func <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$Date_final)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  
  movingwindow <- data.frame(TREATMENT = NA, Unique_ID = NA, dominance = NA, timestep = NA)
  
  #colnames(movingwindow) <- c("TREATMENT", "timestep", "mean_dominance")
  sample_points <- sort(unique(input_data$Date_numeric)) ## create ordered list of sample points
  
  #n_plots <- length(unique(input_data$Unique_ID)) ## number of unique plots
  #n_trt <- length(unique(input_data$TREATMENT))
  #treatments <- unique(input_data$TREATMENT)
  
  for (i in 1:n_windows){
    
    temp_samplepts <- sample_points[i:(i+timestep-1)] ## create a vector of sample points for each iteration
    
    temp_dom <- input_data %>%
      filter(Date_numeric %in% temp_samplepts) %>% ## filter the correct sample points from input data
      group_by(TREATMENT, Unique_ID, Date_numeric) %>%
      mutate(rank = rank(Pin_Hits, na.last = NA, ties.method = "average")) %>% #rank species in order of abundance
      mutate(tot_abund = sum(Pin_Hits)) %>% #calculate total abundance
      filter(rank == max(rank)) %>% #only include most abundant species in each plot
      summarise(dominance = Pin_Hits/tot_abund) #calculate Berger-Parker dominance index
    
    temp_dom$timestep <- i ## create a column for timestep in the temporary data frame
    
    mean_dom <- temp_dom %>%  ## calculate the average dominance over each time window for each plot
      group_by(TREATMENT, Unique_ID, timestep) %>%
      summarise(dominance = mean(dominance))
    
    
    movingwindow <- rbind(movingwindow, mean_dom)
    
  }
  
  return(movingwindow) ## retrieve data frame at the end
  
}
#```

## Apply Dominance MW function
#```{r, echo = FALSE}
mw_size <- c(3:21)

mwdominance <- data.frame(TREATMENT = NA, Unique_ID = NA, dominance = NA, timestep = NA, window_size = NA)

for(i in 1:length(mw_size)){
  
  tmwd <- dominancemw_func(input_data = klee_annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  mwdominance <- rbind(mwdominance, tmwd)
  
}
mwdominance <- mwdominance[-1,]


dom_mw_tx <- mwdominance %>%
  group_by(TREATMENT, timestep, window_size) %>%
  summarize(mean_dom = mean(dominance), SE_dom = calcSE(dominance)) %>%
  filter(!is.na(TREATMENT))

dom_mw_tx$TREATMENT <- as.factor(dom_mw_tx$TREATMENT)

dom_mw_tx <- dom_mw_tx %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))




mean_mwdom <-  mwdominance %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_dom = mean(dominance), SE_dom = calcSE(dominance)) %>%
  filter(!is.na(TREATMENT))

mean_mwdom$TREATMENT <- as.factor(mean_mwdom$TREATMENT)

mean_mwdom <- mean_mwdom %>%
  mutate(TREATMENT = fct_relevel(TREATMENT, "O", "W", "MW", "C", "WC", "MWC"))
#```






### Stability & Dominance Correlation Test
#```{r, echo=FALSE}
## Stability Dominance Correlation test 
##create data frame for correlation test
stab_dom_mw <- left_join(stab_mw_tx, dom_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 3, window_size < 16)

## create vector of treatments
trt <- unique(stab_dom_mw$TREATMENT)

## create vector of window size
wsize <- unique(stab_dom_mw$window_size)

corr_dom <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output


for (i in 1:length(trt)) {
  
  ## select the ith treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- stab_dom_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$mean_dom, tempw$mean_stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    t <- tempw %>%
      group_by(TREATMENT) %>%
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value)
    
    corr_dom <- rbind(corr_dom, t)
    
  }
  
}
corr_dom <- corr_dom[-1,]

## create a variable indicating significance of p-value
corr_dom <- corr_dom %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(corr_dom)
#```