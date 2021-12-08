## Correlation Analysis for Moving Windows
### Stability & Classic VR Correlation Test
```{r, echo=FALSE}
## Does the classic VR correlate with Stability changes over time?
stab_cvr_mw <- left_join(stab_mw_tx, cVR_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 4, window_size < 16)

## create vector of treatments
trt <- unique(stab_cvr_mw$TREATMENT)

## create vector of window size
wsize <- unique(stab_cvr_mw$window_size)

## create empty dataframe to contain output
corr_cvr <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) 


## loop the correlation test over every treatment and every window size of calculation
for (i in 1:length(trt)) {
  
  ## select the treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- stab_cvr_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$classicVR, tempw$stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    ## create a dataframe with the correlation coefficient and p-value
    t <- tempw %>%
      group_by(TREATMENT) %>% 
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value) 
    
    ## bind to output dataframe
    corr_cvr <- rbind(corr_cvr, t)
  }
  
}

corr_cvr <- corr_cvr[-1,]

## create a variable indicating significance of p-value
corr_cvr <- corr_cvr %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(corr_cvr)
```

### Stability & Richness Correlation Test
```{r, echo=FALSE}
## Stability Richness Correlation test
##create data frame for correlation test
stab_rich_mw <- left_join(stab_mw_tx, rich_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 3, window_size < 16)

## create vector of treatments
trt <- unique(stab_rich_mw$TREATMENT)

## create vector of window size
wsize <- unique(stab_rich_mw$window_size)

corr_rich <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output

## loop the correlation test over every treatment and every window size of calculation
for (i in 1:length(trt)) {
  
  ## select the ith treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- stab_rich_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$richness, tempw$stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    ## create a dataframe with the correlation coefficient and p-value
    t <- tempw %>%
      group_by(TREATMENT) %>%
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value)
    
    ## bind to output dataframe
    corr_rich <- rbind(corr_rich, t)
    
  }
  
}
corr_rich <- corr_rich[-1,]

## create a variable indicating significance of p-value
corr_rich <- corr_rich %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(corr_rich)
```

### Stability & Pop Stability Correlation Test
```{r, echo=FALSE}
# Stability - Pop Stability Correlation test
##create data frame for correlation test
stab_popst_mw <- left_join(stab_mw_tx, popst_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 3, window_size < 16)

## create vector of treatments
trt <- unique(stab_popst_mw$TREATMENT)

## create vector of window size
wsize <- unique(stab_popst_mw$window_size)

corr_popst <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output


## loop the correlation test over every treatment and every window size of calculation
for (i in 1:length(trt)) {
  
  ## select the ith treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- stab_popst_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$mean_popst, tempw$stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    ## create a dataframe with the correlation coefficient and p-value
    t <- tempw %>%
      group_by(TREATMENT) %>%
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value)
    
    
    ## bind to output dataframe
    corr_popst <- rbind(corr_popst, t)
    
  }
  
}
corr_popst <- corr_popst[-1,]

## create a variable indicating significance of p-value
corr_popst <- corr_popst %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(corr_popst)
```
