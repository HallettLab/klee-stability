## Create Synchrony over a Moving Window Function
#```{r, echo=FALSE}
## create function
synchronymw_func <- function(input_data, timestep, ...) { ## function inputs = data frame and the time step (window size)
  
  n_samples <- length(unique(input_data$Date_final)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  mw_synchrony <- data.frame(matrix(ncol=3,nrow=(n_windows*length(unique(input_data$Unique_ID))))) ## create empty data frame to contain output ## dimensions = # cols (3) x (uniqueID x timesteps)
  colnames(mw_synchrony) <- c("Unique_ID", "loreau_synchrony", "timestep")
  
  sample_points <- sort(unique(input_data$Date_numeric)) ## create ordered list of sample points
  
  n_plots <- length(unique(input_data$Unique_ID)) ## number of unique plots
  
  for (k in 1:n_windows){
    
    temp_samplepts <- sample_points[k:(k+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>%
      filter(Date_numeric %in% temp_samplepts) ## filter the correct sample points from input data to run through the community stability function
    
    ## Calculate Loreau synchrony metric
    temp_lsyn <- synchrony(
      temp,
      time.var = "Date_numeric",
      species.var = "SPECIES",
      abundance.var = "Pin_Hits",
      metric = "Loreau",
      replicate.var = "Unique_ID"
    )
    
    colnames(temp_lsyn) <- c("Unique_ID", "loreau_synchrony") #rename columns
    temp_lsyn$timestep <- k ## create a column for timestep in the temporary data frame
    
    mw_synchrony[((k-1)*n_plots + 1):(k*n_plots),] <- temp_lsyn ## add synchrony calculations for one iteration to the moving window data frame
    ## ((i-1)*n_plots + 1):(i*n_plots) -> where to put each iteration of the loop
    
  }
  return(mw_synchrony) ## retrieve data frame at the end
}

#```





## Apply Synchrony Moving Window function
```{r, echo = FALSE}
## For all species in the community
mw_size <- c(4:21)

mwsynchrony <- data.frame(Unique_ID = NA, loreau_synchrony = NA, timestep = NA, window_size = NA)

for(i in 1:length(mw_size)){
  
  tmwsyn <- synchronymw_func(input_data = klee_annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  mwsynchrony <- rbind(mwsynchrony, tmwsyn)
  
}
mwsynchrony <- mwsynchrony[-1,]

syn_mw_tx <- left_join(mwsynchrony, treats, by="Unique_ID") %>%
  group_by(TREATMENT, timestep, window_size) %>%
  summarize(mean_synchrony = mean(loreau_synchrony), SE_syn = calcSE(loreau_synchrony))

mean_mwsyn <- left_join(mwsynchrony, treats, by="Unique_ID") %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_syn = mean(loreau_synchrony), SE_syn = calcSE(loreau_synchrony))


## For big 5 species
mw_size <- c(4:21)

b5mwsynchrony <- data.frame(Unique_ID = NA, loreau_synchrony = NA, timestep = NA, window_size = NA)

for(i in 1:length(mw_size)){
  
  tmwsyn <- synchronymw_func(input_data = big5annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  b5mwsynchrony <- rbind(b5mwsynchrony, tmwsyn)
  
}
b5mwsynchrony <- b5mwsynchrony[-1,]

b5syn_mw_tx <- left_join(b5mwsynchrony, treats, by="Unique_ID") %>%
  group_by(TREATMENT, timestep, window_size) %>%
  summarize(mean_synchrony = mean(loreau_synchrony), SE_syn = calcSE(loreau_synchrony))

b5mean_mwsyn <- left_join(b5mwsynchrony, treats, by="Unique_ID") %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_syn = mean(loreau_synchrony), SE_syn = calcSE(loreau_synchrony))

## For Subordinate species
mw_size <- c(4:21)

ndmwsynchrony <- data.frame(Unique_ID = NA, loreau_synchrony = NA, timestep = NA, window_size = NA)

for(i in 1:length(mw_size)){
  
  tmwsyn <- synchronymw_func(input_data = nondom, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  ndmwsynchrony <- rbind(ndmwsynchrony, tmwsyn)
  
}
ndmwsynchrony <- ndmwsynchrony[-1,]

ndsyn_mw_tx <- left_join(ndmwsynchrony, treats, by="Unique_ID") %>%
  group_by(TREATMENT, timestep, window_size) %>%
  summarize(mean_synchrony = mean(loreau_synchrony), SE_syn = calcSE(loreau_synchrony))

ndmean_mwsyn <- left_join(ndmwsynchrony, treats, by="Unique_ID") %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_syn = mean(loreau_synchrony), SE_syn = calcSE(loreau_synchrony))

```

### Stability & Synchrony Correlation Test
#```{r, echo=FALSE}
## Does Synchrony correlate with Stability changes over time?
stab_syn_mw <- left_join(stab_mw_tx, syn_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 3, window_size < 16)

## create vector of treatments
trt <- unique(stab_syn_mw$TREATMENT)

## create vector of window size
wsize <- unique(stab_syn_mw$window_size)

corr_syn <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output


for (i in 1:length(trt)) {
  
  ## select the treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- stab_syn_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$mean_synchrony, tempw$mean_stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    t <- tempw %>%
      group_by(TREATMENT) %>% 
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value) 
    
    corr_syn <- rbind(corr_syn, t)
  }
  
}

corr_syn <- corr_syn[-1,]

## create a variable indicating significance of p-value
corr_syn <- corr_syn %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(corr_syn)
#```

### Stability & Big 5 Synchrony Correlation Test
#```{r, echo=FALSE}
## Big 5 Synchrony Stability Correlation Test
big5stab_syn_mw <- left_join(stab_mw_tx, b5syn_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 3, window_size < 16)

## create vector of treatments
trt <- unique(big5stab_syn_mw$TREATMENT)

## create vector of window size
wsize <- unique(big5stab_syn_mw$window_size)

b5corr_syn <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output


for (i in 1:length(trt)) {
  
  ## select the treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- big5stab_syn_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$mean_synchrony, tempw$mean_stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    t <- tempw %>%
      group_by(TREATMENT) %>% 
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value) #%>%
    # mutate(corr_coefficientg = cortstg$estimate, gp_val = cortstg$p.value)
    
    b5corr_syn <- rbind(b5corr_syn, t)
  }
  
}

b5corr_syn <- b5corr_syn[-1,]

## create a variable indicating significance of p-value
b5corr_syn <- b5corr_syn %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))
print(b5corr_syn)

#```

### Stability & Subordinate Correlation Test
#```{r, echo=FALSE}
## Non Dominant Synchrony Stability Correlation Test
nondomstab_syn_mw <- left_join(stab_mw_tx, ndsyn_mw_tx, by = c("TREATMENT", "timestep", "window_size")) %>%
  filter(window_size > 3, window_size < 16)

## create vector of treatments
trt <- unique(nondomstab_syn_mw$TREATMENT)

## create vector of window size
wsize <- unique(nondomstab_syn_mw$window_size)

ndcorr_syn <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output


for (i in 1:length(trt)) {
  
  ## select the treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- nondomstab_syn_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$mean_synchrony, tempw$mean_stability, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    t <- tempw %>%
      group_by(TREATMENT) %>% 
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value)# %>%
    #  mutate(corr_coefficientg = cortstg$estimate, gp_val = cortstg$p.value)
    
    ndcorr_syn <- rbind(ndcorr_syn, t)
  }
}

ndcorr_syn <- ndcorr_syn[-1,]

## create a variable indicating significance of p-value
ndcorr_syn <- ndcorr_syn %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(ndcorr_syn)
#```

### Combine Stability & Synchrony correlations into one dataframe
#```{r, echo=FALSE}
corr_syn$TREATMENT <- as.factor(corr_syn$TREATMENT) #change treatment to factor
corr_syn$TREATMENT <- factor(corr_syn$TREATMENT, levels = c("O", "W", "MW", "C", "WC", "MWC"))

## Make one correlation data frame
corr_syn <- corr_syn %>%
  mutate(type = "all species")
b5corr_syn <- b5corr_syn %>%
  mutate(type = "big 5")
ndcorr_syn <- ndcorr_syn %>%
  mutate(type="non-dominant")

syncorr <- rbind(corr_syn, b5corr_syn)
syncorr_all <- rbind(syncorr, ndcorr_syn) 

#```




#```{r}
## Synchrony and Dominance Correlation Test ----
syn_dom_mw <- left_join(syn_mw_tx, dom_mw_tx, by = c("TREATMENT", "timestep", "window_size"))

## create vector of treatments
trt <- unique(syn_dom_mw$TREATMENT)

## create vector of window size
wsize <- unique(syn_dom_mw$window_size)

corr_syndom <- data.frame(TREATMENT = NA, window_size = NA, corr_coefficient = NA, p_val = NA) ## create empty dataframe to contain output


for (i in 1:length(trt)) {
  
  ## select the ith treatment
  tx <- trt[i]
  
  ## filter by treatment
  temptx <- syn_dom_mw %>%
    filter(TREATMENT %in% tx)
  
  for (j in 1:length(wsize)){
    
    ## select window size
    wind <- wsize[j]
    
    ## filter by window size
    tempw <- temptx %>%
      filter(window_size %in% wind)
    
    ## feed through cor.test function
    cortst <- cor.test(tempw$mean_dominance, tempw$meanloreau, 
                       alternative = "two.sided", 
                       method = "pearson")
    
    t <- tempw %>%
      group_by(TREATMENT) %>%
      summarise(window_size = mean(window_size)) %>%
      mutate(corr_coefficient = cortst$estimate, p_val = cortst$p.value)
    
    corr_syndom <- rbind(corr_syndom, t)
    
  }
  
}
corr_syndom <- corr_syndom[-1,]

## create a variable indicating significance of p-value
corr_syndom <- corr_syndom %>%
  mutate(significant = ifelse(p_val < 0.051, "S", 
                              ifelse(p_val > 0.05 & p_val < 0.07, "M", "NS")))

print(corr_syndom)

#```












# Other Figures
## Plot Drought Relevance Score, Stability, & Synchrony
```{r, fig.width=8, fig.height=12}
drst10 <- ggplot(stdr10, aes(x=timestep, y=mean_stability, color = normDscore, shape = TREATMENT)) +
  geom_line(color = "black") +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(16,25,15,12,23,17)) +
  ylab("Stability") + xlab("Window Number") +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

drsyn10 <- ggplot(syndr10, aes(x=timestep, y=mean_synchrony, shape = TREATMENT, color = normDscore)) +
  geom_line(color = "black") +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(16,25,15,12,23,17)) +
  ylab("Synchrony") + xlab("Window Number")+
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

drrich10 <- ggplot(richdr10, aes(x=timestep, y=mean_rich, shape = TREATMENT, color = normDscore)) +
  geom_line(color = "black") +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(16,25,15,12,23,17)) +
  ylab("Richness") + xlab("Window Number") +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

drdom10 <- ggplot(domdr10, aes(x=timestep, y=mean_dom, shape = TREATMENT, color = normDscore)) +
  geom_line(color = "black") +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(16,25,15,12,23,17)) +
  ylab("Dominance") + xlab("Window Number") +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


ggarrange(drst10, drsyn10, drrich10, drdom10,
          ncol = 1, 
          common.legend = TRUE, 
          legend = "right", 
          labels = "AUTO")
```

## Pop Stability through Time
```{r}
ggplot(popstab, aes(x=timestep, y=popstability, color = TREATMENT)) +
  geom_line()


```


# Old Figures

## Synchrony & Stability Correlation
```{r, fig.width=8.5, fig.height=4}
synstabfinal <- ggplot(mw7synstabfig, aes(x=mean_synchrony, y=mean_stability)) +
  geom_point() +
  ylab("Stability") + xlab("Synchrony") +
  facet_wrap(~TREATMENT, ncol = 3) +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 14)) + 
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=timestep), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "davos", direction = -1) +
  labs(fill = "Window #") +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(2, 10))
synstabfinal
```

## OLD: Correlation Coefficients
```{r, echo = FALSE, fig.width=9, fig.height=6}
syncoeff <- ggplot(syncorr_all, aes(x=TREATMENT, y=corr_coefficient, color = type, shape = significant)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Synchrony and Stability") + xlab("") +
  scale_color_manual(values = c("#E58606", "#5D69B1", "#52BCA3")) +
  labs(col = "Type", shape = "Significance") +
  scale_shape_manual(values = c(10,1,16)) +
  facet_wrap(~window_size) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


corr_dom$TREATMENT <- as.factor(corr_dom$TREATMENT) #change treatment to factor
corr_dom$TREATMENT <- factor(corr_dom$TREATMENT, levels = c("O", "W", "MW", "C", "WC", "MWC"))

domcoeff <- ggplot(corr_dom, aes(x=TREATMENT, y=corr_coefficient, shape = significant)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Dominance and Stability") + xlab("Treatment") +
  scale_color_manual(values = c("#ED645A","#CC3A8E","#A5AA99")) +
  labs(shape = "Significance") +
  scale_shape_manual(values = c(10, 1,16)) +
  facet_wrap(~window_size) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  theme(legend.position = "none")
#E58606,#5D69B1,#52BCA3,#99C945,#CC61B0,#24796C,#DAA51B,#2F8AC4,#764E9F,#ED645A,#CC3A8E,#A5AA99


corr_rich$TREATMENT <- as.factor(corr_rich$TREATMENT) #change treatment to factor
corr_rich$TREATMENT <- factor(corr_rich$TREATMENT, levels = c("O", "W", "MW", "C", "WC", "MWC"))

richcoeff <- ggplot(corr_rich, aes(x=TREATMENT, y=corr_coefficient, shape = significant)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Richness and Stability") + xlab("Treatment") +
  scale_color_manual(values = c("#ED645A","#CC3A8E","#A5AA99")) +
  labs(shape = "Significance") +
  scale_shape_manual(values = c(10, 1,16)) +
  facet_wrap(~window_size) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  theme(legend.position = "none")


ggarrange(syncoeff,
          domcoeff, 
          ncol = 1, nrow = 2, 
          align = "hv", 
          common.legend = TRUE, 
          legend = "right")
```

## OLD
```{r}
syncorr7 <- syncorr_select %>%
  filter(window_size == "7")

synstabcorrcoeff <- ggplot(syncorr7, aes(x=TREATMENT, y=corr_coefficient, shape = significant)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Correlation Coefficent") + xlab("") +
  scale_color_manual(values = c("#E58606", "#5D69B1", "#52BCA3")) +
  labs(col = "Type", shape = "Significance") +
  scale_shape_manual(values = c(12,0,15)) +
  facet_wrap(~window_size, ncol = 1) +
  theme(legend.title = element_text(size=10)) +  theme(text = element_text(size = 10))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = c(-1.2,-0.065), legend.box = "vertical", legend.direction = "horizontal")


synstabdrought3col <- ggplot(mw7synstabfig, aes(x=mean_synchrony, y=mean_stability)) +
  geom_point() +
  ylab("Stability") + xlab("Synchrony") +
  facet_wrap(~TREATMENT, ncol = 3) +
  theme(legend.title = element_text(size=10)) +  theme(text = element_text(size = 10)) + 
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=timestep), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "davos", direction = -1) +
  labs(fill = "Window #") +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(2, 10)) +
  theme(legend.position = c(0.2,-0.25), legend.direction = "horizontal")

synstabdrought6col <- ggplot(mw7synstabfig, aes(x=mean_synchrony, y=mean_stability)) +
  geom_point() +
  ylab("Stability") + xlab("Synchrony") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=10)) +  theme(text = element_text(size = 10)) + 
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=timestep), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "davos", direction = -1) +
  labs(fill = "Window #") +
  theme(legend.position = "right") +
  coord_cartesian(ylim = c(2, 10)) +
  theme(legend.position = c(0.2,-0.25), legend.direction = "horizontal")

top <- plot_grid(stabdrought10, syndrought10, ncol = 1, align = "v")
topleg <- plot_grid(top, droughtlegend, rel_widths = c(6,1))

bottom <- plot_grid(synstabdrought3col, synstabcorrcoeff, rel_widths = c(3,1))

bottomlong <- plot_grid(synstabdrought6col, synstabcorrcoeff, rel_widths = c(6,1))



plot_grid(topleg, synstabdrought6col, ncol = 1, rel_heights = c(2.5,1))
```

## Figure S8: Synchrony over a moving window (Big 5 & Non Dom)
#```{r, echo=FALSE, fig.width =8, fig.height=4}

ggplot(big5syn_mw_tx, aes(x=timestep, y=meanloreau, color = TREATMENT)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colors) + #change color scheme
  ylab("Mean Loreau Synchrony (Big 5)") + xlab("Timestep") +
  facet_wrap(~window_size)


ggplot(nondomsyn_mw_tx, aes(x=timestep, y=meanloreau, color = TREATMENT)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = colors) + #change color scheme
  ylab("Mean Loreau Synchrony (Non-Dom)") + xlab("Timestep") +
  facet_wrap(~window_size)

#```



## Figure S10: Synchrony-Stability Correlations Final Figs
#```{r, fig.height=8, fig.width=8, echo=FALSE}
mw7synstabfig <- stab_syn_mw %>%
  filter(window_size == 7)

corrsynL <- ggplot(mw7synstabfig, aes(x=meanloreau, y=mean_stability, color = timestep)) +
  geom_point() +
  ylab("Stability") + xlab("Loreau Synchrony: All species") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))

mw7big5synstabfig <- big5stab_syn_mw %>%
  filter(window_size == 7)

b5corrsynL <- ggplot(mw7big5synstabfig, aes(x=meanloreau, y=mean_stability, color = timestep)) +
  geom_point() +
  ylab("Stability") + xlab("Loreau Synchrony: Big 5 species") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


mw7nondomsynstabfig <- nondomstab_syn_mw %>%
  filter(window_size == 7)

ndcorrsynL <- ggplot(mw7nondomsynstabfig, aes(x=meanloreau, y=mean_stability, color = timestep)) +
  geom_point() +
  ylab("Stability") + xlab("Loreau Synchrony: Non-dominant species") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


ggarrange(corrsynL, b5corrsynL, ndcorrsynL,
          ncol = 1, nrow = 3, align = "hv", 
          common.legend = TRUE, 
          labels = "AUTO",
          legend = "bottom"
)
#```





#syncorr_select <- syncorr_all %>%
# filter(window_size == "5" | window_size == "7" | window_size == "10") %>%
# filter(type == "all species")

#syncorrcoeff <- ggplot(syncorr_select, aes(x=TREATMENT, y=corr_coefficient, shape = significant)) +
#geom_point(size = 2) +
# geom_hline(yintercept = 0, linetype = "dashed") +
# ylab("Correlation Coefficent") + xlab("") +
# scale_color_manual(values = c("#E58606", "#5D69B1", "#52BCA3")) +
#  labs(col = "Type", shape = "Significance") +
# scale_shape_manual(values = c(12,0,15)) +
# facet_wrap(~window_size, ncol = 1) +
# theme(legend.title = element_text(size=11)) +  theme(text = element_text(size = 11))+
# theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  theme(legend.position = c(-1.2,-0.065), legend.box = "vertical", legend.direction = "horizontal")



#ggdraw() +
# draw_plot(synstabfinalcom, x = 0, y = 0.15, width = 0.72, height = 0.85) +
#draw_plot(syncorrcoeff, x = .5, y = .5, width = .5, height = .5) +
# draw_plot(syncorrcoeff, x = 0.7, y = 0, width = 0.3, height = 1) +
# draw_plot_label(label = c("A", "B"), size = 14,
#    x = c(0, 0.72), y = c(1, 1))
