# OLD
## Classify Drought Relevance in a Window
#```{r, echo=FALSE}
### CREATE FUNCTION ###
## create drought MW ID function
droughtrelIDfunc <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  ## number of sampling points
  n_samples <- length(unique(input_data$sample_date)) 
  
  ## number of windows to iterate over
  n_windows <- n_samples - timestep + 1  
  
  ## create ordered list of sample points
  sample_points <- sort(unique(input_data$sample_date)) 
  
  ## create dataframe to contain output
  drought_relevance <- as.data.frame(matrix(nrow = n_windows, ncol = 4))
  
  ## rename columns
  colnames(drought_relevance) <- c("timestep", "Dscore", "num_drought", "contains_drought")
  
  ## set default values of drought_flags 
  drought_relevance$timestep <- c(1:n_windows)
  drought_relevance$contains_drought <- 0
  drought_relevance$Dscore <- 0
  drought_relevance$num_drought <- 0
  
  ## iterate over every window in the time series
  for (i in 1:n_windows){
    
    ## create a vector of sample points for each iteration
    temp_samplepts <- sample_points[i:(i+timestep-1)] 
    
    ## filter the correct sample points from input data
    temp <- input_data %>%
      filter(sample_date %in% temp_samplepts) #%>%
    #select(-tot4moppt, -meanppt, -SEppt, -season, -day)
    
    ## set default value of drought in window to FALSE
    # drought_in_window <- FALSE
    
    index_position  <- unique(temp$sample_date)
    
    ## create empty dataframe to contain output of second loop
    droughtIDs <- data.frame(sample_date = NA, Dscore_indiv = NA, drought = NA, timestep = NA)
    
    ## iterate through each YEAR in the window
    for (j in 1:length(index_position)){
      
      ## need to separate each date first
      date <- index_position[j]
      
      ## identify & score individual droughts in a window
      tempDrID <- temp %>%
        filter(sample_date %in% date) %>%
        #group_by(Year) %>%
        #summarise(numdroughts = sum(drought)) %>%
        mutate(Dscore_indiv = ifelse(drought == "1", 1/j, 0)) %>% ## score by drought position in the window
        select(-preceding12ppt)
      ## by scoring a drought as 1/position #, a drought early in the window will be scored greater than a drought at the end of the window
      
      ## make a column for timestep
      tempDrID$timestep <- i
      
      ## combine with the empty dataframe to save outputs
      droughtIDs <- rbind(droughtIDs, tempDrID) %>%
        filter(!is.na(sample_date)) ## empty dataframe contains a row of NAs -> use this to remove that row
      
    }
    
    ## sum the individual drought score of each year in the window so that every window has a final Drought Score
    meantempDrID <- droughtIDs %>%
      group_by(timestep) %>%
      summarise(Dscore = sum(Dscore_indiv), num_drought = sum(drought)) %>%
      mutate(contains_drought = ifelse(Dscore > 0, 1, 0))
    
    ## if there is a drought in a window, attach that row into the empty drought_relevance dataframe
    if(meantempDrID$contains_drought == "1"){
      drought_relevance[i,] <- meantempDrID
    }
    
  }
  
  return(drought_relevance) ## retrieve data frame at the end
  
}

## make a function to calculate the maximum Dscore for a given windowsize in order to normalize all Dscores
getmaxDscore <- function(num_years, ...) {
  
  maxDscore <- 0
  
  for(i in 1:num_years){
    maxDscore <- maxDscore + (1/i)
  }
  
  return(maxDscore)
  
}

## test
getmaxDscore(5)
#```

## Apply Drought Relevance Function and Plot
#```{r, echo=FALSE}
## use the function with 3 year window
droughtrel3 <- droughtrelIDfunc(input_data = drought_record, timestep = 3) %>%
  mutate(normDscore = Dscore/getmaxDscore(3))

stabmw3 <- stab_mw_tx %>%
  filter(window_size == "3")

stdr3 <- left_join(stabmw3, droughtrel3, by = "timestep")

ggplot(stdr3, aes(x=timestep, y=mean_stability, color = normDscore)) +
  geom_line() +
  geom_point() +
  facet_wrap(~TREATMENT)

## try with 5 year window
droughtrel5 <- droughtrelIDfunc(input_data = drought_record, timestep = 5) %>%
  mutate(normDscore = Dscore/getmaxDscore(5))

stabmw5 <- stab_mw_tx %>%
  filter(window_size == "5")

stdr5 <- left_join(stabmw5, droughtrel5, by = "timestep")

ggplot(stdr5, aes(x=timestep, y=mean_stability, color = normDscore, shape = TREATMENT)) +
  geom_line() +
  geom_point() 

## try with 10 year window
droughtrel10 <- droughtrelIDfunc(input_data = drought_record, timestep = 10) %>%
  mutate(normDscore = Dscore/getmaxDscore(10))

stabmw10 <- stab_mw_tx %>%
  filter(window_size == "10")



ggplot(stdr10, aes(x=timestep, y=mean_stability, color = normDscore, shape = TREATMENT)) +
  geom_line(color = "black") +
  geom_point(size = 3.5) +
  scale_shape_manual(values = c(16,25,15,12,23,17)) +
  ylab("Stability") + xlab("Window Number")+
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) 

## cVR and drought 
cvrmw10 <- cVR_mw_tx %>%
  filter(window_size == "10")
cvrdr10 <- left_join(cvrmw10, droughtrel10, by = "timestep")

cvrmw5 <- cVR_mw_tx %>%
  filter(window_size == "5")
cvrdr5 <- left_join(cvrmw5, droughtrel5, by = "timestep")

## richness and drought
richmw10 <- rich_mw_tx %>%
  filter(window_size == "10")

richdr10 <- left_join(richmw10, droughtrel10, by = "timestep")


## species stability and drought
spstmw10 <- popst_mw_tx %>%
  filter(window_size == "10")
popstdr10 <- left_join(spstmw10, droughtrel10, by = "timestep")

spstmw5 <- popst_mw_tx %>%
  filter(window_size == "5")
popstdr5 <- left_join(spstmw5, droughtrel5, by = "timestep")
#```

## Synchrony-Stability-Drought temp dynamics figure
#```{r, fig.height=9, fig.width=8}
## make a vector of all sample dates to use for axis labels
dates <- sort(unique(klee_annual[['Date_final']]))
window10dates <- year(dates[1:13])

breaks <- c(1:13)

stabdrought10 <- ggplot(stdr10, aes(x=timestep, y=mean_stability, shape = TREATMENT)) +
  geom_line(color = "black") +
  geom_point() +
  geom_point(aes(fill = normDscore, size = TREATMENT), colour = "black") +
  scale_shape_manual(values = c(21,22,24,23,25,21)) +
  scale_size_manual(values=c(4,3.5,3.5,3.5,3.5,2.5)) +
  ylab("Stability") + xlab("Window Starting Year") +
  scale_fill_scico(palette = "lajolla") +
  #scale_color_scico(palette = "lajolla") +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none")

cvrdrought10 <- ggplot(cvrdr10, aes(x=timestep, y=mean_cVR, shape = TREATMENT)) +
  geom_line(color = "black") +
  geom_point() +
  geom_point(aes(fill = normDscore, size = TREATMENT), colour = "black") +
  scale_shape_manual(values = c(21,22,24,23,25,21)) +
  scale_size_manual(values=c(4,3.5,3.5,3.5,3.5,2.5)) +
  ylab("Classic Variance Ratio") + xlab("Window Starting Year")+
  scale_fill_scico(palette = "lajolla") +
  #scale_color_scico(palette = "lajolla") +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) +
  geom_hline(yintercept = 1, linetype = "dashed")


spstabdrought10 <- ggplot(popstdr10, aes(x=timestep, y=avgpopstab, shape = TREATMENT)) +
  geom_line(color = "black") +
  geom_point() +
  geom_point(aes(fill = normDscore, size = TREATMENT), colour = "black") +
  scale_shape_manual(values = c(21,22,24,23,25,21)) +
  scale_size_manual(values=c(4,3.5,3.5,3.5,3.5,2.5)) +
  ylab("Mean Population Stability") + xlab("Window Starting Year")+
  scale_fill_scico(palette = "lajolla") +
  #scale_color_scico(palette = "lajolla") +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) #+
#geom_hline(yintercept = 1, linetype = "dashed")



forlegend <- ggplot(stdr10, aes(x=timestep, y=mean_stability, shape = TREATMENT)) +
  geom_line(color = "black") +
  #geom_point() +
  geom_point(aes(fill = normDscore, size = TREATMENT), colour = "black") +
  scale_shape_manual(values = c(21,22,24,23,25,21)) +
  scale_size_manual(values=c(4,3.5,3.5,3.5,3.5,2.5)) +
  ylab("Stability") + xlab("") +
  scale_fill_scico(palette = "lajolla") +
  #scale_color_scico(palette = "lajolla") +
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))# +
# guides(size = guide_legend("none"))

droughtlegend <- get_legend(
  forlegend + theme(legend.box.margin = margin(0,0,0,8))
)

top <- plot_grid(stabdrought10, cvrdrought10, spstabdrought10, ncol = 1, align = "v")
plot_grid(top, droughtlegend, rel_widths = c(6,1))
#```

## Drought Score colored background (Stability & All Mech MW)
#```{r, fig.height=8.5, fig.width=10}
dates <- sort(unique(klee_annual[['Date_final']]))
window5dates <- year(dates[1:18])
breaks18 <- c(1:18)


stabdr10 <- ggplot(stdr10, aes(x=timestep, y=mean_stability, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = normDscore), size = 14) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 4) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.7) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab("Stability") + labs(fill = "Treatment") + xlab("") +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))



cvarrdr10 <- ggplot(cvrdr10, aes(x=timestep, y=mean_cVR, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = normDscore), size = 14) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 4) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.7) +
  ylab("Classic Variance Ratio") + xlab("")+
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) +
  geom_hline(yintercept = 1, linetype = "dashed")  +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


sppopstabdr10 <- ggplot(popstdr10, aes(x=timestep, y=avgpopstab, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = normDscore), size = 14) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 4) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.7) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Mean Population Stability") + xlab("Starting Year (10yr Window)")+
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks, labels = window10dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))



stabdr5 <- ggplot(stdr5, aes(x=timestep, y=mean_stability, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = normDscore), size = 14) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 4) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.7) +
  scale_x_continuous(breaks = breaks18, labels = window5dates) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab("Stability") + labs(fill = "Treatment") + xlab("") +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


cvarrdr5 <- ggplot(cvrdr5, aes(x=timestep, y=mean_cVR, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = normDscore), size = 14) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 4) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.7) +
  ylab("Classic Variance Ratio") + xlab("")+
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  scale_x_continuous(breaks = breaks18, labels = window5dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) +
  geom_hline(yintercept = 1, linetype = "dashed")  +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


sppopstabdr5 <- ggplot(popstdr5, aes(x=timestep, y=avgpopstab, fill = TREATMENT)) +
  geom_vline(mapping = aes(xintercept = timestep, color = normDscore), size = 14) +
  geom_line(size = 0.75) +
  geom_point(aes(fill = TREATMENT),
             colour = "black", pch = 21, size = 4) +
  scale_color_scico(palette = "grayC", begin = 0.1, end = 0.7) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  ylab("Mean Population Stability") + xlab("Starting Year (5yr Window)")+
  theme(legend.title = element_text(size=12)) +  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = breaks18, labels = window5dates) +
  theme(legend.position = "none") +
  guides(fill = guide_legend("none")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.title = element_text(size=14)) +  theme(text = element_text(size = 14))


ggarrange(stabdr5, stabdr10, 
          cvarrdr5, cvarrdr10,
          sppopstabdr5, sppopstabdr10, 
          ncol = 2, 
          nrow = 3, 
          common.legend = TRUE, 
          legend = "bottom")
#```

## another correlation fig
#```{r}
corrVR <- ggplot(corrfigVRstdr10, aes(x=mean_cVR, y=mean_stability)) +
  geom_point() +
  ylab("Stability (10yr)") + xlab("Classic Variance Ratio (10yr)") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=11)) +  theme(text = element_text(size = 11)) + 
  coord_cartesian(xlim = c(0,4)) +
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=normDscore), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "D Score") +
  theme(legend.position = "right")



corrVR5 <- ggplot(corrfigVRstdr5, aes(x=mean_cVR, y=mean_stability)) +
  geom_point() +
  ylab("Stability (5yr)") + xlab("Classic Variance Ratio (5yr)") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=11)) +  theme(text = element_text(size = 11)) + 
  coord_cartesian(xlim = c(0,4)) +
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=normDscore), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "D Score") +
  theme(legend.position = "right")


ggarrange(corrVR5, corrVR, 
          ncol = 1, 
          common.legend = TRUE, 
          legend = "bottom", 
          labels = "AUTO")




mw10popststabfig <- stab_popst_mw %>%
  filter(window_size == "10")

corrfigpopststdr <- left_join(stdr10, mw10popststabfig, by = c("TREATMENT", "timestep", "window_size", "mean_stability", "SE"))


mw5popststabfig <- stab_popst_mw %>%
  filter(window_size == "5")

corrfigpopststdr5 <- left_join(stdr5, mw5popststabfig, by = c("TREATMENT", "timestep", "window_size", "mean_stability", "SE"))

corrpstab <- ggplot(corrfigpopststdr, aes(x=avgpopstab, y=mean_stability)) +
  geom_point() +
  ylab("Stability") + xlab("Population Stability") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=11)) +  theme(text = element_text(size = 11)) + 
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=normDscore), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "D Score") +
  theme(legend.position = "right")


corrpstab5 <- ggplot(corrfigpopststdr5, aes(x=avgpopstab, y=mean_stability)) +
  geom_point() +
  ylab("Stability") + xlab("Population Stability") +
  facet_wrap(~TREATMENT, ncol = 6) +
  theme(legend.title = element_text(size=11)) +  theme(text = element_text(size = 11)) + 
  geom_smooth(method = "lm", color = "black", fullrange = T, se = FALSE) +
  geom_point(aes(fill=normDscore), 
             colour="black",pch=21, size=2.5) +
  scale_fill_scico(palette = "lajolla") +
  labs(fill = "D Score") +
  theme(legend.position = "right")

ggarrange(corrpstab5, corrpstab, ncol = 1, common.legend = TRUE, legend = "bottom")
#```

## Regress Drought Score with Stability & VR
#```{r}

ggplot(corrfigVRstdr10, aes(x= normDscore, y=mean_stability)) +
  geom_point() +
  #facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(corrfigVRstdr10, aes(x= normDscore, y=mean_cVR)) +
  geom_point() +
  #facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(corrfigVRstdr5, aes(x= normDscore, y=mean_stability)) +
  geom_point() +
  #facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

ggplot(corrfigVRstdr5, aes(x= normDscore, y=mean_cVR)) +
  geom_point() +
  #facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")



ggplot(corrfigpopststdr, aes(x= normDscore, y=avgpopstab)) +
  geom_point() +
  #facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")

#```

## Calculate the change after each window to associate with drought score
#```{r}
## calculate the change in stability from one timestep to the next
dStab10 <- stdr10 %>%
  group_by(TREATMENT) %>%
  #arrange(timestep) %>%
  mutate(stability_diff = mean_stability - lead(mean_stability, default = NA, order_by = timestep))


ggplot(dStab10, aes(x=normDscore, y=stability_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")


## calculate the change in stability from one timestep to the next
dStab5 <- stdr5 %>%
  group_by(TREATMENT) %>%
  #arrange(timestep) %>%
  mutate(stability_diff = mean_stability - lead(mean_stability, default = NA, order_by = timestep))


ggplot(dStab5, aes(x=normDscore, y=stability_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")







## calculate the change in the variance ratio from one timestep to the next
dCVR10 <- cvrdr10 %>%
  group_by(TREATMENT) %>%
  arrange(timestep) %>%
  mutate(cVR_diff = mean_cVR - lead(mean_cVR, default = NA))


ggplot(dCVR10, aes(x=normDscore, y=cVR_diff)) +
  geom_point() +
  facet_wrap(~TREATMENT) +
  geom_smooth(method = "lm")
#```

## An attempt at adding herbivore graphics to MW figure
#```{r, echo=FALSE}
library(png)
library(patchwork)


filler <- stdr10 %>%
  mutate(drposition = 2, timestep_shifted = (timestep - 0.5))

Otrt <- readPNG("O_trt.png", native = TRUE)
Wtrt <- readPNG("W_trt.png", native = TRUE)
MWtrt <- readPNG("MW_trt.png", native = TRUE)
Ctrt <- readPNG("C_trt.png", native = TRUE)
WCtrt <- readPNG("WC_trt.png", native = TRUE)
MWCtrt <- readPNG("MWC_trt.png", native = TRUE)

test <- ggplot(filler, aes(x=timestep, y=mean_stability)) +
  geom_line(aes(linetype = TREATMENT, color = TREATMENT), size = 1) +
  geom_point(aes(fill = normDscore),
             colour = "black", pch = 21, size = 3.25) +
  #geom_point() +
  #scale_color_scico_d(palette = "devon", direction = -1) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_linetype_manual(values = c(1,2,3,4,6,5)) +
  scale_color_manual(values = c("#000000", "#000000", "#000000", "#4f4f4f", "#4f4f4f", "#4f4f4f")) +
  ylab("Stability") + labs(fill = "Treatment") +
  geom_point(aes(y = drposition, fill = normDscore), size = 8, shape = 22, colour = "black") +
  scale_fill_scico(palette = "lajolla") +
  coord_cartesian(xlim = c(0,13)) #+
#geom_line(aes(x=timestep_shifted, y = drposition, color = normDscore), size = 8) +
# scale_color_scico(palette = "lajolla")

test +
  inset_element(p = Otrt,
                left = 0.01, 
                bottom = 0.53,
                right = 0.1,
                top = 0.58) +
  inset_element(p = Wtrt,
                left = 0.01, 
                bottom = 0.44,
                right = 0.11,
                top = 0.49) +
  inset_element(p = MWtrt,
                left = 0.01, 
                bottom = 0.29,
                right = 0.1,
                top = 0.34) +
  inset_element(p = Ctrt,
                left = 0.01, 
                bottom = 0.34,
                right = 0.1,
                top = 0.39) +
  inset_element(p = WCtrt,
                left = 0.01, 
                bottom = 0.2,
                right = 0.1,
                top = 0.24) +
  inset_element(p = MWCtrt,
                left = 0.01, 
                bottom = 0.24,
                right = 0.1,
                top = 0.29)


#```
