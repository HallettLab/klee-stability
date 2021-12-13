## Moving Window Calculations
  ## for stability & biotic mechanisms

## set working directory
setwd("~/Repositories/klee-stability")

## Load packages
library(tidyverse)
library(codyn)
library(tsvr)
library(lubridate)


## source cleaned data
source("current_scripts/klee_allyears_cleaning.R")


## Create Stability over a Moving Window Function
movingwindow_func <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$Date_final)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  movingwindow <- data.frame(matrix(ncol=3,nrow=(n_windows*length(unique(input_data$Unique_ID))))) ## create empty dataframe to contain output ## dimensions = # cols (3) x (uniqueID x timesteps)
  colnames(movingwindow) <- c("Unique_ID", "stability", "timestep")
  sample_points <- sort(unique(input_data$Date_numeric)) ## create ordered list of sample points
  
  n_plots <- length(unique(input_data$Unique_ID)) ## number of unique plots
  
  for (i in 1:n_windows){
    
    temp_samplepts <- sample_points[i:(i+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>%
      filter(Date_numeric %in% temp_samplepts) ## filter the correct sample points from input data to run through the community stability function
    
    temp_commstab <- community_stability(temp,  ## run the community stability function from codyn
                                         time.var = "Date_numeric", 
                                         abundance.var = "Pin_Hits", 
                                         replicate.var = "Unique_ID")
    temp_commstab$timestep <- i ## create a column for timestep in the temporary data frame
    
    movingwindow[((i-1)*n_plots + 1):(i*n_plots),] <- temp_commstab ## add stability calculations for one iteration to the moving window data frame
    ## ((i-1)*n_plots + 1):(i*n_plots) -> where to put each iteration of the loop
    
  }
  
  return(movingwindow) ## retrieve data frame at the end
  
}


## Apply Stability Moving Window Function
mw_size <- c(5:21) ## set range of sizes for the moving window

## create dataframe to contain output
mwstability <- data.frame(Unique_ID = NA, stability = NA, timestep = NA, window_size = NA)

## run the moving window function over the whole range of window sizes
for(i in 1:length(mw_size)){
  
  tmws <- movingwindow_func(input_data = klee_annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  mwstability <- rbind(mwstability, tmws)
  
}

mwstability <- mwstability[-1,] ## remove the first row of NA values

## join with treatment information 
stab_mw_tx <- left_join(mwstability, treats, by="Unique_ID") 

## calculate the mean stability for each treatment by window size
mean_mwstab <- left_join(mwstability, treats, by="Unique_ID") %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_stab = mean(stability), SE_stab = calcSE(stability))


## Create VR Moving Window Function
VRmw_func <- function(input_data, timestep, ...) { ## function inputs = data frame and the time step (window size)
  
  n_samples <- length(unique(input_data$Date_final)) ## number of sampling points
  n_windows <- n_samples - timestep + 1  ## number of windows to iterate over
  
  ## create empty dataframe to contain output
  mw_VR <- data.frame(matrix(ncol=3,nrow=(n_windows*length(unique(input_data$Unique_ID))))) 
  colnames(mw_VR) <- c("Unique_ID", "classicVR", "timestep") ## change column names
  
  sample_points <- sort(unique(input_data$Date_numeric)) ## create ordered list of sample points
  n_plots <- length(unique(input_data$Unique_ID)) ## number of unique plots
  plots <- unique(input_data$Unique_ID) ## unique plot identity

  ## iterate over every window
  for (k in 1:n_windows){
    
    ## create a vector of sample points for each iteration
    temp_samplepts <- sample_points[k:(k+timestep-1)] 
    
    # filter the correct sample points from input data 
    temp <- input_data %>%
      filter(Date_numeric %in% temp_samplepts) #
    
    outnames <- c("Unique_ID", "classicVR") ## column names
    
    ## create empty dataframe to hold output of next loop
    siteout <- as.data.frame(matrix(nrow = 0, ncol = 2))
    names(siteout) <- outnames ## change names in dataframe
    
    ## Need to loop through all individual plots
    for (i in 1:n_plots) {
      
      ## filter unique plot
      tplot <- subset(temp, Unique_ID == plots[i]) %>%
        tbl_df()
      
      ## select species and fill 0's
      tplot2 <- tplot %>%
        select(Date_numeric, SPECIES, Pin_Hits) %>%
        spread(SPECIES, Pin_Hits, fill = 0)
      
      ## transpose the data
      tdat <- t(as.matrix(tplot2[,2:dim(tplot2)[2]]))
      
      ## save plot & Tx info to append at the end
      VRplots <- tplot %>%
        select(Unique_ID) %>%
        unique()
      
      ## calculate the classic VR
      cvr <- vr(tdat)
      
      VRplots$classicVR <- cvr ## add vr into dataframe as column
      
      ## append to external dataframe
      siteout <- rbind(siteout, VRplots)
      
    }
    
    ## create a column for timestep in the temporary data frame
    siteout$timestep <- k 
    
    ## add VR calculations for one iteration to the moving window data frame
    mw_VR[((k-1)*n_plots + 1):(k*n_plots),] <- siteout 
    
  }
  return(mw_VR) ## retrieve data frame at the end
}


## Apply VR MW Function
mw_size <- c(5:21) ## set range of window sizes to use
## this function does not work for 4 years. It gives the error: "data should not contain species with constant abundance/density"

## create dataframe to contain output
mw_classicVR <- data.frame(Unique_ID = NA, classicVR = NA, timestep = NA, window_size = NA)

## run the moving window function over the whole range of window sizes
for(i in 1:length(mw_size)){
  
  tmwcVR <- VRmw_func(input_data = klee_annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  mw_classicVR <- rbind(mw_classicVR, tmwcVR)
  
}
mw_classicVR <- mw_classicVR[-1,]

## join with treatment information
cVR_mw_tx <- left_join(mw_classicVR, treats, by="Unique_ID")

## calculate the mean classic VR for each treatment at each window size
mean_mwcVR <- left_join(mw_classicVR, treats, by="Unique_ID") %>%
  group_by(TREATMENT, window_size, BLOCK) %>%
  summarise(mean_cVR = mean(classicVR), SE_cVR = calcSE(classicVR))



## Create Dominant Sp Pop Stability Moving Window Function
popstab_mwfunc <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$Date_final)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  n_sp_plots <- length(unique(input_data$Unique_ID))*length(unique(input_data$SPECIES)) ## number of rows that should come out depending on the number of species & treatments
  
  movingwindow <- data.frame(matrix(ncol=4,nrow=(n_windows*n_sp_plots))) ## create empty dataframe to contain output ## dimensions = # cols (4) x (Treatment x Species x timesteps)
  
  colnames(movingwindow) <- c("Unique_ID", "SPECIES", "popstability", "timestep")
  sample_points <- sort(unique(input_data$Date_numeric)) ## create ordered list of sample points
  
  for (i in 1:n_windows){
    
    temp_samplepts <- sample_points[i:(i+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>%
      filter(Date_numeric %in% temp_samplepts) %>% ## filter the correct sample points from input data to run through the community stability function
      group_by(Unique_ID, SPECIES) %>%
      summarise(temp_mean = mean(Pin_Hits), sdhits = sd(Pin_Hits), popstability = temp_mean/sdhits) %>%
      select(-temp_mean, -sdhits)
    
    temp$timestep <- i ## create a column for timestep in the temporary data frame
    
    movingwindow[((i-1)*n_sp_plots + 1):(i*n_sp_plots),] <- temp ## add stability calculations for one iteration to the moving window data frame
    
  }
  
  return(movingwindow) ## retrieve data frame at the end
  
}

## Apply Dominant species population stability MW Function
mw_size <- c(5:21) ## set range of window sizes to use

## create dataframe to contain output
mwspstab <- data.frame(Unique_ID = NA, SPECIES = NA, popstability = NA, timestep = NA, window_size = NA)

## run the moving window function over the whole range of window sizes
for(i in 1:length(mw_size)){
  
  psmw <- popstab_mwfunc(input_data = big5annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  mwspstab <- rbind(mwspstab, psmw)
  
}
mwspstab <- mwspstab[-1,]

## Calculate average species stability of the 5 dominant species for every Unique_ID
popst_mw_tx <- left_join(mwspstab, treats, by = "Unique_ID") %>%
  filter(!is.na(popstability), popstability != "Inf") %>% ## some occurrences of sp stability are NA or Inf
  group_by(TREATMENT, Unique_ID, timestep, window_size, BLOCK) %>%
  summarize(mean_popst = mean(popstability))

## Calculate average dom species stability by treatment
mean_mwpopst <-  left_join(mwspstab, treats, by = "Unique_ID") %>%
  group_by(TREATMENT, window_size) %>%
  summarise(mean_popst = mean(popstability), SE_popst = calcSE(popstability)) 



## Create Richness over a Moving Window Function
richnessmw_func <- function(input_data, timestep, ...) { ## function inputs = data frame and number of time steps
  
  n_samples <- length(unique(input_data$Date_final)) ## number of sampling points
  n_windows <- n_samples - timestep + 1 ## how many windows to iterate over 
  
  ## create empty dataframe to contain output
  movingwindow <- data.frame(TREATMENT = NA, Unique_ID = NA, richness = NA, timestep = NA)
  
  #colnames(movingwindow) <- c("TREATMENT", "timestep", "mean_dominance")
  sample_points <- sort(unique(input_data$Date_numeric)) ## create ordered list of sample points
  
  for (i in 1:n_windows){
    
    temp_samplepts <- sample_points[i:(i+timestep-1)] ## create a vector of sample points for each iteration
    
    temp_rich <- input_data %>%
      filter(Date_numeric %in% temp_samplepts) %>% ## filter the correct sample points from input data
      group_by(TREATMENT, Unique_ID, Date_numeric) %>%
      mutate(richness = n())
    
    temp_rich$timestep <- i ## create a column for time step in the temporary data frame
    
    mean_rich <- temp_rich %>%  ## calculate the average dominance over each time window for each plot
      group_by(TREATMENT, Unique_ID, timestep) %>%
      summarise(richness = mean(richness))
    
    movingwindow <- rbind(movingwindow, mean_rich)
    
    
  }
  
  return(movingwindow) ## retrieve data frame at the end
  
}

## Apply Richness MW Function
mw_size <- c(5:21) ## set range of window sizes to use

## create dataframe to contain output
mwrichness <- data.frame(TREATMENT = NA, Unique_ID = NA, richness = NA, timestep = NA, window_size = NA)

## run the moving window function over the whole range of window sizes
for(i in 1:length(mw_size)){
  
  tmwr <- richnessmw_func(input_data = klee_annual, timestep = mw_size[i]) %>%
    mutate(window_size = mw_size[i])
  
  mwrichness <- rbind(mwrichness, tmwr)
  
}

mwrichness <- mwrichness %>%
  filter(!is.na(TREATMENT))
## this contains NAs for some reason


## join with treatment information
rich_mw_tx <- left_join(mwrichness, treats, by = c("Unique_ID", "TREATMENT")) %>%
  filter(!is.na(TREATMENT))

## calculate the mean richness for each treatment at each window size
mean_mwrich <- left_join(mwrichness, treats, by = c("Unique_ID", "TREATMENT")) %>%
  group_by(TREATMENT, window_size, BLOCK) %>%
  summarise(mean_rich = mean(richness), SE_rich = calcSE(richness)) %>%
  filter(!is.na(TREATMENT))



## Join everything into one data frame ##
## select stability 5 & 10 year windows
stabmw5 <- stab_mw_tx %>%
  filter(window_size == "5")
stabmw10 <- stab_mw_tx %>%
  filter(window_size == "10")

## select classic VR 5 & 10 year windows
cvrmw5 <- cVR_mw_tx %>%
  filter(window_size == "5")
cvrmw10 <- cVR_mw_tx %>%
  filter(window_size == "10")

## select population stability 5 & 10 year windows
spstmw10 <- popst_mw_tx %>%
  filter(window_size == "10")
spstmw5 <- popst_mw_tx %>%
  filter(window_size == "5")

## select richness 5 & 10 year windows
richmw5 <- rich_mw_tx %>%
  filter(window_size == "5")
richmw10 <- rich_mw_tx %>%
  filter(window_size == "10")


jk <- left_join(stabmw10, cvrmw10, by = c("Unique_ID", "BLOCK", "TREATMENT", "timestep", "window_size"))
jk2 <- left_join(jk, spstmw10, by = c("Unique_ID", "BLOCK", "TREATMENT", "timestep", "window_size"))
mw10all <- left_join(jk2, richmw10, by = c("Unique_ID", "BLOCK", "TREATMENT", "timestep", "window_size")) 

jkl <- left_join(stabmw5, cvrmw5, by = c("Unique_ID", "BLOCK", "TREATMENT", "timestep", "window_size"))
jkl2 <- left_join(jkl, spstmw5, by = c("Unique_ID", "BLOCK", "TREATMENT", "timestep", "window_size"))
mw5all <- left_join(jkl2, richmw5, by = c("Unique_ID", "BLOCK", "TREATMENT", "timestep", "window_size")) 

## Clean up environment
rm(list = c("jk", "jk2", "nondom", "tmws", "totcov", "avg_biomass", "big5annual",
            "klee_annual", "klee_long", "mw_classicVR", "mwstability",
            "stab_mw_tx", "cVR_mw_tx", "cvrmw10", "cvrmw5",
            "tmwr", "tmwcVR", "stabmw5", "stabmw10", "spstmw5",
             "mwrichness", "mwspstab", "popst_mw_tx", "psmw",
            "rich_mw_tx", "richmw10", "richmw5", "spstmw10", "jkl", "jkl2"))
