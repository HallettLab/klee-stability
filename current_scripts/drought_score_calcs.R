## Classifying & Scoring Drought ##

## load packages
library(tidyverse)
library(lubridate)

source("precipitation_data_cleaning.R") ## read in data

## Classify Drought Years
## from total precipitation in the 12 months preceding sampling
ppt98_20$Monthnum <- as.character(ppt98_20$Monthnum)

## make sure the ppt data is in chronological order
ppt_forcalc <- ppt98_20 %>%
  arrange(Date_final) %>%
  filter(Date_final > "1998-06-15")  %>% ## filter out this June date so it's not included as a sampling period because this will mess with the for-loop.
  mutate(Monthnum = ifelse(Monthnum == "10", Monthnum, 
                           ifelse(Monthnum == "11", Monthnum, 
                                  ifelse(Monthnum == "12", Monthnum, 
                                         paste(0, Monthnum, sep = ""))))) %>% ## This line just adds a '0' before single digit month numbers to get it in a format that will work with the line below
  mutate(Date_numeric = paste(year(Date_final), Monthnum, day(Date_final), sep = "")) #create column that can be coerced into numeric form as lubridate date format does not seem to work well in for-loops. 

ppt_forcalc$Date_numeric <- as.numeric(ppt_forcalc$Date_numeric) ## make date numeric

## create dataframe of sampling dates, beginning in 1999
june <- ppt_forcalc %>%
  filter(Month %like% "un") %>% ## selecting June, but some are written June or Jun and the capitalization is not consistent
  select(Date_numeric) %>%
  arrange(Date_numeric)

sample_dates <- length(june$Date_numeric) ## calculate # of sample dates

## create dataframe of all July months - in order to bound the calculation period. Starts in 1998
july <- ppt_forcalc %>%
  filter(Month %like% "ul") %>% ## select July
  select(Date_numeric) %>%
  arrange(Date_numeric)

## create empty data frame to contain the output
preceding12 <- data.frame(preceding12ppt = NA, sample_date = NA)

for (i in 1:sample_dates){
  
  ## select correct 12 month period and sum ppt
  tempppt <- ppt_forcalc %>%
    filter(Date_numeric <= june[i,]) %>% ## filter out obs after sampling date
    filter(Date_numeric >= july[i,]) ## filter out obs before the previous sampling date -> leaves you with 12 months (11 preceding sample date and 1 including sample date)
  
  meanppt <- tempppt %>%
    summarise(preceding12ppt = sum(Average)) %>% ## sum the precipitation over these 12 months
    mutate(sample_date = june[i,])
  
  preceding12 <- rbind(preceding12, meanppt)
  
}

preceding12 <- preceding12[-1,] ## remove the row of NAs

## NEED TO CALCULATE SPECIAL VALUES FOR 1999 and 2003
    ## as there was missing data in these years. The 1999 survey takes place in Sept and the 2003 in Feb.

## since this was collected in Sept, I need to sample from Sep98 - Aug99
y99 <- ppt_forcalc %>%
  filter(Date_numeric >= 19980915) %>% 
  filter(Date_numeric <= 19990815) %>%
  summarise(preceding12ppt = sum(Average)) %>%
  mutate(sample_date = 19990915)

## since this was collected in Feb, I need to sample from Feb02 - Jan03
y03 <- ppt_forcalc %>%
  filter(Date_numeric <= 20030115, Date_numeric >= 20020215) %>%
  summarise(preceding12ppt = sum(Average)) %>%
  mutate(sample_date = 20030215)

## remove the incorrect 03 and 99 dates from this dataframe
prec12 <- preceding12 %>%
  filter(sample_date != 19990615, sample_date != 20030615)

## add the correct 03 and 99 values into the dataframe
x <- rbind(prec12, y99)
prec12_corrected <- rbind(x, y03)

rm(list = c("x", "prec12"))



## Classify Drought Events ##
drought_record <- prec12_corrected %>% 
  mutate(drought = ifelse(preceding12ppt <= quantile(prec12_corrected$preceding12ppt, probs = 0.25, na.rm = T), 1, 0))





# CALC DROUGHT PERCENTILE -> for severity function #
    ## severity = 1-((precip_percentile - 1)/25)

## calculate the percentiles from 0-100 of the precipitation record
quant <- data.frame(percentile = c(0:100), ppt_value = quantile(drought_record$preceding12ppt, seq(0, 1, .01))) %>%
  arrange(percentile) 
    ## since this includes 0th quantile, it won't fit well into the defined severity function
    ## need to coerce it into giving percentiles

## define a function to quantify the percentile that any given precipitation amount falls into from 1-100
ppt_per_func <- function (precip, q, ...) {
  
  for (i in 1:length(q$percentile)) {
    
    ## check if the ppt value is less than or equal to each percentile value
    if (precip <= q$ppt_value[i]) { 
      ## QUESTION: does the above line mean it cycles through every ppt_value until it finds one that 
      ## it is less than or equal to?
      
      ## the first percentile is technically called the 0th percentile by the quantile function, 
      ## so when a ppt value falls in the first quantile, we need to assign the index (1) 
      ## as the percentile rather than keeping the percentile from the quantile function
      if (i == 1) {
        percentile <- i
        
        ## for all other percentiles, i-1 should give the correct percentile
      }else{
        percentile <- i - 1
      }
      
      break()
      
    }
  }
  return(percentile)
}

## test the function
ppt_per_func(precip = 530, q = quant)
ppt_per_func(precip = 211.300, q = quant) ## calls this 1st %tile rather than 0th - good!


## Use the new ppt percentile function for every ppt value in the ppt record ##
nyears <- length(drought_record$preceding12ppt) ## calculate # of years to iterate over

## make an empty dataframe to contain the output
dperc_record <- data.frame(preceding12ppt = NA, sample_date = NA, drought = NA, percentile = NA)

## iterate over every year
for (j in 1:nyears) {
  
  ## select the particular ppt record
  ppt <- drought_record$preceding12ppt[j]
  
  ## filter out the correct row in drought_record
  dtemp <- drought_record %>%
    filter(preceding12ppt %in% ppt)
  
  ## use the function defined above to calculate the percentile
  percentile <- ppt_per_func(precip = ppt, q = quant)
  
  ## append this to the temp data frame in new column
  dtemp$percentile <- percentile
  
  ## append temp dataframe to output dataframe
  dperc_record <- rbind(dperc_record, dtemp)
  
}
dperc_record <- dperc_record[-1,] ## remove first row of NA values


# CALCULATE DROUGHT SEVERITY #
## drought severity = 1-((precip_percentile - 1)/25)
d_severity <- dperc_record %>%
  mutate(severity = ifelse(drought == "1", (1 - ((percentile - 1)/25)), 0)) ## divide by 25 as that's the # of divisions in quartile 1
## if it's not a drought, severity is set to 0
## 1 is the highest severity



# CALCULATE TOTAL DROUGHT SCORE IN EVERY WINDOW #
## first, identify years following droughts to eventually account for lag effects
d_sever_prev <- d_severity %>%
  arrange(sample_date) %>%
  mutate(drought_prev = ifelse(drought == 1, 0, 
                               ifelse(lag(drought, default = 0) == 1, 1, 0))) %>%
  mutate(severity_prev = lag(severity, default = 0)) ## add a column with the severity of previous year's drought to use in drought score function
  ## CODE EDIT: This doesn't appear as if it will affect the outcome, 
  ## but dataframe should be sorted by sample_date *before* calculating lag. 
  ## But see below, you may also need to update so that each year has a column 
  ## for severity of previous year, in order to correctly add 0.5*severity for lag years.




### CREATE Drought Scoring FUNCTION ###
dr_score_func <- function(input_data, timestep, ...) { 
  ## function inputs = data frame and number of time steps
  ## input_data should be d_sever_prev
  ## timestep is the window_size
  
  n_samples <- length(unique(input_data$sample_date)) ## calculate number of sampling points
  n_windows <- n_samples - timestep + 1  ## calculate number of windows to iterate over
  ## timestep here is referring to the window_size, this is poorly named
  sample_points <- sort(unique(input_data$sample_date)) ## create ordered list of sample points
  Dscore <- as.data.frame(matrix(nrow = n_windows, ncol = 4)) ## create dataframe to contain output
  colnames(Dscore) <- c("timestep", "Dscore", "num_drought", "contains_drought") ## rename columns
  
  ## set default values as 0's 
  Dscore$timestep <- c(1:n_windows)
  Dscore$contains_drought <- 0
  Dscore$Dscore <- 0
  Dscore$num_drought <- 0
  
  
  ## iterate over every WINDOW in the time series
  for (i in 1:n_windows){
    
    temp_samplepts <- sample_points[i:(i+timestep-1)] ## create a vector of sample points for each iteration
    
    temp <- input_data %>% ## filter the correct sample points from input data
      filter(sample_date %in% temp_samplepts)
    
    ## create a vector of index positions
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
        mutate(Dscore_indiv = ifelse(drought == "1", 1*severity, 
                                     ifelse(drought_prev == "1",
                                            0.5*severity_prev, 0))) %>%
        ## addressed code edit by adding column for severity of previous year's drought
        ## multiply this by 0.5 and it should now affect the final outcome.
        select(-preceding12ppt, -percentile, -severity, -drought_prev, - severity_prev)
      ## CODE EDIT: For a year in which there isn't a drought, but there was a previous-year drought, is this meant to give 0.5*(drought severity of previous year)? Currently, it's multiplying 0.5 by the severity of the year in question (not the previous year), which just gives 0.
      
      ## make a column for timestep
      tempDrID$timestep <- i
      
      ## combine with the empty dataframe to save outputs
      droughtIDs <- rbind(droughtIDs, tempDrID) %>%
        filter(!is.na(sample_date)) ## empty dataframe contains a row of NAs, remove this
      
    }
    
    ## sum the individual drought score of each year in the window so that every window has a final Drought Score
    meantempDrID <- droughtIDs %>%
      group_by(timestep) %>%
      summarise(Dscore = (sum(Dscore_indiv)/length(index_position)), num_drought = sum(drought)) %>%
      mutate(contains_drought = ifelse(Dscore > 0, 1, 0))
    
    ## if there is a drought in a window, attach that row into the empty drought_relevance dataframe
    if(meantempDrID$contains_drought == "1"){
      Dscore[i,] <- meantempDrID
    }
    
  }
  
  return(Dscore) ## retrieve data frame at the end
  
}

## calculate drought score over 10 year windows
dscore10 <- dr_score_func(input_data = d_sever_prev, timestep = 10)

## calculate drought score over 5 year windows
dscore5 <- dr_score_func(input_data = d_sever_prev, timestep = 5)

dscore12 <- dr_score_func(input_data = d_sever_prev, timestep = 12)

dscore7 <- dr_score_func(input_data = d_sever_prev, timestep = 7)

dscore15 <- dr_score_func(input_data = d_sever_prev, timestep = 15)


rm(list = c("d_sever_prev", "d_severity", "dperc_record", "dtemp", "june", "july", "meanppt", "ppt_forcalc",
            "ppt98_20", "pptmean", "pptrange", "preceding12", "quant", "tempppt", "prec12_corrected", "y03", "y99"))
