## Data Cleaning for Full Time Series

library(tidyverse)
library(data.table)

klee_all <- read.csv("KLEE_First_Hits_LumpedSpp_1999-2020_HW.csv")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


## Data QAQC ##

## change year column names to numbers
names(klee_all) <- c("BLOCK", "TREATMENT", "SPECIES", "1999_06", "1999_09", "2000_02", "2000_06", "2001_01", "2001_06", "2002_02",
                     "2002_06", "2003_02", "2003_06", "2004_02", "2004_06", "2005_02", "2005_06", "2006_03", "2006_06", "2007_02", "2007_06",
                     "2008_02", "2008_06", "2009_02", "2009_06", "2010_01", "2010_06", "2011_06", "2012_06", "2013_06", "2014_06", "2015_06",
                     "2016_06", "2017_06", "2018_06", "2019_06", "2020_06")

## pivot to long format so that there is a column for date (year-month combo) and for abundance 
klee_all_long <- pivot_longer(klee_all, cols = "1999_06":"2020_06", names_to = "Date", values_to = "Pin_Hits")

dont_want <- c("Bare") #make a vector of species that are not wanted (so far only bare ground)

## create Unique ID for each block-plot combo and format date columns so they can fit in the lubridate package
klee_long <- klee_all_long %>%
  filter(Pin_Hits > 0, !is.na(Pin_Hits)) %>% #most observations are 0
  filter(!SPECIES %in% dont_want) %>% #filter out bare ground
  mutate(Unique_ID = paste(BLOCK, TREATMENT, sep = "_")) %>% #create unique ID for each plot
  mutate(day = "01") %>% #create a filler column with day value so that dates can be parsed
  mutate(Date_junk = paste(Date, day, sep = "_")) %>% #paste filler day column with year-month column
  mutate(Date_final = ymd(Date_junk)) %>% #change dates into recognized format
  mutate(Date_numeric = paste(year(Date_final), month(Date_final), day(Date_final), sep = "0")) %>% #create column that can be coerced into numeric form
  select(BLOCK:SPECIES, Pin_Hits:Unique_ID, Date_final:Date_numeric) #select columns, drop Date and Date_junk

## change Date_numeric to integer
klee_long$Date_numeric <- as.integer(klee_long$Date_numeric)

## Create an annual samplings only data frame
annual1 <- klee_long[klee_long$Date_final %like% "-06", ]        # Extract matching rows with %like%
annual2 <- klee_long[klee_long$Date_final %like% "-05", ]  

klee_annual <- rbind(annual1, annual2)


## Calculate Total Cover
totcov <- klee_annual %>%
  group_by(BLOCK, TREATMENT, Unique_ID, Date_final) %>% #group by block, treatment, and date 
  summarise(totcov = sum(Pin_Hits, na.rm=T)) #sum abundances to calculate tot cover

## Calculate the temporal mean total cover for each treatment
avg_biomass <- totcov %>%
  group_by(TREATMENT) %>% #group by treatment
  summarize(avgbio = mean(totcov), SEbio = calcSE(totcov)) #calculate mean and standard error of total cover

#reorder treatments to match grazing pressure.
avg_biomass$TREATMENT <- as.factor(avg_biomass$TREATMENT) #change treatment to factor
avg_biomass$TREATMENT <- factor(avg_biomass$TREATMENT, levels = c("O", "W",  "MW",  "C", "MWC", "WC"))


treats <- klee_annual %>%
  select(BLOCK, TREATMENT, Unique_ID) %>%
  unique()


## Create Big 5 and Other species data frames to run the analysis for each group

## big 5
big5 <- c("Brachiaria_lachnantha", "Pennisetum_stramineum", "Lintonia_nutans", "Themeda_triandra", "Pennisetum_mezianum")

big5annual <- klee_annual %>%
  filter(SPECIES %in% big5)


nondom <- klee_annual %>%
  filter(!SPECIES %in% big5)


## remove unneeded objects to keep environment clean
rm(list = c("klee_all", "klee_all_long", "dont_want", "annual1", "annual2"))
