

library(tidyverse)
library(lubridate)

source("google-drive-function.R")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


## read in data, first hits (1999-2015), using lumped species data. Groupings determined by Kari, Lauren, & Corrina 
#https://drive.google.com/file/d/10xNVrnnDDWbNBJM8nWLEm7keCMcJoXAG/view?usp=sharing
kleedat <- read_csv_gdrive("10xNVrnnDDWbNBJM8nWLEm7keCMcJoXAG") %>%
  tbl_df()



## Data QAQC ##

## change year column names to numbers
names(kleedat) <- c("BLOCK", "TREATMENT", "SPECIES", "1999_01", "2000_02", "2000_06", "2001_01", "2001_05",  
                    "2002_02", "2002_06",  "2003_02",  "2004_02",  "2004_06",  "2005_02",  "2005_06",  "2006_03",
                    "2006_06",  "2007_02", "2007_06", "2008_02", "2008_06", "2009_02", "2009_06", "2010_01",  
                    "2010_06", "2011_06", "2012_06", "2013_06", "2014_06", "2015_06")


## pivot to long format so that there is a column for date (year-month combo) and for abundance 
kleedat_long <- pivot_longer(kleedat, cols = "1999_01":"2015_06", names_to = "Date", values_to = "Pin_Hits")

dont_want <- c("bare") #make a vector of species that are not wanted (so far only bare ground)

## create Unique ID for each block-plot combo and format date columns so they can fit in the lubridate package
klee_long <- kleedat_long %>%
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

## make a separate data frame with only treatment data. key = Unique_ID
treats <- klee_long %>%
  select(BLOCK, TREATMENT, Unique_ID) %>%
  unique()


## Calculate Total Cover
## Calculate Total Cover 1999-2015 ##
klee_totcov <- klee_long %>%
  group_by(BLOCK, TREATMENT, Unique_ID, Date_final) %>% #group by block, treatment, and date 
  summarise(totcov = sum(Pin_Hits, na.rm=T)) #sum abundances to calculate tot cover

## Calculate the mean total cover for each treatment & time point
meantotcov <- klee_totcov %>%
  group_by(TREATMENT, Unique_ID, Date_final) %>% #group by treatment & date
  summarize(Mean_TotCov = mean(totcov)) #calculate the mean total cover at each treatment & date

## Calculate the temporal mean total cover for each treatment
avg_biomass <- meantotcov %>%
  group_by(TREATMENT) %>% #group by treatment
  summarize(avgbio = mean(Mean_TotCov), SEbio = calcSE(Mean_TotCov)) #calculate mean and standard error of total cover

#reorder treatments to match grazing pressure.
avg_biomass$TREATMENT <- as.factor(avg_biomass$TREATMENT) #change treatment to factor
avg_biomass$TREATMENT <- factor(avg_biomass$TREATMENT, levels = c("0", "W",  "MW",  "C", "MWC", "WC"))


## Create an annual samplings only data frame
annual1 <- klee_long[klee_long$Date_final %like% "-06", ]        # Extract matching rows with %like%
annual2 <- klee_long[klee_long$Date_final %like% "-05", ]  

annual_all <- rbind(annual1, annual2)


## remove unneeded objects to keep environment clean
rm(list = c("kleedat", "kleedat_long", "dont_want", "annual1", "annual2"))
