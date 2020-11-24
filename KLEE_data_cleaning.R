###########################################################
### KLEE Data Cleaning
###########################################################

#set working directory
setwd("~/Repositories/klee-stability")

## Load packages
library("tidyverse")
library("lubridate")

#get google drive function
source("google-drive-function.R")


################################################################################
### Proportional Cover Data ####################################################
################################################################################

#read in data (1999-2011)
#https://drive.google.com/file/d/19Ne9-8VrbuzyQitcjfVyg98P5WArDcfG/view?usp=sharing

j <- read_csv_gdrive("19Ne9-8VrbuzyQitcjfVyg98P5WArDcfG") %>%
  tbl_df()

prop_cov <- j[1:18,] #select only the total proportional cover - not indiv species - may go back to this data later

prop_cov <- as_tibble(prop_cov) #convert to tibble

#rename columns to remove 'X' from dates
names(prop_cov) <- c("Block", "Treatment", "Cattle", "Mega", "Meso", "199909", "200002", "200006", "200101", "200105", "200202",
                     "200206", "200302", "200306", "200402", "200406", "200502", "200506", "200603", "200606", "200702", "200706", 
                     "200802", "200806", "200902", "200906", "201001", "201006", "201106")

#change data to long format
propcov_long <- pivot_longer(prop_cov, cols = "199909":"201106", names_to = "Date", values_to = "Prop_Cover")

#remove unneeded objects to keep environment clean
rm(prop_cov)
rm(j)

################################################################################
### Existing Total Cover Data ##################################################
################################################################################
## Read in Data
#total cover by time, years 1999 - 2013
totcov <- read_csv_gdrive("1nNVOMwaYY8GD9pYuLrXJro1aLR3RKn73") %>%
  tbl_df()

#rename columns
names(totcov) <- c("Block",  "Treatment", "Cattle",   "Mega",     "Meso",     "199909",   "200002",   "200006",   "200101",  
                   "200105",   "200202",   "200206",   "200302",   "200306",   "200402",   "200406",   "200502",   "200506",
                   "200603",   "200606",   "200702",   "200706",   "200802",   "200806",   "200902",   "200906",   "201001",
                   "201006",   "201106",   "201206",   "201306")

#change data to long format 
totcov_long <- pivot_longer(totcov, cols = "199909":"201306", names_to = "Date", values_to = "Tot_Cover")


#create Unique ID for each block-plot combo and format date columns so they can fit in the lubridate package
tcov_long <- totcov_long %>%
  mutate(Unique_ID = paste(Block, Treatment, sep = "_")) %>% #create unique ID for each plot
  mutate(day = "01") %>% #create a filler column with day value so that dates can be parsed
  mutate(Date_junk = paste(Date, day, sep = "_")) %>% #paste filler day column with year-month column
  mutate(Date_final = ymd(Date_junk)) %>% #change dates into recognized format
  mutate(Date_numeric = paste(year(Date_final), month(Date_final), day(Date_final), sep = "0")) #create column that can be coerced into numeric form

#remove unneeded objects to keep environment clean
rm(totcov_long)
rm(totcov)

################################################################################
### First Hits to 2015, Master #################################################
################################################################################

#read in data, first hits (1999-2015), using lumped species data. Groupings determined by Kari, Lauren, & Corrinna 
#https://drive.google.com/file/d/10xNVrnnDDWbNBJM8nWLEm7keCMcJoXAG/view?usp=sharing

kleedat <- read_csv_gdrive("10xNVrnnDDWbNBJM8nWLEm7keCMcJoXAG") %>%
  tbl_df()

#change year names to numbers
names(kleedat) <- c("BLOCK", "TREATMENT", "SPECIES", "1999_01", "2000_02", "2000_06", "2001_01", "2001_05",  
                    "2002_02", "2002_06",  "2003_02",  "2004_02",  "2004_06",  "2005_02",  "2005_06",  "2006_03",
                    "2006_06",  "2007_02", "2007_06", "2008_02", "2008_06", "2009_02", "2009_06", "2010_01",  
                    "2010_06", "2011_06", "2012_06", "2013_06", "2014_06", "2015_06")


#pivot to longer format so that there is a column for date (year-month combo) and for abundance 
kleedat_long <- pivot_longer(kleedat, cols = "1999_01":"2015_06", names_to = "Date", values_to = "Pin_Hits")

dont_want <- c("bare") #make a vector of species that are not wanted (so far only bare ground)

#create Unique ID for each block-plot combo and format date columns so they can fit in the lubridate package
klee_long <- kleedat_long %>%
  filter(Pin_Hits > 0, !is.na(Pin_Hits)) %>% #most observations are 0
  filter(!SPECIES %in% dont_want) %>% #filter out bare ground
  mutate(Unique_ID = paste(BLOCK, TREATMENT, sep = "_")) %>% #create unique ID for each plot
  mutate(day = "01") %>% #create a filler column with day value so that dates can be parsed
  mutate(Date_junk = paste(Date, day, sep = "_")) %>% #paste filler day column with year-month column
  mutate(Date_final = ymd(Date_junk)) %>% #change dates into recognized format
  mutate(Date_numeric = paste(year(Date_final), month(Date_final), day(Date_final), sep = "0")) %>% #create column that can be coerced into numeric form
  select(BLOCK:SPECIES, Pin_Hits:Unique_ID, Date_final:Date_numeric) #select columns, drop Date and Date_junk

#change Date_numeric to integer
klee_long$Date_numeric <- as.integer(klee_long$Date_numeric)


#remove unneeded objects to keep environment clean
rm(kleedat)
rm(kleedat_long)
rm(dont_want)


################################################################################
### Create a Treatment Data Frame ##############################################
################################################################################

#make a dataframe with treatment data. key = Unique_ID
treats <- klee_long %>%
  select(BLOCK, TREATMENT, Unique_ID) %>%
  unique()




