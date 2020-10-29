###########################################################
### KLEE Data Cleaning
###########################################################

## Load packages
library("tidyverse")

source("google-drive-function.R")

## Read in Data
#total cover by time, years 1999 - 2013
totcov <- read_csv_gdrive("1nNVOMwaYY8GD9pYuLrXJro1aLR3RKn73") %>%
  tbl_df()

#rename columns
names(totcov) <- c("Block",  "Treatment", "Cattle",   "Mega",     "Meso",     "199909",   "200002",   "200006",   "200101",  
                   "200105",   "200202",   "200206",   "200302",   "200306",   "200402",   "200406",   "200502",   "200506",
                   "200603",   "200606",   "200702",   "200706",   "200802",   "200806",   "200902",   "200906",   "201001",
                   "201006",   "201106",   "201206",   "201306")

#change data to long format for ease of graphing?
totcov_long <- pivot_longer(totcov, cols = "199909":"201306", names_to = "Date", values_to = "Tot_Cover")