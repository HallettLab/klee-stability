library(tidyverse)
library(lubridate)
library(data.table)
#library(SPEI)

## read in data
ppt <- read.csv("monthly_ppt.csv")
early_ppt <- read.csv("KLEE_rainfall_1996_2012_monthly.csv")

## create a function to calculate standard error
calcSE<-function(x){
  x2<-na.omit(x)
  sd(x2)/sqrt(length(x2))
}


## Clean up 1996-2012 precipitation data ##
colnames(early_ppt) <- c("Month", "North", "Central", "South", "Average", "Notes")

## Extract 1999-2002 monthly precipitation to fill out time series
ppt98 <- early_ppt[early_ppt$Month %like% "-98", ]
ppt99 <- early_ppt[early_ppt$Month %like% "-99", ]   # Extract matching rows with %like%
ppt00 <- early_ppt[early_ppt$Month %like% "-00", ]   # Extract matching rows with %like%
ppt01 <- early_ppt[early_ppt$Month %like% "-01", ]   # Extract matching rows with %like%
ppt02 <- early_ppt[early_ppt$Month %like% "-02", ]   # Extract matching rows with %like%

jx <- rbind(ppt98, ppt99)
jppt <- rbind(jx, ppt00)
jjppt <- rbind(jppt, ppt01)
ppt98_02 <- rbind(jjppt, ppt02) %>%
  select(Month, Average, Notes) %>%
  separate(Month, into = c("Month", "Year"), sep = "-")
  ## almost ready to combine with 2003-2020 data


## Clean up 2003 - 2020 precipitation data ##
## rename columns
colnames(ppt) <- c("Month", "North", "Central", "South", "Average", "std.error")

## Get this ready for combining with 99-02 data
ppt2 <- ppt %>%
  select(Month, Average) %>%
  separate(Month, into = c("Month", "Year"), sep = " ") %>%
  mutate(Notes = NA)

## combine dataframes
ppt98_20 <- rbind(ppt2, ppt98_02) %>%
  mutate(Day = 15) %>% ## just add in a random day value, because I think the lubridate package needs that
  mutate(Date_temp = paste(Year, Month, sep = "_")) %>%
  mutate(Date_temp2 = paste(Date_temp, Day, sep = "_")) %>%
  mutate(Date_final = ymd(Date_temp2)) %>% #change dates into recognized format
  select(-Day, -Date_temp, -Date_temp2, -Year) %>%
  mutate(Year = year(Date_final), Monthnum = month(Date_final), Day = day(Date_final))


## SITE INFO FOR METHODS ##
## Calculate the average annual ppt to obtain range of MAP
pptrange <- ppt98_20 %>%
  group_by(Year) %>%
  summarise(annppt = sum(Average)) %>%
  arrange(annppt)

## Calculate the mean and standard error of annual precip
pptmean <- pptrange %>%
  summarise(meanall = mean(annppt, na.rm = T), SEall = calcSE(annppt))


## clean up workspace
rm(list = c("jx", "jppt", "jjppt", "ppt2", "ppt", "early_ppt",
            "ppt98", "ppt99", "ppt00", "ppt01", "ppt02"))
