setwd("~/Repositories/klee-stability")

library(tidyverse)
library(lubridate)
library(data.table)
library(SPEI)

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
ppt99 <- early_ppt[early_ppt$Month %like% "-99", ]   # Extract matching rows with %like%
ppt00 <- early_ppt[early_ppt$Month %like% "-00", ]   # Extract matching rows with %like%
ppt01 <- early_ppt[early_ppt$Month %like% "-01", ]   # Extract matching rows with %like%
ppt02 <- early_ppt[early_ppt$Month %like% "-02", ]   # Extract matching rows with %like%

jppt <- rbind(ppt99, ppt00)
jjppt <- rbind(jppt, ppt01)
ppt99_02 <- rbind(jjppt, ppt02) %>%
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
ppt99_20 <- rbind(ppt2, ppt99_02) %>%
  mutate(Day = 01) %>% ## just add in a random day value, because I think the lubridate package needs that
  mutate(Date_temp = paste(Year, Month, sep = "_")) %>%
  mutate(Date_temp2 = paste(Date_temp, Day, sep = "_")) %>%
  mutate(Date_final = ymd(Date_temp2)) %>% #change dates into recognized format
  select(-Day, -Date_temp, -Date_temp2, -Year) %>%
  mutate(Year = year(Date_final))

spi_dat <- ppt99_20 %>%
  mutate(Month = month(Date_final), Precip = Average) %>%
  select(-Average, -Notes, -Date_final) %>%
  filter(!(Year == "2020" & Month > 7)) %>%
  arrange(Year)
  
## try switching to time series object for convenience
spi_ts <- ts(spi_dat,  end(c(2020,7)), frequency = 12)



## Try Defining the SPI Index
spi1 <- spi(data = spi_dat, scale = 1)
spi1.v2 <- spi(data = spi_ts[, 'Precip'], scale = 1)
summary(spi1.v2)

names(spi1.v2)


spi1_fitted <- data.frame(spi1.v2$fitted) %>%
  mutate(Year = spi_dat$Year, Month = spi_dat$Month)



#spi1.v2$coefficients

spi4 <- spi(data = spi_dat, scale = 4)
spi6 <- spi(data = spi_dat, scale = 6)
spi12 <- spi(data = spi_dat, scale = 12)

plot(spi1)
plot(spi4)
plot(spi6)
plot(spi12)

plot(spi1.v2)

## DEFINING DRY AND WET PERIODS ##
## from Riginos 2018: Drought = rainfall in the bottom 25th percentile of all sample periods; less than 120 mm in the four-month rainy season preceding sampling

## need total rainfall in each month? have the average of the three blocks, 
## so if I add the averages of the last four months that should give the correct definition of drought


## filter rainy seasons 
## should change this to be the 4 months prior to the sampling month...
## September-December
sepdec <- ppt99_20 %>%
  filter(Date_final %like% "-09" | Date_final %like% "-10" | Date_final %like% "-11" | Date_final %like% "-12") %>%
  group_by(Year) %>%
  summarize(tot4moppt = sum(Average), meanppt = mean(Average), SEppt = calcSE(Average)) %>%
  mutate(season = "sepdec")

## February to May
febmay <- ppt99_20 %>%
  filter(Date_final %like% "-02"| Date_final %like% "-03" | Date_final %like% "-04" | Date_final %like% "-05") %>%
  group_by(Year) %>%
  summarize(tot4moppt = sum(Average), meanppt = mean(Average), SEppt = calcSE(Average)) %>%
  mutate(season = "febmay")


## Calculate the 25th percentile for precipitation to identify dry events
drysepdec <- sepdec %>%
  filter(meanppt < quantile(sepdec$meanppt, probs = .25, na.rm = TRUE)) %>%
  mutate(condition = "dry")

altdrysepdec <- sepdec %>%
  filter(tot4moppt < quantile(sepdec$tot4moppt, probs = .25, na.rm = TRUE)) %>%
  mutate(condition = "dry")


dryfebmay <- febmay %>%
  filter(meanppt < quantile(febmay$meanppt, probs = .25, na.rm = TRUE)) %>%
  mutate(condition = "dry")

altdryfebmay <- febmay %>%
  filter(tot4moppt < quantile(febmay$tot4moppt, probs = .25, na.rm = TRUE)) %>%
  mutate(condition = "dry")


## Calculate precipitation above 75th percentile to ID wet events (in 4th quantile)
wetsepdec <- sepdec %>%
  filter(meanppt > quantile(sepdec$meanppt, probs = .75, na.rm = TRUE)) %>%
  mutate(condition = "wet")

wetfebmay <- febmay %>%
  filter(meanppt > quantile(febmay$meanppt, probs = .75, na.rm = TRUE)) %>%
  mutate(condition = "wet")


altdry <- rbind(altdryfebmay, altdrysepdec)

## Rejoin dry events with full time series
extremes_sd <- rbind(drysepdec, wetsepdec)
extremes_fm <- rbind(dryfebmay, wetfebmay)

tempsd <- left_join(sepdec, extremes_sd[,c("Year", "condition")], by = "Year") %>%
  mutate(condition = ifelse(is.na(condition), "normal", condition))

tempfm <- left_join(febmay, extremes_fm[,c("Year", "condition")], by = "Year") %>%
  mutate(condition = ifelse(is.na(condition), "normal", condition))


pptcondition <- rbind(tempsd, tempfm) %>%
  mutate(month = ifelse(season == "sepdec", "Jan", "Jun"), day = 15) %>%
  mutate(Date_temp = paste(Year, month, sep = "_")) %>%
  mutate(Date_temp2 = paste(Date_temp, day, sep = "_")) %>%
  mutate(Date_final = ymd(Date_temp2)) %>%
  select(-Date_temp, -Date_temp2)


ggplot(pptcondition, aes(x=Year, y=meanppt)) +
  geom_point() +
  facet_wrap(~condition) +
  theme_bw()

ggplot(pptcondition, aes(x=Year, y=meanppt)) +
  geom_point(aes(color = condition)) +
  geom_line() +
  facet_wrap(~season) +
  theme_bw()


rm(list = c("ppt", "sepdec", "febmay", "drysepdec", "dryfebmay", "wetsepdec", "wetfebmay", 
            "extremes_sd", "extremes_fm", "tempsd", "tempfm", "jjppt", "jppt"))