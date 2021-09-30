
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