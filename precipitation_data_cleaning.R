setwd("~/Repositories/klee-stability")

library(tidyverse)
library(lubridate)

ppt <- read.csv("KLEE_monthly_precipitation_03_13.csv")

colnames(ppt) <- c("Month", "North", "Central", "South", "Average", "std.error")

ppt2 <- ppt %>%
  separate(Month, into = c("Month", "Year"), sep = " ") %>%
  mutate(Day = 15) %>%
  mutate(Date_temp = paste(Year, Month, sep = "_")) %>%
  mutate(Date_temp2 = paste(Date_temp, Day, sep = "_")) %>%
  mutate(Date_final = ymd(Date_temp2)) %>% #change dates into recognized format
  select(-Month, -Day, -Year, -Date_temp, -Date_temp2)

ggplot(ppt2, aes(x=Date_final, y=Average)) +
  geom_line() +
  xlab("Date") + ylab("Average Monthly Precipitation (mm)") +
  theme_bw()

