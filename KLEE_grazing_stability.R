setwd("~/Repositories/klee-stability")

source("KLEE_data_cleaning.R")
library(tidyverse)

## Get data ready to graph as a time series
meancov <- totcov_long %>%
  group_by(Treatment, Date) %>% #group by treatment & date
  summarize(Mean_Cov = mean(Tot_Cover)) #calculate the mean total cover at each treatment & date


#change variable formats to get this to work in geom_line
meancov$Treatment <- as.character(meancov$Treatment)
meancov$Date <- as.numeric(meancov$Date)

## Graph! 
#graph the time series - all treatments in one plot
ggplot(meancov, aes(x=Date)) +
  geom_line(aes(y=Mean_Cov, col=Treatment)) +
  theme_bw() +
  ylab("Total Cover") + xlab("Date (yyyymm)")
 
#separate by treatment to more easily view overlapping lines
ggplot(meancov, aes(x=Date)) +
  geom_line(aes(y=Mean_Cov, col=Treatment)) +
  theme_bw() +
  ylab("Total Cover") + xlab("Date (yyyymm)") +
  facet_wrap(~Treatment)

#graph as scatterplot
ggplot(meancov, aes(x=Date, y=Mean_Cov, col=Treatment)) + 
  geom_point() +
  facet_wrap(~Treatment) +
  theme_bw()
