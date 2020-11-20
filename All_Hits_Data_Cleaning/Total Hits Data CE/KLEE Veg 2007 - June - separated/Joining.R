setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2007 - June - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2007Jun_BlockN.csv")
S <- read.csv("2007Jun_BlockS.csv")
C <- read.csv("2007Jun_BlockC.csv")

## join

temp <- rbind(N, S)
Jun2007 <- rbind(temp, C)

write.csv(Jun2007, "2007Jun_complete.csv", row.names = FALSE)
