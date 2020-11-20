setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2008 - Jan - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2008Feb_BlockN.csv")
S <- read.csv("2008Feb_BlockS.csv")
C <- read.csv("2008Feb_BlockC.csv")

## join

temp <- rbind(N, S)
Feb2008 <- rbind(temp, C)

write.csv(Feb2008, "2008Feb_complete.csv", row.names = FALSE)
