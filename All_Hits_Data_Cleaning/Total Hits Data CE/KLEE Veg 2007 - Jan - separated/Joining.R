setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2007 - Jan - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2007Jan_BlockN.csv")
S <- read.csv("2007Jan_BlockS.csv")
C <- read.csv("2007Jan_BlockC.csv")

## join

temp <- rbind(N, S)
Jan2007 <- rbind(temp, C)

write.csv(Jan2007, "2007Jan_complete.csv", row.names = FALSE)
