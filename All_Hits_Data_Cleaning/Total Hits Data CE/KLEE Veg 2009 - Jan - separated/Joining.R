setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2009 - Jan - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2009Jan_BlockN.csv")
S <- read.csv("2009Jan_BlockS.csv")
C <- read.csv("2009Jan_BlockC.csv")

## join

temp <- rbind(N, S)
Jan2009 <- rbind(temp, C)

write.csv(Jan2009, "2009Jan_complete.csv", row.names = FALSE)
