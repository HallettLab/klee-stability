setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2008 - June - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2008Jun_BlockN.csv")
S <- read.csv("2008Jun_BlockS.csv")
C <- read.csv("2008Jun_BlockC.csv")

## join

temp <- rbind(N, S)
Jun2008 <- rbind(temp, C)

write.csv(Jun2008, "2008Jun_complete.csv", row.names = FALSE)
