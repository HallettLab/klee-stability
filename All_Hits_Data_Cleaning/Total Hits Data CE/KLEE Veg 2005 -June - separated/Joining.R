setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2005 -June - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2005Jun_BlockN.csv")
S <- read.csv("2005Jun_BlockS.csv")
C <- read.csv("2005Jun_BlockC.csv")

## join

temp <- rbind(N, S)
Jun2005 <- rbind(temp, C)

write.csv(Jun2005, "2005Jun_complete.csv", row.names = FALSE)
