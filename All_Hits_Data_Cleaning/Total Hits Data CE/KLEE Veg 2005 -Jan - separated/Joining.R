setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2005 -Jan - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2005Feb_BlockN.csv")
S <- read.csv("2005Feb_BlockS.csv")
C <- read.csv("2005Feb_BlockC.csv")

## join

temp <- rbind(N, S)
Feb2005 <- rbind(temp, C)

write.csv(Feb2005, "2005Feb_complete.csv", row.names = FALSE)
