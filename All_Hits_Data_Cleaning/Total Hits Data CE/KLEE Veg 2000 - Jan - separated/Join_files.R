setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - Jan - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2000Feb_BlockN.csv")
S <- read.csv("2000Feb_BlockS.csv")
C <- read.csv("2000Feb_BlockC.csv")

## join

temp <- rbind(N, S)
Feb2000 <- rbind(temp, C)

write.csv(Feb2000, "2000Feb_complete.csv", row.names = FALSE)
