setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2006 - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2006Mar_BlockN.csv")
S <- read.csv("2006Mar_BlockS.csv")
C <- read.csv("2006Mar_BlockC.csv")

## join

temp <- rbind(N, S)
Mar2006 <- rbind(temp, C)

write.csv(Mar2006, "2006Mar_complete.csv", row.names = FALSE)
