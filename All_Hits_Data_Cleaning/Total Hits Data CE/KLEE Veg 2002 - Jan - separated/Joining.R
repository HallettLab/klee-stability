setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2002 - Jan - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2002Feb_BlockN.csv")
S <- read.csv("2002Feb_BlockS.csv")
C <- read.csv("2002Feb_BlockC.csv")

## join

temp <- rbind(N, S)
Feb2002 <- rbind(temp, C)

write.csv(Feb2002, "2002Feb_complete.csv", row.names = FALSE)
