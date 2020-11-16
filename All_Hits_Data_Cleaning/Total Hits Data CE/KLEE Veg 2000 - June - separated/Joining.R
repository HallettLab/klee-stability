setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - June - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for Feb 2000 sampling #########################

N <- read.csv("2000Jun_BlockN.csv")
S <- read.csv("2000Jun_BlockS.csv")
C <- read.csv("2000Jun_BlockC.csv")

## join

temp <- rbind(N, S)
Jun2000 <- rbind(temp, C)

write.csv(Jun2000, "2000Jun_complete.csv", row.names = FALSE)
