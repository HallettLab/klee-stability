setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2001 - June - separated")

#load packages
library(tidyverse)

################################################################################
########### Create one data file for June 2001 sampling ########################

N <- read.csv("2001May_BlockN.csv")
S <- read.csv("2001May_BlockS.csv")
C <- read.csv("2001May_BlockC.csv")

## join

temp <- rbind(N, S)
May2001 <- rbind(temp, C)

write.csv(May2001, "2001May_complete.csv", row.names = FALSE)
