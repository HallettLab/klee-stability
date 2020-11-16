################################################################################
############ BLOCK C ###########################################################
################################################################################


setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2001 - Jan - separated")

#load packages
library(tidyverse)


################################################################################
#### CC Jan 2001 Data Cleaning #################################################
################################################################################
CC <- read.csv("2001Jan_CC_raw.csv")

#double check that all plots and dates are correct
view(CC) #plot correct, date are correct (Feb 2000)
colnames(CC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CC <- CC[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
              "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", 
               "Indigofera_sch", "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", 
              "Polygala", "Portulaca", "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Bare")

head(CC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CC2 <- CC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CC_cov <- CC2[1:100,]

#flip to long format
CC_cov_long <- pivot_longer(CC_cov, cols = "Themeda":"Bare", names_to = "Species", values_to = "Sp_Pin_Hits")

CC_cov_long$Sp_Pin_Hits <- as.numeric(CC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CC_final <- CC_cov_long %>%
  mutate(Treatment = "C", Block = "C", Date = "20010101") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CMW Jan 2001 Data Cleaning ################################################
################################################################################
CMW <- read.csv("2001Jan_CMW_raw.csv")

#double check that all plots and dates are correct
view(CMW) #plot correct, date are correct (Feb 2000)
colnames(CMW)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CMW <- CMW[-c(1),1:31] #remove the first row which contains no useful data and column 31 which contains no data

names(CMW) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                     "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                     "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                     "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Bare")

head(CMW) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CMW2 <- CMW[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CMW_cov <- CMW[1:100,]

#flip to long format
CMW_cov_long <- pivot_longer(CMW_cov, cols = "Themeda":"Bare", names_to = "Species", values_to = "Sp_Pin_Hits")

CMW_cov_long$Sp_Pin_Hits <- as.numeric(CMW_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMW_final <- CMW_cov_long %>%
  mutate(Treatment = "MW", Block = "C", Date = "20010101") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CMWC Jan 2001 Data Cleaning ###############################################
################################################################################

CMWC <- read.csv("2001Jan_CMWC_raw.csv")

#double check that all plots and dates are correct
view(CMWC) #plot correct, date are correct (Feb 2000)
colnames(CMWC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CMWC <- CMWC[-c(1),1:31] #remove the first row which contains no useful data and column 31 which contains no data

names(CMWC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                      "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                      "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                      "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Bare")

head(CMWC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CMWC2 <- CMWC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CMWC_cov <- CMWC2[1:100,]

#flip to long format
CMWC_cov_long <- pivot_longer(CMWC_cov, cols = "Themeda":"Bare", names_to = "Species", values_to = "Sp_Pin_Hits")

CMWC_cov_long$Sp_Pin_Hits <- as.numeric(CMWC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMWC_final <- CMWC_cov_long %>%
  mutate(Treatment = "MWC", Block = "C", Date = "20010101") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CO Jan 2001 Data Cleaning ###############################################
################################################################################
CO <- read.csv("2001Jan_CO_raw.csv")

#double check that all plots and dates are correct
view(CO) #plot correct, date are correct (Feb 2000)
colnames(CO)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CO <- CO[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CO) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Bare")

head(CO) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CO2 <- CO[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CO_cov <- CO2[1:100,]

#flip to long format
CO_cov_long <- pivot_longer(CO_cov, cols = "Themeda":"Bare", names_to = "Species", values_to = "Sp_Pin_Hits")

CO_cov_long$Sp_Pin_Hits <- as.numeric(CO_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CO_final <- CO_cov_long %>%
  mutate(Treatment = "O", Block = "C", Date = "20010101") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### CW Jan 2001 Data Cleaning ###############################################
################################################################################

CW <- read.csv("2001Jan_CW_raw.csv")

#double check that all plots and dates are correct
view(CW) #plot correct, date are correct (Feb 2000)
colnames(CW)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CW <- CW[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CW) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Bare")

head(CW) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CW2 <- CW[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CW_cov <- CW2[1:100,]

#flip to long format
CW_cov_long <- pivot_longer(CW_cov, cols = "Themeda":"Bare", names_to = "Species", values_to = "Sp_Pin_Hits")

CW_cov_long$Sp_Pin_Hits <- as.numeric(CW_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CW_final <- CW_cov_long %>%
  mutate(Treatment = "W", Block = "C", Date = "20010101") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CWC Jan 2001 Data Cleaning ###############################################
################################################################################

CWC <- read.csv("2001Jan_CWC_raw.csv")


#double check that all plots and dates are correct
view(CWC) #plot correct, date are correct (Feb 2000)
colnames(CWC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CWC <- CWC[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CWC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                     "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                     "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                     "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Bare")

head(CWC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CWC2 <- CWC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CWC_cov <- CWC2[1:100,]

#flip to long format
CWC_cov_long <- pivot_longer(CWC_cov, cols = "Themeda":"Bare", names_to = "Species", values_to = "Sp_Pin_Hits")

CWC_cov_long$Sp_Pin_Hits <- as.numeric(CWC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CWC_final <- CWC_cov_long %>%
  mutate(Treatment = "WC", Block = "C", Date = "20010101") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### Join all final data sheets into one C block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(CC_final, CMW_final)
b <- rbind(a, CO_final)
c <- rbind(b, CMWC_final)
d <- rbind(c, CW_final)
final <- rbind(d, CWC_final)

unique(final$Treatment)
unique(final$Date)
unique(final$Block)
str(final)

write.csv(final, "2001Jan_BlockC.csv", row.names = FALSE)



