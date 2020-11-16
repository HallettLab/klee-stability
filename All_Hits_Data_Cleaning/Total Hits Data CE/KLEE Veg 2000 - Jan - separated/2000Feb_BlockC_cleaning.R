################################################################################
############ BLOCK C ###########################################################
################################################################################


setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - Jan - separated")

#load packages
library(tidyverse)


#read in February 2000 pin hit data all data to explore
CC2000F <- read.csv("2000Feb_CC_raw.csv")
CMW2000F <- read.csv("2000Feb_CMW_raw.csv")
CMWC2000F <- read.csv("2000Feb_CMWC_raw.csv")
CO2000F <- read.csv("2000Feb_CO_raw.csv")
CW2000F <- read.csv("2000Feb_CW_raw.csv")
CWC2000F <- read.csv("2000Feb_CWC_raw.csv")


################################################################################
#### CC Feb 2000 Data Cleaning #################################################
################################################################################

#double check that all plots and dates are correct
view(CC2000F) #plot correct, date are correct (Feb 2000)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CC2000F <- CC2000F[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CC2000F) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                   "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(CC2000F) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CC00F <- CC2000F[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CC00F_cov <- CC00F[1:100,]

#flip to long format
CC00Fcov_long <- pivot_longer(CC00F_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

CC00Fcov_long$Sp_Pin_Hits <- as.numeric(CC00Fcov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CC00F_final <- CC00Fcov_long %>%
  mutate(Treatment = "C", Block = "C", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CMW Feb 2000 Data Cleaning ################################################
################################################################################

#double check that all plots and dates are correct
view(CMW2000F) #plot correct, date are correct (Feb 2000)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CMW2000F <- CMW2000F[-c(1),1:30] #remove the first row which contains no useful data and column 31 which contains no data

names(CMW2000F) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small")

head(CMW2000F) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CMW00F <- CMW2000F[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CMW00F_cov <- CMW00F[1:100,]

#flip to long format
CMW00Fcov_long <- pivot_longer(CMW00F_cov, cols = "Themeda":"Solanum_small", names_to = "Species", values_to = "Sp_Pin_Hits")

CMW00Fcov_long$Sp_Pin_Hits <- as.numeric(CMW00Fcov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMW00F_final <- CMW00Fcov_long %>%
  mutate(Treatment = "MW", Block = "C", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CMWC Feb 2000 Data Cleaning ###############################################
################################################################################

#double check that all plots and dates are correct
view(CMWC2000F) #plot correct, date are correct (Feb 2000)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CMWC2000F <- CMWC2000F[-c(1),1:30] #remove the first row which contains no useful data and column 31 which contains no data

names(CMWC2000F) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                     "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                     "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                     "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small")

head(CMWC2000F) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CMWC00F <- CMWC2000F[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CMWC00F_cov <- CMWC00F[1:100,]

#flip to long format
CMWC00Fcov_long <- pivot_longer(CMWC00F_cov, cols = "Themeda":"Solanum_small", names_to = "Species", values_to = "Sp_Pin_Hits")

CMWC00Fcov_long$Sp_Pin_Hits <- as.numeric(CMWC00Fcov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMWC00F_final <- CMWC00Fcov_long %>%
  mutate(Treatment = "MWC", Block = "C", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CO Feb 2000 Data Cleaning ###############################################
################################################################################

#double check that all plots and dates are correct
view(CO2000F) #plot correct, date are correct (Feb 2000)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CO2000F <- CO2000F[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CO2000F) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                      "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                      "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                      "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(CO2000F) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CO00F <- CO2000F[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CO00F_cov <- CO00F[1:100,]

#flip to long format
CO00Fcov_long <- pivot_longer(CO00F_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

CO00Fcov_long$Sp_Pin_Hits <- as.numeric(CO00Fcov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CO00F_final <- CO00Fcov_long %>%
  mutate(Treatment = "O", Block = "C", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### CW Feb 2000 Data Cleaning ###############################################
################################################################################

#double check that all plots and dates are correct
view(CW2000F) #plot correct, date are correct (Feb 2000)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CW2000F <- CW2000F[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CW2000F) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(CW2000F) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CW00F <- CW2000F[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CW00F_cov <- CW00F[1:100,]

#flip to long format
CW00Fcov_long <- pivot_longer(CW00F_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

CW00Fcov_long$Sp_Pin_Hits <- as.numeric(CW00Fcov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CW00F_final <- CW00Fcov_long %>%
  mutate(Treatment = "W", Block = "C", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)






################################################################################
#### CWC Feb 2000 Data Cleaning ###############################################
################################################################################

#double check that all plots and dates are correct
view(CWC2000F) #plot correct, date are correct (Feb 2000)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
CWC2000F <- CWC2000F[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(CWC2000F) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(CWC2000F) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
CWC00F <- CWC2000F[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
CWC00F_cov <- CWC00F[1:100,]

#flip to long format
CWC00Fcov_long <- pivot_longer(CWC00F_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

CWC00Fcov_long$Sp_Pin_Hits <- as.numeric(CWC00Fcov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CWC00F_final <- CWC00Fcov_long %>%
  mutate(Treatment = "WC", Block = "C", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)





################################################################################
#### Join all final data sheets into one C block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(CC00F_final, CMW00F_final)
b <- rbind(a, CO00F_final)
c <- rbind(b, CMWC00F_final)
d <- rbind(c, CW00F_final)
final <- rbind(d, CWC00F_final)

unique(final$Treatment)
str(final)

write.csv(final, "2000Feb_BlockC.csv", row.names = FALSE)



