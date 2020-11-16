################################################################################
###################### BLOCK S #################################################
################################################################################

setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - Jan - separated")

#load packages
library(tidyverse)


################################################################################
#### SC Feb 2000 Data Cleaning #################################################
################################################################################
SC <- read.csv("2000Feb_SC_raw.csv")

#double check that all plots and dates are correct
view(SC) #plot correct, date are correct (Feb 2000)
colnames(SC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
SC <- SC[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(SC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
               "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", 
               "Indigofera_sch", "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", 
               "Polygala", "Portulaca", "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(SC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
SC2 <- SC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
SC_cov <- SC2[1:100,]

#flip to long format
SC_cov_long <- pivot_longer(SC_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

SC_cov_long$Sp_Pin_Hits <- as.numeric(SC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SC_final <- SC_cov_long %>%
  mutate(Treatment = "C", Block = "S", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SMW Feb 2000 Data Cleaning ################################################
################################################################################
SMW <- read.csv("2000Feb_SMW_raw.csv")

#double check that all plots and dates are correct
view(SMW) #plot correct, date are correct (Feb 2000)
colnames(SMW)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
SMW <- SMW[-c(1),1:30] #remove the first row which contains no useful data and column 31 which contains no data

names(SMW) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small")

head(SMW) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
SMW2 <- SMW[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
SMW_cov <- SMW2[1:100,]

#flip to long format
SMW_cov_long <- pivot_longer(SMW_cov, cols = "Themeda":"Solanum_small", names_to = "Species", values_to = "Sp_Pin_Hits")

SMW_cov_long$Sp_Pin_Hits <- as.numeric(SMW_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SMW_final <- SMW_cov_long %>%
  mutate(Treatment = "MW", Block = "S", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SMWC Feb 2000 Data Cleaning ###############################################
################################################################################
SMWC <- read.csv("2000Feb_SMWC_raw.csv")

#double check that all plots and dates are correct
view(SMWC) #plot correct, date are correct (Feb 2000)
colnames(SMWC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
SMWC <- SMWC[-c(1),1:30] #remove the first row which contains no useful data and column 31 which contains no data

names(SMWC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                 "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                 "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                 "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small")

head(SMWC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
SMWC2 <- SMWC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
SMWC_cov <- SMWC2[1:100,]

#flip to long format
SMWC_cov_long <- pivot_longer(SMWC_cov, cols = "Themeda":"Solanum_small", names_to = "Species", values_to = "Sp_Pin_Hits")

SMWC_cov_long$Sp_Pin_Hits <- as.numeric(SMWC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SMWC_final <- SMWC_cov_long %>%
  mutate(Treatment = "MWC", Block = "S", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SO Feb 2000 Data Cleaning ###############################################
################################################################################
SO <- read.csv("2000Feb_SO_raw.csv")

#double check that all plots and dates are correct
view(SO) #plot correct, date are correct (Feb 2000)
colnames(SO)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
SO <- SO[-c(1),1:31] #remove the first row which contains no useful data and columns 32+ which contain no data

names(SO) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
               "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
               "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
               "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(SO) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
SO2 <- SO[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
SO_cov <- SO2[1:100,]

#flip to long format
SO_cov_long <- pivot_longer(SO_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

SO_cov_long$Sp_Pin_Hits <- as.numeric(SO_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SO_final <- SO_cov_long %>%
  mutate(Treatment = "O", Block = "S", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### SW Feb 2000 Data Cleaning ###############################################
################################################################################
SW <- read.csv("2000Feb_SW_raw.csv")

#double check that all plots and dates are correct
view(SW) #plot correct, date are correct (Feb 2000)
colnames(SW)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
SW <- SW[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(SW) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
               "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
               "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
               "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(SW) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
SW2 <- SW[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
SW_cov <- SW2[1:100,]

#flip to long format
SW_cov_long <- pivot_longer(SW_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

SW_cov_long$Sp_Pin_Hits <- as.numeric(SW_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SW_final <- SW_cov_long %>%
  mutate(Treatment = "W", Block = "S", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SWC Feb 2000 Data Cleaning ###############################################
################################################################################
SWC <- read.csv("2000Feb_SWC_raw.csv")

#double check that all plots and dates are correct
view(SWC) #plot correct, date are correct (Feb 2000)
colnames(SWC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
SWC <- SWC[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(SWC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(SWC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
SWC2 <- SWC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
SWC_cov <- SWC2[1:100,]

#flip to long format
SWC_cov_long <- pivot_longer(SWC_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

SWC_cov_long$Sp_Pin_Hits <- as.numeric(SWC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SWC_final <- SWC_cov_long %>%
  mutate(Treatment = "WC", Block = "S", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### Join all final data sheets into one C block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(SC_final, SMW_final)
b <- rbind(a, SMWC_final)
c <- rbind(b, SO_final)
d <- rbind(c, SW_final)
final <- rbind(d, SWC_final)

unique(final$Treatment)
unique(final$Block)
str(final)

write.csv(final, "2000Feb_BlockS.csv", row.names = FALSE)



