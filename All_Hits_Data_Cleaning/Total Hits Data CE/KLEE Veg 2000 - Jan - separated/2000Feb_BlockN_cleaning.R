################################################################################
###################### BLOCK N #################################################
################################################################################

setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - Jan - separated")

#load packages
library(tidyverse)


################################################################################
#### NC Feb 2000 Data Cleaning #################################################
################################################################################
NC <- read.csv("2000Feb_NC_raw.csv")

#double check that all plots and dates are correct
view(NC) #plot correct, date are correct (Feb 2000)
colnames(NC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
NC <- NC[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(NC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", 
               "Indigofera_sch", "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", 
               "Polygala", "Portulaca", "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(NC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
NC2 <- NC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
NC_cov <- NC2[1:100,]

#flip to long format
NC_cov_long <- pivot_longer(NC_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

NC_cov_long$Sp_Pin_Hits <- as.numeric(NC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NC_final <- NC_cov_long %>%
  mutate(Treatment = "C", Block = "N", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NMW Feb 2000 Data Cleaning ################################################
################################################################################
NMW <- read.csv("2000Feb_NMW_raw.csv")

#double check that all plots and dates are correct
view(NMW) #plot correct, date are correct (Feb 2000)
colnames(NMW)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
NMW <- NMW[-c(1),1:30] #remove the first row which contains no useful data and column 31 which contains no data

names(NMW) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                     "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                     "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                     "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small")

head(NMW) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
NMW2 <- NMW[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
NMW_cov <- NMW2[1:100,]

#flip to long format
NMW_cov_long <- pivot_longer(NMW_cov, cols = "Themeda":"Solanum_small", names_to = "Species", values_to = "Sp_Pin_Hits")

NMW_cov_long$Sp_Pin_Hits <- as.numeric(NMW_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NMW_final <- NMW_cov_long %>%
  mutate(Treatment = "MW", Block = "N", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NMWC Feb 2000 Data Cleaning ###############################################
################################################################################
NMWC <- read.csv("2000Feb_NMWC_raw.csv")

#double check that all plots and dates are correct
view(NMWC) #plot correct, date are correct (Feb 2000)
colnames(NMWC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
NMWC <- NMWC[-c(1),1:30] #remove the first row which contains no useful data and column 31 which contains no data

names(NMWC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                      "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                      "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                      "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small")

head(NMWC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
NMWC2 <- NMWC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
NMWC_cov <- NMWC2[1:100,]

#flip to long format
NMWC_cov_long <- pivot_longer(NMWC_cov, cols = "Themeda":"Solanum_small", names_to = "Species", values_to = "Sp_Pin_Hits")

NMWC_cov_long$Sp_Pin_Hits <- as.numeric(NMWC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NMWC_final <- NMWC_cov_long %>%
  mutate(Treatment = "MWC", Block = "N", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NO Feb 2000 Data Cleaning ###############################################
################################################################################
NO <- read.csv("2000Feb_NO_raw.csv")

#double check that all plots and dates are correct
view(NO) #plot correct, date are correct (Feb 2000)
colnames(NO)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
NO <- NO[-c(1),1:31] #remove the first row which contains no useful data and columns 32+ which contain no data

names(NO) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(NO) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
NO2 <- NO[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
NO_cov <- NO2[1:100,]

#flip to long format
NO_cov_long <- pivot_longer(NO_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

NO_cov_long$Sp_Pin_Hits <- as.numeric(NO_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NO_final <- NO_cov_long %>%
  mutate(Treatment = "O", Block = "N", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### NW Feb 2000 Data Cleaning ###############################################
################################################################################
NW <- read.csv("2000Feb_NW_raw.csv")

#double check that all plots and dates are correct
view(NW) #plot correct, date are correct (Feb 2000)
colnames(NW)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
NW <- NW[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(NW) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                    "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                    "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                    "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(NW) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
NW2 <- NW[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
NW_cov <- NW2[1:100,]

#flip to long format
NW_cov_long <- pivot_longer(NW_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

NW_cov_long$Sp_Pin_Hits <- as.numeric(NW_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NW_final <- NW_cov_long %>%
  mutate(Treatment = "W", Block = "N", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NWC Feb 2000 Data Cleaning ###############################################
################################################################################
NWC <- read.csv("2000Feb_NWC_raw.csv")

#double check that all plots and dates are correct
view(NWC) #plot correct, date are correct (Feb 2000)
colnames(NWC)

#cut off extraneous rows
##need to cut off first few rows so that the column headers are correct
NWC <- NWC[-c(1),1:31] #remove the first row which contains no useful data and column 32 which contains no data

names(NWC) <- c("Station", "Location", "Disturbance", "Mound<5m", "Themeda", "P.mezianum", "P.stramineum", "Lintonia", 
                     "Brachiaria", "Aerva", "Aspilia", "Commelina", "Conyza", "Discoriste", "Helichrysum", "Hibiscus", "Indigofera_sch", 
                     "Indigofera_brev", "Kyllinger", "Misopates", "Monechima", "Monsonia", "Pavonia", "Phyllanthus", "Polygala", "Portulaca", 
                     "Rhinacanthus", "Rhynchosia", "Solanum_big", "Solanum_small", "Lipia")

head(NWC) #double check that all column names match the first row
#looks good

#delete the first row as it only contains a copy of the column names
NWC2 <- NWC[-c(1),]

##contains both cover and frequency data, want only cover data
## rows 1-100 are cover data
NWC_cov <- NWC2[1:100,]

#flip to long format
NWC_cov_long <- pivot_longer(NWC_cov, cols = "Themeda":"Lipia", names_to = "Species", values_to = "Sp_Pin_Hits")

NWC_cov_long$Sp_Pin_Hits <- as.numeric(NWC_cov_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NWC_final <- NWC_cov_long %>%
  mutate(Treatment = "WC", Block = "N", Date = "20000201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### Join all final data sheets into one C block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(NC_final, NMW_final)
b <- rbind(a, NMWC_final)
c <- rbind(b, NO_final)
d <- rbind(c, NW_final)
final <- rbind(d, NWC_final)

unique(final$Treatment)
str(final)

write.csv(final, "2000Feb_BlockN.csv", row.names = FALSE)



