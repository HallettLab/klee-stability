################################################################################
###################### BLOCK C #################################################
################################################################################

setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - June - separated")

#load packages
library(tidyverse)


################################################################################
#### CC June 2000 Data Cleaning ################################################
################################################################################
CC <- read.csv("2000June_CC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
CC_cov <- CC[1:88,]

#transpose the data frame for easier manipulation
t_CC <- CC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CC) <- 1:nrow(t_CC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colcc <- as.vector(t_CC[1,])

#make character 
col.cc <- as.character(colcc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.cc, sep = ".")

#set as colnames
colnames(t_CC) <- col.cc

#remove first row as it now only contains duplicate info
CC_t <- t_CC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
CC_t <- CC_t[1:100,]

#need to clean the names
CC_clean <- clean_names(CC_t, sep_in = "_")

#flip to long format
CC_long <- pivot_longer(CC_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

CC_long$Sp_Pin_Hits <- as.numeric(CC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CC_final <- CC_long %>%
  mutate(Treatment = "C", Block = "C", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### CMW June 2000 Data Cleaning ###############################################
################################################################################
CMW <- read.csv("2000June_CMW_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
CMW_cov <- CMW[1:88,]

#transpose the data frame for easier manipulation
t_CMW <- CMW_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CMW) <- 1:nrow(t_CMW) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colcmw <- as.vector(t_CMW[1,])

#make character 
col.cmw <- as.character(colcmw)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.cmw, sep = ".")

#set as colnames
colnames(t_CMW) <- col.cmw

#remove first row as it now only contains duplicate info
CMW_t <- t_CMW[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
CMW_t <- CMW_t[1:100,]

#need to clean the names
CMW_clean <- clean_names(CMW_t, sep_in = "_")

#flip to long format
CMW_long <- pivot_longer(CMW_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

CMW_long$Sp_Pin_Hits <- as.numeric(CMW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMW_final <- CMW_long %>%
  mutate(Treatment = "MW", Block = "C", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CMWC June 2000 Data Cleaning ##############################################
################################################################################
CMWC <- read.csv("2000June_CMWC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
CMWC_cov <- CMWC[1:88,]

#transpose the data frame for easier manipulation
t_CMWC <- CMWC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CMWC) <- 1:nrow(t_CMWC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colcmwc <- as.vector(t_CMWC[1,])

#make character 
col.cmwc <- as.character(colcmwc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.cmwc, sep = ".")

#set as colnames
colnames(t_CMWC) <- col.cmwc

#remove first row as it now only contains duplicate info
CMWC_t <- t_CMWC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
CMWC_t <- CMWC_t[1:100,]

#need to clean the names
CMWC_clean <- clean_names(CMWC_t, sep_in = "_")

#flip to long format
CMWC_long <- pivot_longer(CMWC_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

CMWC_long$Sp_Pin_Hits <- as.numeric(CMWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMWC_final <- CMWC_long %>%
  mutate(Treatment = "MWC", Block = "C", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)


################################################################################
#### CO June 2000 Data Cleaning ################################################
################################################################################
CO <- read.csv("2000June_CO_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
CO_cov <- CO[1:88,]

#transpose the data frame for easier manipulation
t_CO <- CO_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CO) <- 1:nrow(t_CO) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colco <- as.vector(t_CO[1,])

#make character 
col.co <- as.character(colco)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.co, sep = ".")

#set as colnames
colnames(t_CO) <- col.co

#remove first row as it now only contains duplicate info
CO_t <- t_CO[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
CO_t <- CO_t[1:100,]

#need to clean the names
CO_clean <- clean_names(CO_t, sep_in = "_")

#flip to long format
CO_long <- pivot_longer(CO_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

CO_long$Sp_Pin_Hits <- as.numeric(CO_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CO_final <- CO_long %>%
  mutate(Treatment = "O", Block = "C", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### CW June 2000 Data Cleaning ################################################
################################################################################
CW <- read.csv("2000June_CW_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
CW_cov <- CW[1:88,]

#transpose the data frame for easier manipulation
t_CW <- CW_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CW) <- 1:nrow(t_CW) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colcw <- as.vector(t_CW[1,])

#make character 
col.cw <- as.character(colcw)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.cw, sep = ".")

#set as colnames
colnames(t_CW) <- col.cw

#remove first row as it now only contains duplicate info
CW_t <- t_CW[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
CW_t <- CW_t[1:100,]

#need to clean the names
CW_clean <- clean_names(CW_t, sep_in = "_")

#flip to long format
CW_long <- pivot_longer(CW_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

CW_long$Sp_Pin_Hits <- as.numeric(CW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CW_final <- CW_long %>%
  mutate(Treatment = "W", Block = "C", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)





################################################################################
#### CWC June 2000 Data Cleaning ###############################################
################################################################################
CWC <- read.csv("2000June_CWC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
CWC_cov <- CWC[1:88,]

#transpose the data frame for easier manipulation
t_CWC <- CWC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CWC) <- 1:nrow(t_CWC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colcwc <- as.vector(t_CWC[1,])

#make character 
col.cwc <- as.character(colcwc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.cwc, sep = ".")

#set as colnames
colnames(t_CWC) <- col.cwc

#remove first row as it now only contains duplicate info
CWC_t <- t_CWC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
CWC_t <- CWC_t[1:100,]

#need to clean the names
CWC_clean <- clean_names(CWC_t, sep_in = "_")

#flip to long format
CWC_long <- pivot_longer(CWC_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

CWC_long$Sp_Pin_Hits <- as.numeric(CWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CWC_final <- CWC_long %>%
  mutate(Treatment = "WC", Block = "C", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### Join all final data sheets into one C block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(CC_final, CMW_final)
b <- rbind(a, CMWC_final)
c <- rbind(b, CO_final)
d <- rbind(c, CW_final)
final <- rbind(d, CWC_final)

unique(final$Treatment)
unique(final$Block)
str(final)

write.csv(final, "2000Jun_BlockC.csv", row.names = FALSE)



