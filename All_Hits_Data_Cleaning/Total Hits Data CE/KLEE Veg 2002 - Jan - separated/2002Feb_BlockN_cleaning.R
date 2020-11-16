################################################################################
###################### BLOCK N #################################################
################################################################################

setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2002 - Jan - separated")

#load packages
library(tidyverse)


################################################################################
#### NC Feb 2002 Data Cleaning #################################################
################################################################################
NC <- read.csv("2002Feb_NC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
NC_cov <- NC[1:53,]

#transpose the data frame for easier manipulation
t_NC <- NC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NC) <- 1:nrow(t_NC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colnc <- as.vector(t_NC[1,])

#make character 
col.nc <- as.character(colnc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.nc, sep = ".")

#set as colnames
colnames(t_NC) <- col.nc

#remove first row as it now only contains duplicate info
NC_t <- t_NC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
NC_t <- NC_t[1:100,]

#need to clean the names
NC_clean <- clean_names(NC_t, sep_in = "_")

#flip to long format
NC_long <- pivot_longer(NC_clean, cols = "bothriochloa":"alistider", names_to = "Species", values_to = "Sp_Pin_Hits")

NC_long$Sp_Pin_Hits <- as.numeric(NC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NC_final <- NC_long %>%
  mutate(Treatment = "C", Block = "N", Date = "20020201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### NMW Feb 2002 Data Cleaning ###############################################
################################################################################
NMW <- read.csv("2002Feb_NMW_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
NMW_cov <- NMW[1:52,]

#transpose the data frame for easier manipulation
t_NMW <- NMW_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NMW) <- 1:nrow(t_NMW) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colnmw <- as.vector(t_NMW[1,])

#make character 
col.nmw <- as.character(colnmw)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.nmw, sep = ".")

#set as colnames
colnames(t_NMW) <- col.nmw

#remove first row as it now only contains duplicate info
NMW_t <- t_NMW[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
NMW_t <- NMW_t[1:100,]

#need to clean the names
NMW_clean <- clean_names(NMW_t, sep_in = "_")

#flip to long format
NMW_long <- pivot_longer(NMW_clean, cols = "bothriochloa":"setaria", names_to = "Species", values_to = "Sp_Pin_Hits")

NMW_long$Sp_Pin_Hits <- as.numeric(NMW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NMW_final <- NMW_long %>%
  mutate(Treatment = "MW", Block = "N", Date = "20020201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NMWC Feb 2002 Data Cleaning ###############################################
################################################################################
NMWC <- read.csv("2002Feb_NMWC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
NMWC_cov <- NMWC[1:52,]

#transpose the data frame for easier manipulation
t_NMWC <- NMWC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NMWC) <- 1:nrow(t_NMWC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colnmwc <- as.vector(t_NMWC[1,])

#make character 
col.nmwc <- as.character(colnmwc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.nmwc, sep = ".")

#set as colnames
colnames(t_NMWC) <- col.nmwc

#remove first row as it now only contains duplicate info
NMWC_t <- t_NMWC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
NMWC_t <- NMWC_t[1:100,]

#need to clean the names
NMWC_clean <- clean_names(NMWC_t, sep_in = "_")

#flip to long format
NMWC_long <- pivot_longer(NMWC_clean, cols = "bothriochloa":"o.", names_to = "Species", values_to = "Sp_Pin_Hits")

NMWC_long$Sp_Pin_Hits <- as.numeric(NMWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NMWC_final <- NMWC_long %>%
  mutate(Treatment = "MWC", Block = "N", Date = "20020201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)


################################################################################
#### NO Feb 2002 Data Cleaning #################################################
################################################################################
NO <- read.csv("2002Feb_NO_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
NO_cov <- NO[1:52,]

#transpose the data frame for easier manipulation
t_NO <- NO_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NO) <- 1:nrow(t_NO) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colno <- as.vector(t_NO[1,])

#make character 
col.no <- as.character(colno)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.no, sep = ".")

#set as colnames
colnames(t_NO) <- col.no

#remove first row as it now only contains duplicate info
NO_t <- t_NO[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
NO_t <- NO_t[1:100,]

#need to clean the names
NO_clean <- clean_names(NO_t, sep_in = "_")

#flip to long format
NO_long <- pivot_longer(NO_clean, cols = "bothriochloa":"o.", names_to = "Species", values_to = "Sp_Pin_Hits")

NO_long$Sp_Pin_Hits <- as.numeric(NO_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NO_final <- NO_long %>%
  mutate(Treatment = "O", Block = "N", Date = "20020201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### NW Feb 2002 Data Cleaning #################################################
################################################################################
NW <- read.csv("2002Feb_NW_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
NW_cov <- NW[1:53,]

#transpose the data frame for easier manipulation
t_NW <- NW_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NW) <- 1:nrow(t_NW) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colnw <- as.vector(t_NW[1,])

#make character 
col.nw <- as.character(colnw)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.nw, sep = ".")

#set as colnames
colnames(t_NW) <- col.nw

#remove first row as it now only contains duplicate info
NW_t <- t_NW[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
NW_t <- NW_t[1:100,]

#need to clean the names
NW_clean <- clean_names(NW_t, sep_in = "_")

#flip to long format
NW_long <- pivot_longer(NW_clean, cols = "bothriochloa":"mikrochloa.kunthii", names_to = "Species", values_to = "Sp_Pin_Hits")

NW_long$Sp_Pin_Hits <- as.numeric(NW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NW_final <- NW_long %>%
  mutate(Treatment = "W", Block = "N", Date = "20020201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)





################################################################################
#### NWC Feb 2002 Data Cleaning ################################################
################################################################################
NWC <- read.csv("2002Feb_NWC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
NWC_cov <- NWC[1:51,]

#transpose the data frame for easier manipulation
t_NWC <- NWC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NWC) <- 1:nrow(t_NWC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colnwc <- as.vector(t_NWC[1,])

#make character 
col.nwc <- as.character(colnwc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.nwc, sep = ".")

#set as colnames
colnames(t_NWC) <- col.nwc

#remove first row as it now only contains duplicate info
NWC_t <- t_NWC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
NWC_t <- NWC_t[1:100,]

#need to clean the names
NWC_clean <- clean_names(NWC_t, sep_in = "_")

#flip to long format
NWC_long <- pivot_longer(NWC_clean, cols = "bothriochloa":"bares", names_to = "Species", values_to = "Sp_Pin_Hits")

NWC_long$Sp_Pin_Hits <- as.numeric(NWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NWC_final <- NWC_long %>%
  mutate(Treatment = "WC", Block = "N", Date = "20020201") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### Join all final data sheets into one S block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(NC_final, NMW_final)
b <- rbind(a, NMWC_final)
c <- rbind(b, NO_final)
d <- rbind(c, NW_final)
final <- rbind(d, NWC_final)

unique(final$Treatment)
unique(final$Block)
unique(final$Date)
str(final)

write.csv(final, "2002Feb_BlockN.csv", row.names = FALSE)



