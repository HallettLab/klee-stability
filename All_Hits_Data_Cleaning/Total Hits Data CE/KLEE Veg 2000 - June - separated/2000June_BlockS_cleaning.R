################################################################################
###################### BLOCK S #################################################
################################################################################

setwd("~/Research/KLEE/Total Hits Data CE/KLEE Veg 2000 - June - separated")

#load packages
library(tidyverse)
library(janitor)


### The first chunk of code works. Should hopefully be able to apply to other 
### data sets formatted this way. 
################################################################################
#### SC June 2000 Data Cleaning ################################################
################################################################################
SC <- read.csv("2000June_SC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
SC_cov <- SC[1:88,]

#transpose the data frame for easier manipulation
t_SC <- SC_cov %>%
   t() %>%
   as.data.frame() #save this as a dataframe

row.names(t_SC) <- 1:nrow(t_SC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
col <- as.vector(t_SC[1,])

#make character 
col.c <- as.character(col)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.c, sep = ".")

#set as colnames
colnames(t_SC) <- col.c

#remove first row as it now only contains duplicate info
SC_t <- t_SC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
SC_t <- SC_t[1:100,]

#need to clean the names
SC_clean <- clean_names(SC_t, sep_in = "_")

#flip to long format
SC_long <- pivot_longer(SC_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

SC_long$Sp_Pin_Hits <- as.numeric(SC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SC_final <- SC_long %>%
  mutate(Treatment = "C", Block = "S", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### SMW June 2000 Data Cleaning ###############################################
################################################################################
SMW <- read.csv("2000June_SMW_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
SMW_cov <- SMW[1:88,]

#transpose the data frame for easier manipulation
t_SMW <- SMW_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_SMW) <- 1:nrow(t_SMW) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colsmw <- as.vector(t_SMW[1,])

#make character 
col.smw <- as.character(colsmw)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.smw, sep = ".")

#set as colnames
colnames(t_SMW) <- col.smw

#remove first row as it now only contains duplicate info
SMW_t <- t_SMW[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
SMW_t <- SMW_t[1:100,]

#need to clean the names
SMW_clean <- clean_names(SMW_t, sep_in = "_")

#flip to long format
SMW_long <- pivot_longer(SMW_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

SMW_long$Sp_Pin_Hits <- as.numeric(SMW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SMW_final <- SMW_long %>%
  mutate(Treatment = "MW", Block = "S", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SMWC June 2000 Data Cleaning ##############################################
################################################################################
SMWC <- read.csv("2000June_SMWC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
SMWC_cov <- SMWC[1:88,]

#transpose the data frame for easier manipulation
t_SMWC <- SMWC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_SMWC) <- 1:nrow(t_SMWC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colsmwc <- as.vector(t_SMWC[1,])

#make character 
col.smwc <- as.character(colsmwc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.smwc, sep = ".")

#set as colnames
colnames(t_SMWC) <- col.smwc

#remove first row as it now only contains duplicate info
SMWC_t <- t_SMWC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
SMWC_t <- SMWC_t[1:100,]

#need to clean the names
SMWC_clean <- clean_names(SMWC_t, sep_in = "_")

#flip to long format
SMWC_long <- pivot_longer(SMWC_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

SMWC_long$Sp_Pin_Hits <- as.numeric(SMWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SMWC_final <- SMWC_long %>%
  mutate(Treatment = "MWC", Block = "S", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)


################################################################################
#### SO June 2000 Data Cleaning ################################################
################################################################################
SO <- read.csv("2000June_SO_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
SO_cov <- SO[1:88,]

#transpose the data frame for easier manipulation
t_SO <- SO_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_SO) <- 1:nrow(t_SO) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colso <- as.vector(t_SO[1,])

#make character 
col.so <- as.character(colso)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.so, sep = ".")

#set as colnames
colnames(t_SO) <- col.so

#remove first row as it now only contains duplicate info
SO_t <- t_SO[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
SO_t <- SO_t[1:100,]

#need to clean the names
SO_clean <- clean_names(SO_t, sep_in = "_")

#flip to long format
SO_long <- pivot_longer(SO_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

SO_long$Sp_Pin_Hits <- as.numeric(SO_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SO_final <- SO_long %>%
  mutate(Treatment = "O", Block = "S", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### SW June 2000 Data Cleaning ################################################
################################################################################
SW <- read.csv("2000June_SW_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
SW_cov <- SW[1:88,]

#transpose the data frame for easier manipulation
t_SW <- SW_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_SW) <- 1:nrow(t_SW) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colsw <- as.vector(t_SW[1,])

#make character 
col.sw <- as.character(colsw)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.sw, sep = ".")

#set as colnames
colnames(t_SW) <- col.sw

#remove first row as it now only contains duplicate info
SW_t <- t_SW[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
SW_t <- SW_t[1:100,]

#need to clean the names
SW_clean <- clean_names(SW_t, sep_in = "_")

#flip to long format
SW_long <- pivot_longer(SW_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

SW_long$Sp_Pin_Hits <- as.numeric(SW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SW_final <- SW_long %>%
  mutate(Treatment = "W", Block = "S", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)





################################################################################
#### SWC June 2000 Data Cleaning ###############################################
################################################################################
SWC <- read.csv("2000June_SWC_raw.csv", skip = 1)

#cut off extraneous rows to only get cover data
SWC_cov <- SWC[1:88,]

#transpose the data frame for easier manipulation
t_SWC <- SWC_cov %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_SWC) <- 1:nrow(t_SWC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
colswc <- as.vector(t_SWC[1,])

#make character 
col.swc <- as.character(colswc)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.swc, sep = ".")

#set as colnames
colnames(t_SWC) <- col.swc

#remove first row as it now only contains duplicate info
SWC_t <- t_SWC[-1,]

#subset again to only get useful data -> remove oddly formatted summary rows
SWC_t <- SWC_t[1:100,]

#need to clean the names
SWC_clean <- clean_names(SWC_t, sep_in = "_")

#flip to long format
SWC_long <- pivot_longer(SWC_clean, cols = "bothriochloa.insculpta":"x_2", names_to = "Species", values_to = "Sp_Pin_Hits")

SWC_long$Sp_Pin_Hits <- as.numeric(SWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SWC_final <- SWC_long %>%
  mutate(Treatment = "WC", Block = "S", Date = "20000601") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### Join all final data sheets into one S block datasheet #####################
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

write.csv(final, "2000Jun_BlockS.csv", row.names = FALSE)



