################################################################################
###################### BLOCK C #################################################
################################################################################

setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2007 - Jan - separated")

#load packages
library(tidyverse)
library(janitor)


################################################################################
#### CC Jan 2007 Data Cleaning #################################################
################################################################################
CC <- read.csv("2007Jan_CC_raw.csv", skip = 4) ##manually filled trap station names, can do this in R for other datasheets


##cut out rows 2&3 -> one is blank, the other is for disturbance but has no values
CC2 <- CC[-(2:3),]

#transpose the data frame for easier manipulation
t_CC <- CC2 %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_CC) <- 1:nrow(t_CC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
col <- as.vector(t_CC[1,])

#make character 
col.cc <- as.character(col)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.cc, sep = ".")

#set as colnames
colnames(t_CC) <- col.cc

#remove first row as it now only contains duplicate info
CC_t <- t_CC[-1,]

#need to clean the names
CC_clean <- clean_names(CC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data

#rename 'x' column
colnames(CC_clean)[colnames(CC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of NC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
CC_clean$type.of.hits[CC_clean$type.of.hits==""] <- NA 
CC_clean$trap.station[CC_clean$trap.station==""] <- NA 


#fill type.of.hits & trap.station columns
CC_fill <- CC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")

#flip to long format
CC_long <- pivot_longer(CC_fill, cols = "bracharia.lacnantha":"x_5", names_to = "Species", values_to = "Sp_Pin_Hits")

CC_long$Sp_Pin_Hits <- as.numeric(CC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CC_final <- CC_long %>%
  mutate(Treatment = "C", Block = "C", Date = "20070129") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CMW Jan 2007 Data Cleaning ################################################
################################################################################
CMW <- read.csv("2007Jan_CMW_raw.csv", skip = 4)

##cut out rows 2&3 -> one is blank, the other is for disturbance but has no values
CMW2 <- CMW[-(2:3),]

#transpose the data frame for easier manipulation
t_CMW <- CMW2 %>%
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

#subset again to only get useful data -> remove blank rows at end
CMW_t <- CMW_t[1:70,]

#need to clean the names
CMW_clean <- clean_names(CMW_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(CMW_clean)[colnames(CMW_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
CMW_clean$type.of.hits[CMW_clean$type.of.hits==""] <- NA 
CMW_clean$trap.station[CMW_clean$trap.station==""] <- NA 


#fill type.of.hits column
CMW_fill <- CMW_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
CMW_long <- pivot_longer(CMW_fill, cols = "bracharia.lacnantha":"x_5", names_to = "Species", values_to = "Sp_Pin_Hits")

CMW_long$Sp_Pin_Hits <- as.numeric(CMW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMW_final <- CMW_long %>%
  mutate(Treatment = "MW", Block = "C", Date = "20070210") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### CMWC Jan 2007 Data Cleaning ###############################################
################################################################################
CMWC <- read.csv("2007Jan_CMWC_raw.csv", skip = 4)

##cut out rows 2&3 -> one is blank, the other is for disturbance but has no values
CMWC2 <- CMWC[-(2:3),]

#transpose the data frame for easier manipulation
t_CMWC <- CMWC2 %>%
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

#subset again to only get useful data -> remove blank rows at end
CMWC_t <- CMWC_t[1:70,]

#need to clean the names
CMWC_clean <- clean_names(CMWC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(CMWC_clean)[colnames(CMWC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
CMWC_clean$type.of.hits[CMWC_clean$type.of.hits==""] <- NA 
CMWC_clean$trap.station[CMWC_clean$trap.station==""] <- NA 


#fill type.of.hits column
CMWC_fill <- CMWC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
CMWC_long <- pivot_longer(CMWC_fill, cols = "bracharia.lacnantha":"x_5", names_to = "Species", values_to = "Sp_Pin_Hits")

CMWC_long$Sp_Pin_Hits <- as.numeric(CMWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CMWC_final <- CMWC_long %>%
  mutate(Treatment = "MWC", Block = "C", Date = "20070125") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### CO Jan 2007 Data Cleaning #################################################
################################################################################
CO <- read.csv("2007Jan_CO_raw.csv", skip = 4)

##cut out rows 2&3 -> one is blank, the other is for disturbance but has no values
CO2 <- CO[-(2:3),]

#transpose the data frame for easier manipulation
t_CO <- CO2 %>%
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

#subset again to only get useful data -> remove blank rows at end
CO_t <- CO_t[1:70,]

#need to clean the names
CO_clean <- clean_names(CO_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(CO_clean)[colnames(CO_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
CO_clean$type.of.hits[CO_clean$type.of.hits==""] <- NA 
CO_clean$trap.station[CO_clean$trap.station==""] <- NA 


#fill type.of.hits column
CO_fill <- CO_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
CO_long <- pivot_longer(CO_fill, cols = "bracharia.lacnantha":"x_5", names_to = "Species", values_to = "Sp_Pin_Hits")

CO_long$Sp_Pin_Hits <- as.numeric(CO_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CO_final <- CO_long %>%
  mutate(Treatment = "O", Block = "C", Date = "20070216") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### CW Jan 2007 Data Cleaning #################################################
################################################################################
CW <- read.csv("2007Jan_CW_raw.csv", skip = 4)

##cut out rows 2&3 -> one is blank, the other is for disturbance but has no values
CW2 <- CW[-(2:3),]

#transpose the data frame for easier manipulation
t_CW <- CW2 %>%
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

#subset again to only get useful data -> remove blank rows at end
CW_t <- CW_t[1:70,]

#need to clean the names
CW_clean <- clean_names(CW_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(CW_clean)[colnames(CW_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
CW_clean$type.of.hits[CW_clean$type.of.hits==""] <- NA 
CW_clean$trap.station[CW_clean$trap.station==""] <- NA 


#fill type.of.hits column
CW_fill <- CW_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
CW_long <- pivot_longer(CW_fill, cols = "bracharia.lacnantha":"x_5", names_to = "Species", values_to = "Sp_Pin_Hits")

CW_long$Sp_Pin_Hits <- as.numeric(CW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CW_final <- CW_long %>%
  mutate(Treatment = "W", Block = "C", Date = "20070215") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)





################################################################################
#### CWC Jan 2007 Data Cleaning ################################################
################################################################################
CWC <- read.csv("2007Jan_CWC_raw.csv", skip = 4)

##cut out rows 2&3 -> one is blank, the other is for disturbance but has no values
CWC2 <- CWC[-(2:3),]

#transpose the data frame for easier manipulation
t_CWC <- CWC2 %>%
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

#subset again to only get useful data -> remove blank rows at end
CWC_t <- CWC_t[1:70,]

#need to clean the names
CWC_clean <- clean_names(CWC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(CWC_clean)[colnames(CWC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
CWC_clean$type.of.hits[CWC_clean$type.of.hits==""] <- NA 
CWC_clean$trap.station[CWC_clean$trap.station==""] <- NA 


#fill type.of.hits column
CWC_fill <- CWC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
CWC_long <- pivot_longer(CWC_fill, cols = "bracharia.lacnantha":"panicum.astro", names_to = "Species", values_to = "Sp_Pin_Hits")

CWC_long$Sp_Pin_Hits <- as.numeric(CWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
CWC_final <- CWC_long %>%
  mutate(Treatment = "WC", Block = "C", Date = "20070127") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### Join all final data sheets into one S block datasheet #####################
################################################################################


#vertically join all final data sheets together using rbind
a <- rbind(CC_final, CMW_final)
b <- rbind(a, CMWC_final)
c <- rbind(b, CO_final)
d <- rbind(c, CW_final)
final <- rbind(d, CWC_final)

unique(final$Treatment)
unique(final$Block)
unique(final$Date)
unique(final$type.of.hits)
unique(final$trap.station)
str(final)

write.csv(final, "2007Jan_BlockC.csv", row.names = FALSE)



