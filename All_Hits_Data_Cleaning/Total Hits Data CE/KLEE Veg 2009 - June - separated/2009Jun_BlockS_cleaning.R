################################################################################
###################### BLOCK S #################################################
################################################################################

setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2009 - June - separated")

#load packages
library(tidyverse)
library(janitor)


################################################################################
#### SC June 2009 Data Cleaning #################################################
################################################################################
SC <- read.csv("2009Jun_SC_raw.csv", skip = 4) 


##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
SC2 <- SC[-(2:4),]

#transpose the data frame for easier manipulation
t_SC <- SC2 %>%
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

#subset again to only get useful data -> remove blank rows at end
SC_t <- SC_t[1:70,]

#need to clean the names
SC_clean <- clean_names(SC_t, sep_in = "_")

##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(SC_clean)[colnames(SC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
SC_clean$type.of.hits[SC_clean$type.of.hits==""] <- NA 
SC_clean$trap.station[SC_clean$trap.station==""] <- NA 

#fill type.of.hits column
SC_fill <- SC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")

#flip to long format
SC_long <- pivot_longer(SC_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

SC_long$Sp_Pin_Hits <- as.numeric(SC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SC_final <- SC_long %>%
  mutate(Treatment = "C", Block = "S", Date = "20090627") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SMW June 2009 Data Cleaning ################################################
################################################################################
SMW <- read.csv("2009Jun_SMW_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
SMW2 <- SMW[-(2:4),]


#transpose the data frame for easier manipulation
t_SMW <- SMW2 %>%
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

#remove first row as it now only contains duplicate info. 
SMW_t <- t_SMW[-1,]

#subset again to only get useful data -> remove blank rows at end
SMW_t <- SMW_t[1:70,]

#need to clean the names
SMW_clean <- clean_names(SMW_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(SMW_clean)[colnames(SMW_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
SMW_clean$type.of.hits[SMW_clean$type.of.hits==""] <- NA 
SMW_clean$trap.station[SMW_clean$trap.station==""] <- NA 


#fill type.of.hits column
SMW_fill <- SMW_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
SMW_long <- pivot_longer(SMW_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

SMW_long$Sp_Pin_Hits <- as.numeric(SMW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SMW_final <- SMW_long %>%
  mutate(Treatment = "MW", Block = "S", Date = "20090604") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### SMWC June 2009 Data Cleaning ###############################################
################################################################################
SMWC <- read.csv("2009Jun_SMWC_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
SMWC2 <- SMWC[-(2:4),]

#transpose the data frame for easier manipulation
t_SMWC <- SMWC2 %>%
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

#remove first row as it now only contains duplicate info. 
SMWC_t <- t_SMWC[-1,]

#subset again to only get useful data -> remove blank rows at end
SMWC_t <- SMWC_t[1:70,]

#need to clean the names
SMWC_clean <- clean_names(SMWC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(SMWC_clean)[colnames(SMWC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
SMWC_clean$type.of.hits[SMWC_clean$type.of.hits==""] <- NA 
SMWC_clean$trap.station[SMWC_clean$trap.station==""] <- NA 


#fill type.of.hits column
SMWC_fill <- SMWC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
SMWC_long <- pivot_longer(SMWC_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

SMWC_long$Sp_Pin_Hits <- as.numeric(SMWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SMWC_final <- SMWC_long %>%
  mutate(Treatment = "MWC", Block = "S", Date = "20090706") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### SO June 2009 Data Cleaning #################################################
################################################################################
SO <- read.csv("2009Jun_SO_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
SO2 <- SO[-(2:4),]

#transpose the data frame for easier manipulation
t_SO <- SO2 %>%
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

#remove first row as it now only contains duplicate info. 
SO_t <- t_SO[-1,]

#subset again to only get useful data -> remove blank rows at end
SO_t <- SO_t[1:70,]

#need to clean the names
SO_clean <- clean_names(SO_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(SO_clean)[colnames(SO_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
SO_clean$type.of.hits[SO_clean$type.of.hits==""] <- NA 
SO_clean$trap.station[SO_clean$trap.station==""] <- NA 


#fill type.of.hits column
SO_fill <- SO_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
SO_long <- pivot_longer(SO_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

SO_long$Sp_Pin_Hits <- as.numeric(SO_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SO_final <- SO_long %>%
  mutate(Treatment = "O", Block = "S", Date = "20090629") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### SW June 2009 Data Cleaning #################################################
################################################################################
SW <- read.csv("2009Jun_SW_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
SW2 <- SW[-(2:4),]

#transpose the data frame for easier manipulation
t_SW <- SW2 %>%
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

#subset again to only get useful data -> remove blank rows at end
SW_t <- SW_t[1:70,]

#need to clean the names
SW_clean <- clean_names(SW_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(SW_clean)[colnames(SW_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
SW_clean$type.of.hits[SW_clean$type.of.hits==""] <- NA 
SW_clean$trap.station[SW_clean$trap.station==""] <- NA 


#fill type.of.hits column
SW_fill <- SW_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
SW_long <- pivot_longer(SW_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

SW_long$Sp_Pin_Hits <- as.numeric(SW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SW_final <- SW_long %>%
  mutate(Treatment = "W", Block = "S", Date = "20090703") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)8




################################################################################
#### SWC June 2009 Data Cleaning ################################################
################################################################################
SWC <- read.csv("2009Jun_SWC_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
SWC2 <- SWC[-(2:4),]

#transpose the data frame for easier manipulation
t_SWC <- SWC2 %>%
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

#subset again to only get useful data -> remove blank rows at end
SWC_t <- SWC_t[1:70,]

#need to clean the names
SWC_clean <- clean_names(SWC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(SWC_clean)[colnames(SWC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
SWC_clean$type.of.hits[SWC_clean$type.of.hits==""] <- NA 
SWC_clean$trap.station[SWC_clean$trap.station==""] <- NA 


#fill type.of.hits column
SWC_fill <- SWC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
SWC_long <- pivot_longer(SWC_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

SWC_long$Sp_Pin_Hits <- as.numeric(SWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
SWC_final <- SWC_long %>%
  mutate(Treatment = "WC", Block = "S", Date = "20090701") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




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
unique(final$Date)
unique(final$type.of.hits)
unique(final$trap.station)
str(final)

write.csv(final, "2009Jun_BlockS.csv", row.names = FALSE)



