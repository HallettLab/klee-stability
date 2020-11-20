################################################################################
###################### BLOCK N #################################################
################################################################################

setwd("~/Repositories/klee-stability/All_Hits_Data_Cleaning/Total Hits Data CE/KLEE Veg 2009 - June - separated")

#load packages
library(tidyverse)
library(janitor)


################################################################################
#### NC June 2009 Data Cleaning ################################################
################################################################################
NC <- read.csv("2009Jun_NC_raw.csv", skip = 4)


##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
NC2 <- NC[-(2:4),]

#transpose the data frame for easier manipulation
t_NC <- NC2 %>%
  t() %>%
  as.data.frame() #save this as a dataframe

row.names(t_NC) <- 1:nrow(t_NC) #get rid of weird rownames

#need to make the first row column names, but first need to make all names unique
#create vector from first row
col <- as.vector(t_NC[1,])

#make character 
col.nc <- as.character(col)

#make each value unique, there are a few columns with duplicate names 
#(these are not species names, rather heights & will need to be removed later)
make.unique(col.nc, sep = ".")

#set as colnames
colnames(t_NC) <- col.nc

#remove first row as it now only contains duplicate info. 
NC_t <- t_NC[-1,]

NC_t <- NC_t[1:70,] #remove empty rows at the bottom of the dataframe


#need to clean the names
NC_clean <- clean_names(NC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column
colnames(NC_clean)[colnames(NC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of NC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
NC_clean$type.of.hits[NC_clean$type.of.hits==""] <- NA 
NC_clean$trap.station[NC_clean$trap.station==""] <- NA 


#fill type.of.hits & trap.station columns
NC_fill <- NC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")

#flip to long format
NC_long <- pivot_longer(NC_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

NC_long$Sp_Pin_Hits <- as.numeric(NC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NC_final <- NC_long %>%
  mutate(Treatment = "C", Block = "N", Date = "20090617") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NMW June 2009 Data Cleaning ###############################################
################################################################################
NMW <- read.csv("2009Jun_NMW_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
NMW2 <- NMW[-(2:4),]


#transpose the data frame for easier manipulation
t_NMW <- NMW2 %>%
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

#remove first row as it now only contains duplicate info. 
NMW_t <- t_NMW[-1,]

#subset again to only get useful data -> remove blank rows at end
NMW_t <- NMW_t[1:70,]

#need to clean the names
NMW_clean <- clean_names(NMW_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(NMW_clean)[colnames(NMW_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
NMW_clean$type.of.hits[NMW_clean$type.of.hits==""] <- NA 
NMW_clean$trap.station[NMW_clean$trap.station==""] <- NA 


#fill type.of.hits column
NMW_fill <- NMW_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
NMW_long <- pivot_longer(NMW_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

NMW_long$Sp_Pin_Hits <- as.numeric(NMW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NMW_final <- NMW_long %>%
  mutate(Treatment = "MW", Block = "N", Date = "20090616") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NMWC June 2009 Data Cleaning ##############################################
################################################################################
NMWC <- read.csv("2009Jun_NMWC_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
NMWC2 <- NMWC[-(2:4),]

#transpose the data frame for easier manipulation
t_NMWC <- NMWC2 %>%
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

#remove first row as it now only contains duplicate info. remove blank 2nd column
NMWC_t <- t_NMWC[-1,]

#subset again to only get useful data -> remove blank rows at end
NMWC_t <- NMWC_t[1:70,]

#need to clean the names
NMWC_clean <- clean_names(NMWC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(NMWC_clean)[colnames(NMWC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
NMWC_clean$type.of.hits[NMWC_clean$type.of.hits==""] <- NA 
NMWC_clean$trap.station[NMWC_clean$trap.station==""] <- NA 


#fill type.of.hits column
NMWC_fill <- NMWC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
NMWC_long <- pivot_longer(NMWC_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

NMWC_long$Sp_Pin_Hits <- as.numeric(NMWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NMWC_final <- NMWC_long %>%
  mutate(Treatment = "MWC", Block = "N", Date = "20090615") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NO June 2009 Data Cleaning ################################################
################################################################################
NO <- read.csv("2009Jun_NO_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
NO2 <- NO[-(2:4),]

#transpose the data frame for easier manipulation
t_NO <- NO2 %>%
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

#remove first row as it now only contains duplicate info. 
NO_t <- t_NO[-1,]

#subset again to only get useful data -> remove blank rows at end
NO_t <- NO_t[1:70,]

#need to clean the names
NO_clean <- clean_names(NO_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(NO_clean)[colnames(NO_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
NO_clean$type.of.hits[NO_clean$type.of.hits==""] <- NA 
NO_clean$trap.station[NO_clean$trap.station==""] <- NA 


#fill type.of.hits column
NO_fill <- NO_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
NO_long <- pivot_longer(NO_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

NO_long$Sp_Pin_Hits <- as.numeric(NO_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NO_final <- NO_long %>%
  mutate(Treatment = "O", Block = "N", Date = "20090617") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)




################################################################################
#### NW June 2009 Data Cleaning #################################################
################################################################################
NW <- read.csv("2009Jun_NW_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
NW2 <- NW[-(2:4),]

#transpose the data frame for easier manipulation
t_NW <- NW2 %>%
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

#remove first row as it now only contains duplicate info. 
NW_t <- t_NW[-1,]

#subset again to only get useful data -> remove blank rows at end
NW_t <- NW_t[1:70,]

#need to clean the names
NW_clean <- clean_names(NW_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column 
colnames(NW_clean)[colnames(NW_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
NW_clean$type.of.hits[NW_clean$type.of.hits==""] <- NA 
NW_clean$trap.station[NW_clean$trap.station==""] <- NA 


#fill type.of.hits column
NW_fill <- NW_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")


#flip to long format
NW_long <- pivot_longer(NW_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

NW_long$Sp_Pin_Hits <- as.numeric(NW_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NW_final <- NW_long %>%
  mutate(Treatment = "W", Block = "N", Date = "20090618") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### NWC June 2009 Data Cleaning ###############################################
################################################################################
NWC <- read.csv("2009Jun_NWC_raw.csv", skip = 4)

##cut out rows 2-4 -> one is blank, the other is for disturbance but has no values
NWC2 <- NWC[-(2:4),]

#transpose the data frame for easier manipulation
t_NWC <- NWC2 %>%
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

#remove first row as it now only contains duplicate info. 
NWC_t <- t_NWC[-1,]

#subset again to only get useful data -> remove blank rows at end
NWC_t <- NWC_t[1:70,]

#need to clean the names
NWC_clean <- clean_names(NWC_t, sep_in = "_")


##need to fill rows to carry over trap station and type of hit data
#rename 'x' column (col #5)
colnames(NWC_clean)[colnames(NWC_clean) == 'x'] <- 'hit_type_plant'

#save blank values of SC_clean as NA in the type.of.hits & trap.station columns 
#(need NA values for fill function to work, it seems)
NWC_clean$type.of.hits[NWC_clean$type.of.hits==""] <- NA 
NWC_clean$trap.station[NWC_clean$trap.station==""] <- NA 


#fill type.of.hits column
NWC_fill <- NWC_clean %>%
  fill(type.of.hits, .direction = "down") %>%
  fill(trap.station, .direction = "down")



#flip to long format
NWC_long <- pivot_longer(NWC_fill, cols = "bracharia.lacnantha":"x_8", names_to = "Species", values_to = "Sp_Pin_Hits")

NWC_long$Sp_Pin_Hits <- as.numeric(NWC_long$Sp_Pin_Hits) #make the # of pin hits numeric

#make block, date, treatment columns
NWC_final <- NWC_long %>%
  mutate(Treatment = "WC", Block = "N", Date = "20090619") #add columns for treatment, block, date (using the first of Feb as sample day is not specified)



################################################################################
#### Join all final data sheets into one N block datasheet #####################
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
unique(final$type.of.hits)
unique(final$trap.station)
str(final)

write.csv(final, "2009Jun_BlockN.csv", row.names = FALSE)



