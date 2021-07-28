library(vegan)

###KLEE Analyses   
data <- read.csv ("C:/Users/A01515674/Dropbox/Kari_Directory/research/Projects_Kenya/KLEE/MS_vegetation/KLEE_MS-2013Effort/PRC-NMDS/KVSR3-final.csv", sep = ',', header = T)

## Carmen's updates - reading in the dataframe Kari sent
kv7 <- read.csv("KV7-final.csv")
head(kv7)


View(data)
str(data)
#kv7<-data
kv8<-kv7
plot<-gl(21,1,length=483)
block<-gl(3,7,length=483)
###gl(number levels, number reps, max length)
#Here there are 21 plots, 18 plus one for level for the "reference" - MWC from 1999
#3 blocks, each with 7 reps (1 per trt plus "reference").
#and 23 time periods
#483=21*23
month <-gl(23,21, labels=c(0,13,17,24,28,37,41,49,61,65,73,77,86,89,97,101,109,
                           113,121,125,132,137,149))
###here there are 23 time periods, each has 21 values (one per plot). 
#Parenthetical values are months (need to be adjusted to match KLEE data)
treatment<-factor(rep(c(0,1,2,3,4,5,6),69)) #1=static ref, 2=C, 3=MW, 4=MWC, 5=O, 6=W, 7=WC
###order of 6 treatments (*3 blocks* 23 time periods)
#here I just assigned numbers to each treatment for simplicity

#PRC
kleemod<-prc(kv8, treatment,month)#(response, treatment, time)
kleemod #RDA
summary(kleemod)#PRC
abundance <- colSums(kv8)# this adds up, by column, the abundances of original matrix
plot(kleemod)
plot(kleemod, select = abundance > 5 )# the 5 indicates that only species with >5 total abundance are plotted 
#The species weights are the correlation with the treatment effects.
#The higher the (abs value) weight, the more the
#actual response pattern of the species is likely to follow the
#pattern in the PRC.

kleemod #prc is special case of model: rda(response~treatment*time+Condition(time))
#time (i.e., conditional) explains 33% of the variance
#treatment and its interaction with time (i.e., constrained) explains 41.8% of the variance

kleemod$CCA$eig/sum(kleemod$CCA$eig)#First axis explains 6% and 
#second 1% of variance

anova(kleemod) # overall model sig; this uses vegan's anova.cca function
anova (kleemod, strata=month, first=TRUE, perm.max=9999) # first axis sig
anova(kleemod, by="terms", permu=100) # treatment sig, and intxn sig

#montecarlo permutation test for treatment differences within time periods; sequential
Ftests<-NULL
for (i in levels(month )){
  timeper<-kv8[month ==i,]
  trt<-treatment[month ==i]
  Ftests [[i]] <- anova(rda(timeper ~ trt), by = "terms", step =1000)
}

Ftests
sapply (Ftests, function(x) x[1,5])#extracts pvalues for each date.

#month effects within treatment
Ftests2<-NULL
for (i in levels(treatment )){
  trt2<-kv8[treatment==i,]
  timeper2<-month[treatment ==i]
  Ftests2 [[i]] <- anova(rda(trt2 ~ timeper2), by = "terms", step =1000)
}

Ftests2
sapply (Ftests2, function(x) x[1,5])


