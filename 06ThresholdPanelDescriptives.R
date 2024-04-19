## Summary statistics for the minimum documents threshold comparison

library(haven)

## Load the data that we generated and 
N1T1 <- read_dta("./terminationplus_1_1.dta") ## dataframe with at least one doc every year

N10T1 <- read_dta("./terminationplus_10_1.dta") ## dataframe with at least one doc every year

colnames(N10T1)

dim(N1T1)


setdiff(unique(N1T1$sideb),unique(N10T1$sideb))

## How many rows were disqualified in the threshold?
summary(N10T1$modeledarticles) ## 1024 NA
summary(N1T1$modeledarticles) ## 57 NA

## Which groups are there?
length(unique(N10T1[!is.na(N10T1$modeledarticles),]$sidebid)) ## 105
length(unique(N10T1[!is.na(N1T1$modeledarticles),]$sidebid)) ## 285

## 
set10 <- unique(N10T1[!is.na(N10T1$modeledarticles),]$sideb) 
set1 <- unique(N10T1[!is.na(N1T1$modeledarticles),]$sideb)

setdiff(set1, set10) ## which groups are in set1 and not set 10?

dropped10 <- N10T1[is.na(N10T1$modeledarticles),] ## who is dropped? 

droppedfreq <- as.data.frame(table(dropped10$sideb))
droppedfreq <- droppedfreq[rev(order(droppedfreq$Freq)),]

## 10 longest-acting groups dropped by the threshold?
droppedfreq[1:10,]
