rm(list=ls())

## Robustness from R2:
## Version of change measure that imposes a minimum threshold 
## number of documents per year

## Plan of attack:
## Create dataset of all groups which clear an 
## {N} threshold of articles/year
## run model on this set

## use that to run the replication; see if the results are similar

rm(list=ls())
##Libraries and helper files:

library(ggplot2)
library(tidyverse)
library(readxl)


source("./00articleModelingRep.R") 
source("./00dominantFramingRep.R") ## wrapper to summarize yearly STM

dataPath <- "./data/"
outPath <- "./output/"

ucdp.ged<- read.csv(paste0(dataPath,"ged211.csv"))

ucdp.acd <- read.csv(paste0(dataPath, "ucdp-prio-acd-211.csv"))

ucdp.actor <- read_excel(paste0(dataPath, "ucdp-actor-211.xlsx"))

nonstate.actors <- ucdp.actor[which(ucdp.actor$Org==1),]
nonstate.actorList <- unique(nonstate.actors$ActorId)

## GED/ACD want conflicts that are extrasystemic (1);
##intrastate (3); and internationalized interstate (4)
## In all conflicts other than type 2 the side_b is a rebel group

## Goal: 
## identify candidate conflicts and actors in the
## armed conflict data (which is in conflict-year)
## & list conflicts and actors 
## Use that list to subset event data

table(ucdp.acd$type_of_conflict)

types <- c(1, 3, 4)

acd.nonstate <- ucdp.acd[which(
  ucdp.acd$type_of_conflict %in% types),]

substate.conflicts <- unique(acd.nonstate$conflict_id)
length(substate.conflicts) ## 244

## First: violent events as part of existing substate conflicts:
ucdp.subset <- ucdp.ged[which(
  ucdp.ged$conflict_new_id %in% substate.conflicts),]
## Second, those events carried out by organized, non-state, armed actors:
ucdp.subset <- ucdp.subset[which(
  ucdp.subset$side_b_dset_id %in% nonstate.actorList),]

dim(ucdp.subset) ## 191252
summary(ucdp.subset$year)

## Frequency of events associated with actors:
## by year:

colnames(ucdp.subset)

actor.freq <- ucdp.subset %>% 
    dplyr::group_by(side_b, year, side_b_new_id) %>%
  summarize(year.events=n())

## Keep only those groups with  >= N events for every year active:
N=5

actor.freq$geqN <- ifelse(actor.freq$year.events >= N, 
                           "yes", "no")

propSummary <- actor.freq %>% 
  group_by(side_b, side_b_new_id) %>%
  count(geqN) %>% mutate(prop = prop.table(n))

active9 <- propSummary[which(propSummary$prop > .9),]
active75 <- propSummary[which(propSummary$prop > .75),]

rm(active2)

dim(active9) ##168 groups with 90\% or more of their years 
## with more than 5 events
dim(active75)## 198 groups with more than 75\% years with more than 5 events
dim(propSummary) ##558 groups 

## IDs for group
## OBSERVE: This pretty much gets rid of joint events
## but those are written about together-- one article, several groups
## so it's unclear how tightly the article would be linked to the 
## activities of each constituent org.

subset9 <- unique(active9$side_b_new_id)
subset75 <- unique(active75$side_b_new_id)

print(subset9)
print(subset75)

class(subset9)

ged9 <- ucdp.subset[which(
  ucdp.subset$side_b_new_id %in% subset9),]

ged75 <- ucdp.subset[which(
  ucdp.subset$side_b_new_id %in% subset75),]

## extract both data:

uids9 <- unique(ged9$side_b_new_id)
uids75 <- unique(ged75$side_b_new_id)

## Generate data for the .9 years with more than 5 events
## criteria:

yearsum9 <- list()
basedata9 <- list()
i=1

for(u in uids9){
  print(paste0("iteration: ", i))
  analysis<- dominantFraming(groupID= u,
                             ucdpdata=ged9)
  
  yearsum9[[i]] <- analysis$yearsum
  basedata9[[i]] <- analysis$basedata
  i=i+1 
}


## Generate data for the 75% of years with more than 
## 5 events criteria:
## (Each approximately 45 minutes on 32G Apple M1 2021 Macbook Pro)
yearsum75 <- list()
basedata75 <- list()
i=1

for(u in uids75){
  print(paste0("iteration: ", i))
  analysis<- dominantFraming(groupID= u,
                             ucdpdata=ged75)
  
  yearsum75[[i]] <- analysis$yearsum
  basedata75[[i]] <- analysis$basedata
  i=i+1 
}

save(yearsum9, basedata9,
     file=paste0(outPath,
                 "Robustness9Data.Rdata"))

save(yearsum75, basedata75,
     file=paste0(outPath,
                 "Robustness75Data.Rdata"))

### Checkpoint 1
print("Script 1 Finished, data saved")

outPath <- "./output/"

##rm(list=ls())

class(basedata9) ## list

length(basedata9)
##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## From Script 2:

df.basedata9 <- do.call("rbind",basedata9)
df.yearsum9 <- do.call("rbind",yearsum9)

df.basedata75 <- do.call("rbind",basedata75)
df.yearsum75 <- do.call("rbind",yearsum75)

df.yearsum9$modeledarticles <- df.yearsum9$countT1+
  df.yearsum9$countT2

summary(df.yearsum9$modeledarticles) ## 0-17735; median: 8; mean = 83

## Right tail:
print(df.yearsum9[which(df.yearsum9$modeledarticles>1500),], n=23)
## With one exception, thes are all very recent articles (after 2011)
## and seem to be Taliban (actor 303), Syrian insurgents (4456),
## IS (actor 234). The exception is the Serbian Republic of Bosnia-Herzogovina (actor 339) in 1992.

## Left tail:
print(df.yearsum9[which(df.yearsum9$modeledarticles==0),], n=53) 

## 53. Removing these from the data, because there isn't any post-modeling
## analysis that can be done in this set


## What do the 0 article years do to my lag variables:
## (A: introduce NAN values)

print(df.yearsum9[which(df.yearsum9$groupID==223),],n=24)## for this group the no
## modeled article years are a cluster in the center of a set of years with
## only one article; ads one NAN year of propdif.L1 at the end of the streak

print(df.yearsum9[which(df.yearsum9$groupID==169),],n=32)##

## Convert NAN and NA to 0.
## NAN are years with no articles. Don't want to drop them because that
## distorts the year structure of the data
## But equally need to skip over those zeros when I take the lag
## Note that there are 67 of these years; so pretty rare

df.yearsum9[which(is.nan(df.yearsum9$propT1)==TRUE|
                   is.na(df.yearsum9$propT1)==TRUE), "propT1"] <- 0## No NAN or NA in prop difference


df.yearsum9[which(is.nan(df.yearsum9$propT2)==TRUE|
                   is.na(df.yearsum9$propT2)==TRUE), "propT2"] <- 0## No NAN or NA in prop difference


df.yearsum9[which(is.nan(df.yearsum9$propdiff)==TRUE|
                   is.na(df.yearsum9$propdiff)==TRUE), "propdiff"] <- 0## No NAN or NA in prop difference

df.yearsum9[which(is.nan(df.yearsum9$frexWords)==TRUE|
                   is.na(df.yearsum9$frexWords)==TRUE), "frexWords"] <- "None"## No NAN or NA in prop difference


df.yearsum9[which(df.yearsum9$modeledarticles==0),]

summary(df.yearsum9$modeledarticles) ##0-17,735

##Reindex the lag to skip the rows with no modeled articles

zeros <- df.yearsum9[which(df.yearsum9$modeledarticles==0),]
zeros$propdif.L1 <- 0 ## placeholder for no change from previous year.

df.yearsum2 <- df.yearsum9 %>%
  group_by(groupID) %>%
  arrange(year, .by_group=TRUE) %>%
  filter(modeledarticles > 0) %>%
  mutate(propdif.L1 = propdiff - lag(propdiff, n=1L)) %>%
  mutate(propdif.L2 = propdiff - lag(propdiff, n=2L))

## Reattach the 0 article years:

df.yearsum9 <- rbind(df.yearsum2,
                    zeros)


dim(df.yearsum9)

## Convert NAN and NA to zero
## NAN are caused by years with no articles
## NA ae usually first years in the data

df.yearsum9[is.finite(df.yearsum9$propdif.L1)==FALSE,
           "propdif.L1"] <- 0

print(df.yearsum9[which(df.yearsum9$modeledarticles==0),], n=100)

###%%%%%%%%%%%%%%%%%%%%%%%
## Create Delta measure
## Make an indicator for group-years with:
## propdiff L1:
## (a) greater than = |1|
## (b) Between |1| and |1.5|
## (c) between |1.5| and |2|
## (d) equal to |2|


df.yearsum9$delta1 <- 0 ## group-years with change over 1:
df.yearsum9$delta1<- ifelse(abs(df.yearsum9$propdif.L1)>= 1, 1, 0)

table(df.yearsum9$delta1) ## 343 years of change

round(prop.table(table(df.yearsum9$delta1)), 2) ## 84% no; 16% yes

hadgap1 <- unique(df.yearsum9[df.yearsum9$delta1==1,]$groupID) ## 157

length(hadgap1)/length(unique(df.yearsum9$groupID))

## group-years with a lag-to-now change 1.5 or larger:
df.yearsum9$delta1.5 <- 0
df.yearsum9$delta1.5<- ifelse(abs(df.yearsum9$propdif.L1) >= 1.5,
                             1, 0)

table(df.yearsum9$delta1.5) ## 181 group-years; 
length(unique(df.yearsum9[which(df.yearsum9$delta1.5==1),]$groupID)) ## 102 unque groups

## list of groups with a 1.5 gap:
hadgap1.5 <- unique(df.yearsum9[df.yearsum9$delta1.5==1,]$groupID)

## group-years with a one year lag change
## of 2
df.yearsum9$delta2 <- 0
df.yearsum9$delta2<- ifelse(abs(df.yearsum9$propdif.L1) == 2,
                           1, 0)

table(df.yearsum9$delta2) ##118 years in 68 different groups
## (versus 38 years in 33 different groups)

length(unique(df.yearsum9[which(df.yearsum9$delta2==1),]$groupID))

hadgap2 <- unique(df.yearsum9[df.yearsum9$delta2==1,]$groupID)

##%%%%%%%%%%%%%%%%%%%%%%%%%
## Two-year Lag Changes:
##%%%%%%%%%%%%%%%%%%%%%%%%
## make a window for groups that might not have made the change threshold
## across one year but did across two:

df.yearsum9$delta1_L2 <- 0 ## group-years with a change of at least |1| over
## a two-year change:
df.yearsum9[which(abs(df.yearsum9$propdif.L2)>= 1 |
                   abs(df.yearsum9$propdif.L1) >=1), "delta1_L2"] <- 1

table(df.yearsum9$delta1_L2) ## 554 years with the two-year window of change
round(prop.table(table(df.yearsum9$delta1_L2)),2)

hadgap1.l2 <- unique(df.yearsum9[df.yearsum9$delta1_L2==1,]$groupID) ## 172 groups

length(hadgap1.l2)
length(unique(df.yearsum9$groupID))

setdiff(hadgap1.l2, hadgap1) 

## group-years with a lag-to-now change 1.5 or larger:

df.yearsum9$delta15_L2 <- 0

df.yearsum9[which(abs(df.yearsum9$propdif.L2)>= 1.5 |
                   abs(df.yearsum9$propdif.L1) >=1.5),
           "delta15_L2"] <- 1

table(df.yearsum9$delta15_L2) ## 317 group-years; 

length(unique(df.yearsum9[which(df.yearsum9$delta15_L2==1),]$groupID)) ## 120 unque groups

## list of groups with a 1.5 gap:
hadgap15L2 <- unique(df.yearsum9[df.yearsum9$delta15_L2==1,]$groupID)

## group-years with a one year lag change
## of 2
df.yearsum9$delta2_L2 <- 0

df.yearsum9[which(abs(df.yearsum9$propdif.L2)>= 2 |
                   abs(df.yearsum9$propdif.L1) >=2), "delta2_L2"] <- 1


table(df.yearsum9$delta2_L2) ##196 years in 68 different groups

length(unique(df.yearsum9[which(df.yearsum9$delta2_L2==1),]$groupID)) ##80

hadgap2L2 <- unique(df.yearsum9[df.yearsum9$delta2_L2==1,]$groupID)


##############################
## Measure 2: Ambiguity:
## Identifies group-years where the radio of T1:T2 is
## very close to 1:1
## which might indicate some sort of underlying
## observer uncertainty about how to classify
## the group

## Conversely,in a group with a very consistent
## framing: the design of the algorithm means that there will be
## two topics assigned to the activities of that group.
## However, we would expect the dominant perception and framing
## to be most of what the model assigns.
## So T1 or T2 will be a very high proportion, consistent
## each yea

## gap between the topics is less than .25:
## AND a non-zero number of articles modeled
df.yearsum9$gap25 <- 0
df.yearsum9[which(abs(df.yearsum9$propdiff) <= .25 &
                   df.yearsum9$modeledarticles >0), "gap25"] <- 1

table(df.yearsum9$gap25) ## 10 event threshold: 1882  no, 231  yes

length(unique(df.yearsum9[which(df.yearsum9$gap25==1),]$groupID))
## 140 group-years between the topics is less than .25

## Gap less than .5 & pos. number of articles modeled
df.yearsum9$gap50 <- 0
df.yearsum9[which(abs(df.yearsum9$propdiff) <= .5 &
                   df.yearsum9$modeledarticles >0), "gap50"] <- 1


## gap between the topics is less than .5
table(df.yearsum9$gap50)## 10 threshold: 1664  no, 449 yes in 176 groups

length(unique(df.yearsum9[which(df.yearsum9$gap50==1),]$groupID))

##%%%%%%%%%%%%%%%%%%%%%%%%%
## Per-group count
## years between framing change periods

colnames(df.yearsum9)

cols <-  c("year",
           "countT1", "countT2",
           "propdiff","propdif.L1",
           "delta1", "delta1_L2","gap25")

## highlight the need for a two-year lag
## because the shifts in framing are complete over two years
## but just barely under the |1| framing
## 223: NSCN-IM (Naga Insurgent Group)
print(df.yearsum9[which(df.yearsum9$groupID==223),
                 cols ],n=24)


print(df.yearsum9[which(df.yearsum9$groupID==551),
                 cols ],n=24)

tmp <- df.yearsum9[which(df.yearsum9$groupID==551),
                  cols ]

colnames(tmp)

tmp$counter <- NA

for(r in 1:dim(tmp)[1]){
  print(r)
  if(r==1){
    tmp[r, "counter"] <- 0
  } 
  else{
    if(tmp[r-1, "delta1"]==1){
      tmp[r,"counter"] <- 1
    }else{
      if(tmp[r-1, "counter"] > 0){
        tmp[r, "counter"] <- tmp[r-1, "counter"] + 1
      }else{
        if(tmp[r-1, "counter"]==0){
          tmp[r, "counter"] <- 0
        }
      }
    }
  }
}

## zero out the delta1 years:
tmp[which(tmp$delta1==1), "counter"] <- 0
tmp$counter <- NA

## which rows have 1:
ind <- which(tmp$delta1==1)


## Nested loops to iterate through the list of groups
## and create a counter:
## Counter has: 0 until a change
## 0 in the year that delta = 1
## plus 1 for every year until the next change 
## [Not efficient, but more interpretable]

df.yearsum9$yearsSinceChange <- NA
df.yearsum9$counter <- NA
for(g in unique(df.yearsum9$groupID)){
  print(g)
  for(r in 1:dim(df.yearsum9[which(df.yearsum9$groupID==g),])[1]){
    print(r)
    if(r==1){
      df.yearsum9[which(df.yearsum9$groupID==g),][r, "counter"] <- 0
    } 
    else{
      if(df.yearsum9[which(df.yearsum9$groupID==g),][r-1, "delta1"]==1){
        df.yearsum9[which(df.yearsum9$groupID==g),][r,"counter"] <- 1
      }else{
        if(df.yearsum9[which(df.yearsum9$groupID==g),][r-1, "counter"] > 0){
          df.yearsum9[which(df.yearsum9$groupID==g),][r, "counter"] <- df.yearsum9[which(df.yearsum9$groupID==g),][r-1, "counter"] + 1
        }else{
          if(df.yearsum9[which(df.yearsum9$groupID==g),][r-1, "counter"]==0){
            df.yearsum9[which(df.yearsum9$groupID==g),][r, "counter"] <- 0
          }
        }
      }
    }
  }
}


## now zero out "counter" for years of change:

df.yearsum9[which(df.yearsum9$delta1==1), "counter"] <- 0

## But: need to zero out the groups with no changes
## and, for those with a change,
## also from the start until the first delta change

##%%%%%%%%%%%%%%%
## Good idea to get a qualitative handle on some groups:

##set.seed(31622)
##sample(hadgap1, 5)

##print(df.yearsum9[which(df.yearsum9$groupID==223),
##                 c(cols[1:6], "counter")],n=24)

##print(df.yearsum9[which(df.yearsum9$groupID==205),
##                 c(cols[1:6], "counter")],n=24)

##print(df.yearsum9[which(df.yearsum9$groupID==504),
##                c(cols[1:6], "counter")],n=24)

## MRTA, Peru, co-oped by drug traffickers in early 1990s
#print(df.yearsum9[which(df.yearsum9$groupID==751),
##                c(cols[1:6], "counter")],n=24)


##print(df.yearsum9[which(df.yearsum9$groupID==211),
##                c(cols[1:6], "counter")],n=24)


save(df.yearsum9, df.basedata9, hadgap2, hadgap1.5,
     file=paste0(outPath,"02dfyearsumAndRelatedTinyUpdate.Rdata"))
)