rm(list=ls())

## This script for a threshold of 10 events
## probably introducing noise at the 10-25 range
## but hoping to get more coverage for the recurrence
## replication


## Load packages to clean and work with data

library(ggplot2)
library(tidyverse)
library(readxl)


source("./00articleModelingRep.R") ## 4/22: this is the version that replicates the findings. Original stopword list & quandeda cleaning
source("./00dominantFramingRep.R") ## wrapper to summarize yearly STM

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### 2020 UCDP; good underlying base to
## try out strategies for measuring change

dataPath <- "./data/"
outPath <- "./output/"

ucdp.ged<- read.csv(paste0(dataPath,"ged211.csv"))

ucdp.acd <- read.csv(paste0(dataPath, "ucdp-prio-acd-211.csv"))

ucdp.actor <- read_excel(paste0(dataPath, "ucdp-actor-211.xlsx"))

dim(ucdp.acd)  ##2506;
dim(ucdp.ged) ## 261864 x 49

nonstate.actors <- ucdp.actor[which(ucdp.actor$Org==1),]
nonstate.actorList <- unique(nonstate.actors$ActorId)

length(nonstate.actorList) ## 1016

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

dim(acd.nonstate) ## 2370 x 28 conflict-years

table(acd.nonstate$type_of_conflict)
      ## 117 extrasystemtic conflict-years;; 1864 intrastate conflict years;
      ##349 internationalized interstate conflict years

length(unique(ucdp.acd$conflict_id)) ## 292 conflicts including state-state
length(unique(acd.nonstate$conflict_id)) ## 244 conflicts that are state-nonstate
length(unique(acd.nonstate$side_b_id)) ##593
 
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
length(unique(ucdp.subset$side_b_dset_id))
length(unique(ucdp.subset$dyad_new_id))

actor.freq <- as.data.frame(table(ucdp.subset$side_b))
actor.freq <- actor.freq[order(actor.freq$Freq),]

dim(actor.freq) ## 352x 2

tail(actor.freq) ## Syrian insurgents; Kashmir insurgents;
print(actor.freq)

dim(actor.freq[which(actor.freq$Freq > 10),]) 
## 258 with more than 10
dim(actor.freq[which(actor.freq$Freq > 50),]) 
## 147 with more than 50
dim(actor.freq[which(actor.freq$Freq > 100),]) 
## 105 with more than 100

groups.tiny <- as.character(actor.freq[which(
    actor.freq$Freq >= 10), "Var1"])

ged.tiny <- ucdp.subset[which(
    ucdp.subset$side_b %in% groups.tiny),]

dim(ged.tiny) ## 190830
#############################

## Text analysis:
## dominantFraming.R is a wrapper around
## (1) a call to topic model K=2 for a UCDP actor
## and then a series of summaries that produces yearly counts of the
## dominant topic.
## Returns group-year dominant discourse topic,
## topic proportions, and the first 7 REX words of the dominant topic


uids <- unique(ged.tiny$side_b_dset_id)

tiny.yearsum <- list()
tiny.basedata <- list()
i=1

for(u in uids){
    print(paste0("iteration: ", i))
    analysis<- dominantFraming(groupID= u,
                               ucdpdata=ged.tiny)
    
    tiny.yearsum[[i]] <- analysis$yearsum
    tiny.basedata[[i]] <- analysis$basedata
   i=i+1 
}

grep(277, tiny.yearsum)

grep(326, tiny.yearsum)

as <- as.data.frame(tiny.yearsum[179])

ulfa <- as.data.frame(tiny.yearsum[132])

save(tiny.yearsum, tiny.basedata,
     file=paste0(outPath,
         "01measurementScaleUpInProgressTiny55.Rdata"))

print("Script 1 Finished, data saved")