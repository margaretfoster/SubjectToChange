rm(list=ls())

## Check searchK and then an alternate specification
## for:
## PKK; AQAP; LRA; ONLF
## PKK-- likely K=2 is underspecified
## AQAP: substantive knowledge suggests 3 distinct periods
## ONLF: One very dominant framing
## LRA: This group has no change periods with the K=2 model; would they with a different spec?

## Load packages to clean and work with data

library(ggplot2)
library(tidyverse)
library(readxl)
source("topicSearchK.R") ## wrapper for text prep + searchK
##source("dominantFraming.R") ## wrapper to summarize yearly STM

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### 2020 UCDP; good underlying base to
## try out strategies for measuring change

dataPath <- "./data/"

ucdp.ged<- read.csv(paste0(dataPath,"ged211.csv"))

ucdp.acd <- read.csv(paste0(
    dataPath, "ucdp-prio-acd-211.csv"))

ucdp.actor <- read_excel(paste0(
    dataPath, "ucdp-actor-211.xlsx"))

dim(ucdp.acd)  ##2506;
dim(ucdp.ged) ## 261864 x 49

nonstate.actors <- ucdp.actor[which(ucdp.actor$Org==1),]
nonstate.actorList <- unique(nonstate.actors$ActorId)

length(nonstate.actorList) ## 1016

## GED/ACD want conflicts that are extrasystemic (1);
##intrastsate (3); and interationlized interstate (4)
## In all conflicts other than type 2 the side_b is a rebel group

## What I need to do is identfify candiate conflicts and actors in the
## ACD (which is in conflict-year)
## then make a list of conflicts and actors to use to subset the event data

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
ucdp.subset <- ucdp.subset[which(ucdp.subset$side_b_dset_id %in% nonstate.actorList),]

dim(ucdp.subset) ## 191252
summary(ucdp.subset$year)
length(unique(ucdp.subset$side_b_dset_id))
length(unique(ucdp.subset$dyad_new_id))

actor.freq <- as.data.frame(table(ucdp.subset$side_b))
actor.freq <- actor.freq[order(actor.freq$Freq),]

dim(actor.freq) ## 352x 2

tail(actor.freq) ## Syrian insurgents; Kashmir insurgents;
print(actor.freq)

dim(actor.freq[which(actor.freq$Freq > 10),])  ## 258 with more than 10
dim(actor.freq[which(actor.freq$Freq > 50),])  ## 147 with more than 50
dim(actor.freq[which(actor.freq$Freq > 100),])  ## 105 with more than 100

groups.tiny <- as.character(actor.freq[which(
    actor.freq$Freq >= 10), "Var1"])

ged.tiny <- ucdp.subset[which(
    ucdp.subset$side_b %in% groups.tiny),]

dim(ged.tiny) ## 190830
#############################
## subsets:

ged.pkk <-ged.tiny[which(ged.tiny$side_b_dset_id==323),]
ged.aqap <-ged.tiny[which(ged.tiny$side_b_dset_id==881),]
ged.lra <-ged.tiny[which(ged.tiny$side_b_dset_id==488),]
ged.onlf <-ged.tiny[which(ged.tiny$side_b_dset_id==497),]


## PKK: expectation is that two is too few
sk.pkk <- group.searchK(groupID= "-pkk",
                        data=ged.pkk,
                        k=20)

## AQAP: expectation is that three is more ideal
sk.aqap <- group.searchK(groupID= "-aqap",
                         data=ged.aqap,
                         kmax=20)


## lra: no prior
sk.lra <- group.searchK(groupID= "-lra",
                         data=ged.lra,
                        kmax=20)

## ONLF: no prior
sk.onlf <- group.searchK(groupID= "-onlf",
                         data=ged.onlf,
                         kmax=20)


