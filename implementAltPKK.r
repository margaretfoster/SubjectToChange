rm(list=ls())

## Check alternate specifications for
## PKK

library(ggplot2)
library(tidyverse)
library(readxl)

source("articleModelingAlt.R") ## wrapper for text prep + model with K as a parameter

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### 2020 UCDP; good underlying base to
## try out strategies for measuring change

dataPath <- "~/Dropbox/Data/"

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
## general subset:
ged.pkk <-ged.tiny[which(ged.tiny$side_b_dset_id==323),]

ged.aqap <-ged.tiny[which(ged.tiny$side_b_dset_id==881),]
ged.lra <-ged.tiny[which(ged.tiny$side_b_dset_id==488),]
ged.onlf <-ged.tiny[which(ged.tiny$side_b_dset_id==497),]

## Subset that matches the cases I spotlighted in the draft:
ged.as <-ged.tiny[which(ged.tiny$side_b_dset_id==277),]
ged.ulfa <-ged.tiny[which(ged.tiny$side_b_dset_id==326),]
##ged.unita <-ged.tiny[which(ged.tiny$side_b_dset_id==),]

##%%%%%%%%%%%%%%%%%%%
## Check PKK around K=5
## For what conclusions one might draw
##%%%%%%%%%%%%%%%%%%%

## K=2 [normal spec]

pkkmain <- article.analysis.alt(ged.pkk,k=5)

topics <- labelTopics(pkkmain$model) 

topics

summary(pkkmain$results)
table(pkkmain$results$maxtopic) ##

datk5 <- merge(ged.pkk,
               pkkmain$results,
               by.x="id",
               by.y="id",
               all.x=TRUE)

dim(datk5)
colnames(datk5)
table(datk5$maxtopic)
summary(datk5$scaledvalue)

k5.yearsum <- datk5 %>%
    group_by(year) %>%
    summarize(
        countT1 = sum(maxtopic==1, na.rm = T),
        countT2 =  sum(maxtopic==2, na.rm = T),
        countT3 =  sum(maxtopic==3, na.rm = T),
        countT4 =  sum(maxtopic==4, na.rm = T),
        countT5 =  sum(maxtopic==5, na.rm = T))


denom <- k5.yearsum$countT1+
    k5.yearsum$countT2 +
    k5.yearsum$countT3 +
    k5.yearsum$countT4 +
    k5.yearsum$countT5
                           
k5.yearsum$propT1 <- round(k5.yearsum$countT1/denom, 2)
k5.yearsum$propT2 <- round(k5.yearsum$countT2/denom, 2)
k5.yearsum$propT3 <- round(k5.yearsum$countT3/denom , 2)
k5.yearsum$propT4 <- round(k5.yearsum$countT4/denom, 2)
k5.yearsum$propT5 <- round(k5.yearsum$countT5/denom, 2)
k5.yearsum

##Find max:
propcols <- c("propT1", "propT2",
              "propT3", "propT4" ,"propT5")
k5.yearsum$maxTopic <- colnames(k5.yearsum[,propcols])[max.col(k5.yearsum[, propcols],ties.method="first")]

k5.yearsum$maxTopic <- gsub(k5.yearsum$maxTopic,
                            pattern="propT",
                            replace="")

topics <- labelTopics(pkkmain$model, topics=c(1:5), n=10)

t1frex <-  paste0(topics$frex[1,], collapse=",")
t2frex <-  paste0(topics$frex[2,], collapse=",")
t3frex <-  paste0(topics$frex[3,], collapse=",")
t4frex <-  paste0(topics$frex[4,], collapse=",")
t5frex <-  paste0(topics$frex[5,], collapse=",")

k5.yearsum[which(
    k5.yearsum$maxTopic==1), "frexWords"] <- t1frex
k5.yearsum[which(
    k5.yearsum$maxTopic==2), "frexWords"] <-t2frex
k5.yearsum[which(
    k5.yearsum$maxTopic==3), "frexWords"] <- t3frex
k5.yearsum[which(
    k5.yearsum$maxTopic==4), "frexWords"] <-t4frex
k5.yearsum[which(
    k5.yearsum$maxTopic==5), "frexWords"] <-t5frex

table(k5.yearsum$maxTopic)##

## Make into a 2-D set of clusters (C1: Turkey-PKK;
##C2: PKK-Has-Territory)
## cluster 1: 1, 2, 4
## cluster 2: 3, 5

## Scale to -1 for C1

k5.yearsum$propC1 <- -1*((k5.yearsum$countT1 +
    k5.yearsum$countT2+
    k5.yearsum$countT4)/denom)

k5.yearsum$propC2 <- (k5.yearsum$countT3 +
    k5.yearsum$countT5)/denom

k5.yearsum$Cdiff <- k5.yearsum$propC2+
    k5.yearsum$propC1

summary(k5.yearsum$propC2)
summary(k5.yearsum$propC1)
summary(k5.yearsum$Cdiff)

pdf(file="PKKAlternateK5.pdf")
plot(k5.yearsum$Cdiff~k5.yearsum$year,
     main="PKK-Alternate Topic Proportions",
     sub="Clustered Classifications With K-5",
     xlab="Year",
     ylab="Proportion PKK-Turkey Cluster (-1) vs PKK-Territory Cluster (1)",
     ylim=c(-1, 1),
     cex=.75)
lines(k5.yearsum$Cdiff~k5.yearsum$year,
      col="darkgreen",
      type="l",
      lty="dashed")
abline(h=0, col="red")
dev.off()



print(k5.yearsum[,c(1, 12,13)], n=32)

