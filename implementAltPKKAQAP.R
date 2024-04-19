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

dataPath <- "./"

ucdp.ged<- read.csv(paste0(dataPath,"ged211.csv"))

ucdp.acd <- read.csv(paste0(
    dataPath, "ucdp-prio-acd-211.csv"))

ucdp.actor <- read_excel(paste0(
    dataPath, "ucdp-actor-211.xlsx"))


nonstate.actors <- ucdp.actor[which(ucdp.actor$Org==1),]
nonstate.actorList <- unique(nonstate.actors$ActorId)

## Identify candidate conflicts and actors in the
## ACD (which is in conflict-year)
## then make a list of conflicts and actors to use to subset the event data

types <- c(1, 3, 4)

acd.nonstate <- ucdp.acd[which(
    ucdp.acd$type_of_conflict %in% types),]


substate.conflicts <- unique(acd.nonstate$conflict_id)
##length(substate.conflicts) ## 244

## First: violent events as part of existing substate conflicts:
ucdp.subset <- ucdp.ged[which(
    ucdp.ged$conflict_new_id %in% substate.conflicts),]
## Second, those events carried out by organized, non-state, armed actors:
ucdp.subset <- ucdp.subset[which(ucdp.subset$side_b_dset_id %in% nonstate.actorList),]

actor.freq <- as.data.frame(table(ucdp.subset$side_b))
actor.freq <- actor.freq[order(actor.freq$Freq),]

groups.tiny <- as.character(actor.freq[which(
    actor.freq$Freq >= 10), "Var1"])

ged.tiny <- ucdp.subset[which(
    ucdp.subset$side_b %in% groups.tiny),]

#dim(ged.tiny) ## 190830

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
##%%%%%%%%%%%%%%%%%%%

pkkmain <- article.analysis.alt(ged.pkk,k=5)

topics <- labelTopics(pkkmain$model) 

#topics

#summary(pkkmain$results)
#table(pkkmain$results$maxtopic) ##

datk5 <- merge(ged.pkk,
               pkkmain$results,
               by.x="id",
               by.y="id",
               all.x=TRUE)

#dim(datk5)
#colnames(datk5)
#table(datk5$maxtopic)
#summary(datk5$scaledvalue)

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

#pdf(file="PKKAlternateK5.pdf")
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
#dev.off()


#%%%%%%%%%%%%%%%%%%%%
### Visualize alternative topic specification for
## AQAP
##%%%%%%%%%%%%%%%%%%%

alt3 <- article.analysis.alt(ged.aqap,
                             k=3)

topics <- labelTopics(alt3$model, topics=c(1:3), n=10)   

head(alt3$results)
table(alt3$results$maxtopic)

dataqap3 <- merge(ged.aqap,
                  alt3$results,
                  by.x="id",
                  by.y="id",
                  all.x=TRUE)

class(alt3$results$maxtopic)

aqap3.yearsum <- dataqap3 %>%
  group_by(year) %>%
  summarize(
    countT1 = sum(maxtopic==1, na.rm = T),
    countT2 =  sum(maxtopic==2, na.rm = T),
    countT3 =  sum(maxtopic==3, na.rm = T))

#tail(aqap3.yearsum)


aqap3.yearsum$propT1 <- round(aqap3.yearsum$countT1/
                                (aqap3.yearsum$countT1+aqap3.yearsum$countT2 +
                                   aqap3.yearsum$countT3), 2)
aqap3.yearsum$propT2 <- round(aqap3.yearsum$countT2/
                                (aqap3.yearsum$countT1+aqap3.yearsum$countT2 +
                                   aqap3.yearsum$countT3), 2)
aqap3.yearsum$propT3 <- round(aqap3.yearsum$countT3/
                                (aqap3.yearsum$countT1+aqap3.yearsum$countT2 +
                                   aqap3.yearsum$countT3), 2)

aqap3.yearsum

##Find max:
propcols <- c("propT1","propT2", "propT3")
aqap3.yearsum$maxTopic <- colnames(aqap3.yearsum[,propcols])[max.col(aqap3.yearsum[,propcols],
                                                                     ties.method="first")]
aqap3.yearsum$maxTopic <- gsub(aqap3.yearsum$maxTopic,
                               pattern="propT",
                               replace="")

topics <- labelTopics(alt3$model, topics=c(1:3), n=10)
t1frex <-  paste0(topics$frex[1,], collapse=",") ## War on Terror topic
"us,polic,bomber,local,assault,capit,defenc,offens,assassin,blast"
t2frex <-  paste0(topics$frex[2,], collapse=",") ## AQAP as militant group topic
"qaeda,soldier,yemeni,clash,gunmen,al-qa'idah,troop,bomb,say,three"
t3frex <-  paste0(topics$frex[3,], collapse=",") ## Internationalized civil war topic
"aden,gulf,threat,critic,houthi,rebel,secur,begin,border,aqap"


aqap3.yearsum[which(aqap3.yearsum$maxTopic==1), "frexWords"] <- t1frex
aqap3.yearsum[which(aqap3.yearsum$maxTopic==2), "frexWords"] <-t2frex
aqap3.yearsum[which(aqap3.yearsum$maxTopic==3), "frexWords"] <- t3frex

table(aqap3.yearsum$maxTopic)

library(stargazer)

#sink(file="aqapthreetopics.txt")
stargazer(aqap3.yearsum[,1:8],
          summary=FALSE,
          rownames=FALSE)
#sink()



