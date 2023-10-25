 rm(list=ls())
## Takes the text modeling, derives
## A version of the analysis data
## that retains estimated change year


## load packages to clean and work with data
library(ggplot2)
library(ggridges)
library(tidyverse)
library(readxl)
library(stargazer)
library(stringi)

outPath <- "./output/"

load(file=paste0(outPath,
         "02dfyearsumAndRelatedTinyUpdate.Rdata"))

## Loads:
## df.basedata: 190,688 x 54
## PRIO GED with the per-event topic summary:
## maxtopic: numeric identifier of assigned topic
## maxvalue:  Probability assignment to the topic [0,1], this is Theta
## scaledvalue: Theta scaled to [-1 (T1), 1 (T2)]
## factor:  identified topic as a Factor variable
## frexWords: Frex for the identified topic

## df.yearsum:
## 2113 x 16
## Group-year dataframe with number of articles,
## Count of number of articles in T1 or T2;
## estimated yearly topic proportions
## topic proportion difference relative to previous year
## whether there was a changeover year
## magnitude of the change (|1|, |1.5|, |2|)
## measure of ambiguity (gap25, gap50)

## All data on the minimum of 10 PRIO GED events
## and 10 words to model

########################
## Make an overview of groups that change
## and "oscillating" groups 
## cross-reference with UCDP encylopedia


dim(df.yearsum)

head(df.yearsum)
colnames(df.yearsum)

## Count propdiff L1 and tally the number that are:
## (a) greater than = |1|
## (b) Between |1| and |1.5|
## (c) between |1.5| and |2|
## (d) equal to |2|

#######

colnames(df.yearsum)

df.basedata$side_b[grep(
    as.character(df.basedata$side_b),
    pattern="Consolidation")] <- "Military Junta CDPJ"

## Three active-year threashold
ggdata <- df.yearsum.plot[which(df.yearsum.plot$groupID
                                %in% freqs.g3$Var1),]

gch <- ggplot(ggdata,
             aes(x=propdif.L1,
                    y=side_b,
                 group = side_b)) +
    geom_density_ridges2()+
    scale_y_discrete(limits=rev)+
    labs(x= "Distribution of Yearly Proportion Changes",
           y="Actor")+
    ggtitle(label="Overview of Yearly Proportion Changes",
            subtitle="Grouped By Quartiles of Activity Level")+
    theme_bw()+
   facet_wrap(~quartile, scales="free")+
    theme(axis.text.y=element_text(size=3))

gch

ggsave(gch,
       file="propdiffLRidgePlot.pdf")

library(dplyr)

## 47 groups with years with a changeover in [1, 1.5) 
first.zone <- as.data.frame(table(df.yearsums[which(
    abs(df.yearsums$propdif.L1) >= 1 &
    abs(df.yearsums$propdif.L1) < 1.5),]$groupID))

colnames(first.zone) <- c("groupID", "fz")
head(first.zone)

## 43 groups with yearly changes in (1.5, 2]
tiny.zone <- as.data.frame(table(df.yearsums[which(
    abs(df.yearsums$propdif.L1) > 1.5 &
    abs(df.yearsums$propdif.L1) <= 2),]$groupID))

colnames(tiny.zone) <- c("groupID", "hz")
colnames(tiny.zone)

years.active <- as.data.frame(table(df.yearsums$groupID))
colnames(years.active) <- c("groupID", "numyears")
head(years.active)

## make into a subset:

large.changes <- merge(x=first.zone,
                       y=tiny.zone,
                       all=TRUE)

dim(large.changes) ##154 x 3


large.changes <- merge(x=large.changes,
                       y=years.active,
                       by="groupID",
                       all.x=TRUE)

dim(large.changes) ## 154 x 4

## order by the biggest "tiny-zone" swings
##  Expectations:
## should be able to go through this list and have the
## groups be intelligible as "weird" or changing cases

## merge in group names:
large.changes <- merge(x=large.changes,
                       y=unique(df.basedata[,
                           c("side_b_dset_id", "side_b")]),
                       by.x="groupID",
                       by.y="side_b_dset_id",
                       all.x=TRUE)

large.changes[is.na(large.changes)] <- 0
dim(large.changes) ## 71x 6

large.changes <- large.changes[order(large.changes$hz,
                                     large.changes$fz),]


## Print table as an appendix:
## also print as an html file so that I can
## build a spreadsheet table to cross-reference
## the periods of changes with UCDP writeups

## This is a subset that do not have years in change
## 1.5-2
stargazer(large.changes[which(large.changes$hz==0),
                        c(5, 1, 2, 3,4)],
          summary=FALSE,
          type="latex",
          covariate.labels=c("Group Name",
              "UCDP Group ID",
              "Years in Change (1,1.5]",
              "Years in Change (1.5, 2]",
              "Years in Data"),
          out="changeOverview1.tex"
          )

## subset with 1 or more years in change
## 1.5 or 2:

stargazer(large.changes[which(large.changes$hz>0),
                        c(5, 1, 2, 3,4)],
          summary=FALSE,
          type="latex",
          covariate.labels=c("Group Name",
              "UCDP Group ID",
              "Years in Change (1,1.5]",
              "Years in Change (1.5, 2]",
              "Years in Data"),
          out="changeOverview2.tex"
          )


## This is an html of the full list
## to make a spreadsheet to reference with the
## UDCP encylopedia
stargazer(large.changes[,c(5, 1, 2, 3,4)],
          summary=FALSE,
          type="html",
          covariate.labels=c(
              "Row number",
              "Group Name",
              "UCDP Group ID",
              "Years in Change (1,1.5]",
              "Years in Change (1.5, 2]",
              "Years in Data"),
          out="changeOverviewAll.html"
          )

colnames(df.basedata)
colnames(df.yearsums)
