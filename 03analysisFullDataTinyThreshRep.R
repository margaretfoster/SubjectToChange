rm(list=ls())

## Takes the text modeling, presents descriptive
## analsyis of the results


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

##load(file="df.yearsumAndRelatedTinyUpdate.Rdata")

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
## Subset with a minium threshold of 10 events (and 10 words to model)
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
####################
## Plotting results 
####################
ls()

class(df.basedata)
dim(df.basedata) ## 190688

colnames(df.basedata)
length(unique(df.basedata$conflict_new_id))
length(unique(df.basedata$side_b))

### Plot summaries of number of violent events
## Associated with each group

## Table of the data:

freqs <- as.data.frame(table(df.basedata$side_b_new_id))
## Add names:
freqs <- merge(x=freqs,
               y=unique(df.basedata[,
                   c("side_b_new_id", "side_b")]),
               by.y="side_b_new_id",
               by.x="Var1",
               all.x=TRUE)

freqs <- freqs[order(freqs$Freq),]

dim(freqs) 

freqs[1:100,] ## know that lowering the threshold to 10 from 50/25 adds 117 extra groups

## Plot distribution of groups:
## Syrian insurgency with the 60k events
## massively distorts the distribution of # of events

## plot(freqs[which(freqs$Freq < 10000),]$Freq)
## plot(density(freqs$Freq))
## plot(density(freqs[which(freqs$Freq < 10000),]$Freq))

summary(freqs$Freq)

## Categorize groups according to
## number of operations; for use in
## eventual ridgeline plot of article word counts

## About 65 groups in each bin:
## First quartile: Freq <=25
## second quartile   25 <Freq =< 75
## third quartile 75 < Freq  =< 219
## fourth quartile 219<

freqs$quartile <- NA
freqs[which(freqs$Freq <= 25), "quartile"] <- "First"
freqs[which(freqs$Freq > 25 &
            freqs$Freq <= 75), "quartile"] <- "Second"
freqs[which(freqs$Freq > 75 &
            freqs$Freq <= 219), "quartile"] <- "Third"
freqs[which(freqs$Freq > 219), "quartile"] <- "Fourth"

table(freqs$quartile) ## ~65 in each

## Min: 10, max 60814;
## Median: 75
## Merge the quartile variable back into the basedata:
## and yearly data

df.basedata <- merge(x=df.basedata,
                     y=freqs,
                     by.x= c("side_b_new_id", "side_b"),
                     by.y=c("Var1", "side_b"))

df.yearsum2 <- merge(x=df.yearsum,
                     y=freqs,
                     by.x= c("groupID"),
                     by.y=c("Var1"))

## Change name for interpetability
colnames(df.basedata)[which(
    colnames(df.basedata)=="Freq")] <- "GroupNumEvents"

table(df.basedata$quartile) ## extremely different number of violent events in each segment; Makes sense because the bins are # of groups; but df.basedata aggreggates events. And since the groups were arrayed in order of number of events, of course the ones in the "top" quartile have more events.

## Graph the length of the news articles.
## Want to check for systematic differences across
## the set of groups. In this case, ruling out difference
##  by activity level:
  
df.basedata$nwords <- stringi::stri_count_words(
    df.basedata$source_article)

summary(df.basedata$nwords) ## 1-998

## Explore the outliers:
## what are the 1-2 word articles:

one.word <- df.basedata[which(df.basedata$nwords <= 2),]
dim(one.word) ## 4919

one.word[1:400, c("side_b", "year", "source_article")]
unique(one.word$source_article)

## Explore the very long articles:

dim(df.basedata[which(df.basedata$nwords > 250),])
## 842 entries have over 100 words;
## 46 have over 250 words;
## 8 have over 500 words

long <- df.basedata[which(df.basedata$nwords > 250), "side_b"]

table(long) ## Top: Al-Shabaab; Syrians;
## IS; Caucasus Emirate; PSLF; ARSA

long2 <- df.basedata[which(
    df.basedata$nwords > 500), "side_b"]

table(long2)## Syrians; IS; Caucasus; ARSA; TAK
## Notably dropping the threshold brings a lot of outlier Burmese insurgent groups

##%%%%%%%%%%%%% Produce Ridgeline Plots For Word Distributions
## ridgeline plots for number of words:

library(ggplot2)
library(ggridges)

##
df.basedata$quartile <- as.factor(df.basedata$quartile)
df.basedata$quartile <- factor(df.basedata$quartile,
                              levels = c("First", "Second",
                                  "Third", "Fourth"))

## full data is too long:
## So this is all articles less than 250 words
## And there are 46 that are more than 250 words
## versus ~842 that are over 100
## (though, the 842 over 100 is still 0.4% of the total)

head(df.basedata$side_b)

df.basedata$side_b[grep(as.character(df.basedata$side_b),
                        pattern="Consolidation")] <- "Military Junta CDPJ"

##breaks<- c(0,5,10, 15, 20,30, 40, 50,100,200,500,1000)

gr <- ggplot(df.basedata,
             aes(x=nwords,
                    y=side_b,
                 group = side_b)) +
    geom_density_ridges2()+
    scale_x_log10() +
    scale_y_discrete(limits=rev)+
    labs(x= "Distribution of Words in Source Articles",
           y="UCDP Actor")+
    ggtitle(label="Distribution of Article Lengths",
            subtitle="Grouped By Quartiles of Activity Level")+
    theme_bw()+
    facet_wrap(~quartile, scales="free")+
    theme(axis.text.y=element_text(size=3))

gr

ggsave(gr,
       file="distArticleLengthsTiny.pdf")

########################
## Make an overview of groups that change
## and "oscellating" groups, so that you can
## cross-reference these groups with UCDP encylopedia

dim(df.yearsum)
head(df.yearsum)
colnames(df.yearsum)

## want to count propdiff L1 and tally the number that are:
## (a) greater than = |1|
## (b) Between |1| and |1.5|
## (c) between |1.5| and |2|
## (d) equal to |2|

df.yearsum.plot <- df.yearsum2 %>%
    filter(!is.na(propdif.L1))

dim(df.yearsum.plot) ## 2180 x 24
length(unique(df.yearsum.plot$groupID)) # 260

df.yearsum.plot$quartile <- factor(df.yearsum.plot$quartile,
                              levels = c("First", "Second",
                                  "Third", "Fourth"))

##12/3: Mystery in the propdiff ridgeplots: why are many rows missing ridgelines.
##Answer, the ridgelines are yearly topic distributions; groups with fewer than some number (2? 5?)
##Answer: fewer than 5 modeled articles.
## check out a couple of the missing rows:
## Are the missing rows due to not having enough events to model?

## subset to groups with more than 5 articles modeled:
active.year.freqs <-  as.data.frame(table(
    df.yearsum.plot$groupID))

active.year.freqs <- active.year.freqs[order(
    active.year.freqs$Freq),]

freqs.g3 <- active.year.freqs[which(
    active.year.freqs$Freq >= 3),]

freqs.g5 <- active.year.freqs[which(
    active.year.freqs$Freq >= 5),]

dim(freqs)
dim(freqs.g3)
dim(freqs.g5)

head(freqs)
df.yearsum.plot[which(df.yearsum.plot$side_b=="ASL"),]
summary(df.yearsum.plot) ## 1818 x 16

#######3
df.yearsum.plot$side_b[grep(
    as.character(df.yearsum.plot$side_b),
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


## 100 groups with years with a changeover in [1, 1.5) 
first.zone <- as.data.frame(table(df.yearsum[which(
    abs(df.yearsum$propdif.L1) >= 1 &
    abs(df.yearsum$propdif.L1) < 1.5),]$groupID))

colnames(first.zone) <- c("groupID", "fz")
head(first.zone) ## 100 x 2

## 43 groups with yearly changes in (1.5, 2]
tiny.zone <- as.data.frame(table(df.yearsum[which(
    abs(df.yearsum$propdif.L1) > 1.5 &
    abs(df.yearsum$propdif.L1) <= 2),]$groupID))

colnames(tiny.zone) <- c("groupID", "hz")
head(tiny.zone) ## 100x 2

years.active <- as.data.frame(table(df.yearsum$groupID))
colnames(years.active) <- c("groupID", "numyears")
head(years.active)

dim(years.active) ## 260


## make into a subset:

large.changes <- merge(x=first.zone,
                       y=tiny.zone,
                       all=TRUE)

dim(large.changes) ##156 x 3


large.changes <- merge(x=large.changes,
                       y=years.active,
                       by="groupID",
                       all.x=TRUE)

dim(large.changes) ## 156 x 4

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
dim(large.changes) ## 156x 6

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

## Summarize the counter:
colnames(df.basedata)
colnames(df.yearsum)

summary(df.yearsum$counter)

df.yearsum[which(df.yearsum$counter > 20),
           c("year", "groupID")]

tmp <- df.yearsum[which(df.yearsum$counter > 10),
                  c("year", "groupID")]
unique(tmp$groupID)

print(df.yearsum[which(df.yearsum$groupID==338),
                 c("year", "propT1", "propT2",
                   "frexWords", "counter")], n=28)

## Find the distribution of max values:
require(dplyr)
countermax <- df.yearsum%>%
    group_by(groupID) %>%
    summarise(max = max(counter))

dim(countermax) ## 260 x2

head(countermax)

plot(hist(countermax$max))
summary(countermax$max)
