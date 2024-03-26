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

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
## Subset with a minium threshold of 10 events (and 10 words to model)
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
####################
## Plotting results 
####################

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

## Plot distribution of groups:
## Content notes:
## Syrian insurgency (with 60k events)
## massively distorts the distribution of # of events

## plot(freqs[which(freqs$Freq < 10000),]$Freq)
## plot(density(freqs$Freq))
## plot(density(freqs[which(freqs$Freq < 10000),]$Freq))

##summary(freqs$Freq)

## Categorize groups according to
## number of operations; for use in
##ridgeline plot of article word counts

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

## Change name for readability
colnames(df.basedata)[which(
    colnames(df.basedata)=="Freq")] <- "GroupNumEvents"

table(df.basedata$quartile)
## Notes: extremely different number of violent events in each segment; 
## Makes sense because the bins are # of groups; but df.basedata aggreggates events. 

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
## and "oscillating" groups, so that you can
## cross-reference these groups with UCDP encyclopedia

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

## subset to groups with more than 5 articles modeled:
active.year.freqs <-  as.data.frame(table(
    df.yearsum.plot$groupID))

active.year.freqs <- active.year.freqs[order(
    active.year.freqs$Freq),]

freqs.g3 <- active.year.freqs[which(
    active.year.freqs$Freq >= 3),]

freqs.g5 <- active.year.freqs[which(
    active.year.freqs$Freq >= 5),]

#head(freqs)
#df.yearsum.plot[which(df.yearsum.plot$side_b=="ASL"),]
#summary(df.yearsum.plot) ## 1818 x 16

######
df.yearsum.plot$side_b[grep(
    as.character(df.yearsum.plot$side_b),
    pattern="Consolidation")] <- "Military Junta CDPJ"

## Three active-year threshold
ggdata <- df.yearsum.plot[which(df.yearsum.plot$groupID
                                %in% freqs.g3$Var1),]

## Text Figure One:
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



