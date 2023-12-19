## Scratch file to save descriptive

## From 03analysisFullDataTinyThreshRep.R
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


##from source("./04nsPrepTerminationRep.R")

## What groups and dyads are NA in the post 1989 subset:
terminationplus1 <- terminationplus[is.na(
  terminationplus$propdiff),] ## 295
terminationplus2 <- terminationplus[is.na(
  terminationplus$delta1.5),] ## 404

unique(terminationplus1$sideb) 

## Outlier identification:
numchanges[which(numchanges$numchanges==4),]##2
numchanges[which(numchanges$numchanges==3),] ## 8
numchanges[which(numchanges$numchanges==2),] ##19


## 4 changes:
cols <- c("sideb", "frexWords", 'year')

terminationplus[which(terminationplus$new_id==277),cols]## Abu Sayyaf
terminationplus[which(terminationplus$new_id==743), cols]##  FARC

