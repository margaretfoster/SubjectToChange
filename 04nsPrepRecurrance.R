
rm(list=ls())

## Script to replicate and extend
## Nilsson and Svensson 2021
## with transformation measure

## Build the recurrence dataset with the new
## measure

library(tidyverse)
library(haven)
library(dplyr)

dataPath <- "./data/"

## Recurrence base data
recurrence <- read_dta(paste0(dataPath,
                               "Recurrence-data-ISQ.dta"))

## Termination base data to compare
termination <- read_dta(paste0(dataPath,
                               "Termination-data-ISQ.dta"))

##changes <- load(paste0(dataPathChange,
  ##                     "measurementScaleUpInProgressTiny.Rdata"))

load(paste0("./output/",
            "02dfyearsumAndRelatedTinyUpdate.Rdata"))

idkey <- read_csv(paste0(dataPath,"translate_actor.csv"))

## Recurrence only has information when the
## events change (for conflict-dyad-termination variables).
## so the 6.4k NA in some of the values is
## actually consistent with the
## underlying data architecture.

summary(recurrence)
## Add the new/old ID key:

##  Need to convert "" to NA in the sidebid column:
## For fill to work:
## Fill the empty ID rows:
## prep for fill by changing "" to NA"
recurrence[which(recurrence$sidebid==""),"sidebid"] <- NA
recurrence[which(recurrence$location==""),"location"] <- NA
recurrence[which(recurrence$sidea==""),"sidea"] <- NA
recurrence[which(recurrence$sideb==""),"sideb"] <- NA
recurrence[which(recurrence$conflictid==""),"conflictid"] <- NA
recurrence[which(recurrence$territoryname==""),"territoryname"] <- NA
recurrence[which(recurrence$region==""),"region"] <- NA


## fill empty rows
## b/c base data just "" for years with no changes

recurrence <- recurrence %>%
    arrange(dyadid,year) %>%
    fill(sidebid, location,
         sidea, sideb, conflictid,
         territoryname, region)

## Link old UCDP ID to new UCDP ID:
recurrence$sidebid <- as.numeric(recurrence$sidebid)
ids.recurrence <- unique(recurrence$sidebid)

recurrence <- merge(recurrence,
                     idkey,
                     by.x=("sidebid"),
                     by.y=("old_id"),
                     all.x=TRUE)

termination <- merge(termination,
                     idkey,
                     by.x=("sidebid"),
                     by.y=("old_id"),
                     all.x=TRUE)


dim(termination) ## unit is dyad-year
dim(recurrence) ## 6927 (unit is dyad-termination-year)

summary(recurrence) ## No NA in new_id

###########
## What are the groups in the overlap of
## the recurrence dataset and the change measure:

length(unique(recurrence$new_id))## 359
length(unique(df.yearsum$groupID)) ## 260 with the 10 event; 10 word threshold
length(unique(termination$new_id)) ## 377

length(intersect(unique(recurrence$new_id),
                 unique(df.yearsum$groupID))) ## 200 groups in the intersection

## need the 200 groups in reucurrence and not in df.yearsum:
missingIDs <- setdiff(recurrence$new_id,
                      df.yearsum$groupID)

length(missingIDs) ## 159 groups in the recurrence dataset and not in the framing dataset

## In recurrence but not df.yearsum:
missing <- recurrence[which(
    recurrence$new_id %in% missingIDs),
                      c("name", "year", "location")]


dim(missing) ## 4039
summary(missing$year) ## 1975-2013
plot(density(missing$year))

##%%%%%%%%%%%%%%%%%%%
## Merge change data into recurrence
##%%%%%%%%%%%%%%%%%%%
## 
recurrenceplus <- merge(x=recurrence,
                         y=df.yearsum,
                         by.x=c("new_id", "year"),
                         by.y=c("groupID", "year"),
                         all.x=TRUE)

summary(recurrenceplus) ## but still have very few matching
## Need to limit recurrenceplus to 1989 and later
## because that is what I have change data for.
## Though, despite this, I still have 6k rows of missing event data.

dim(recurrenceplus) ## 6927

## Identify the armed groups that had a change of
## 1, 1.5 and greater than 2:

haddelta1 <- unique(recurrenceplus[which(
    recurrenceplus$delta1==1),]$new_id)

haddelta15 <- unique(recurrenceplus[which(
    recurrenceplus$delta1.5==1),]$new_id)

haddelta2 <- unique(recurrenceplus[which(
    recurrenceplus$delta2==1),]$new_id)

length(haddelta1) ## 83 groups
length(haddelta15) ## 52 groups
length(haddelta2) ## 35 groups

## Add in the indicator variable for whether
## the militant group in a dyad had a change
## In any year:

recurrenceplus$haddelta1 <- NA
recurrenceplus$haddelta1 <- ifelse(recurrenceplus$new_id
                                    %in% haddelta1, 1, 0)

## Haddelta1.5 -> haddelta15 becuase STATA does not like
## dots in variable names
recurrenceplus$haddelta15 <- NA
recurrenceplus$haddelta15 <- ifelse(recurrenceplus$new_id
                                    %in% haddelta15, 1, 0)
recurrenceplus$haddelta2 <- NA
recurrenceplus$haddelta2 <- ifelse(recurrenceplus$new_id
                                    %in% haddelta2, 1, 0)

## Build NAs back into the data
## for the groups that I don't have any coverage for at all
## Otherwise the 0 for the "haddelta" variables are both substantive and structural zeros
dim(recurrenceplus[which(
    recurrenceplus$new_id %in% missingIDs),
                   c("haddelta1", "haddelta15",
                     "haddelta2")]) ## 4069

recurrenceplus[which(
    recurrenceplus$new_id %in% missingIDs),
               c("haddelta1", "haddelta15", "haddelta2")] <- NA ## 4069

summary(recurrenceplus$haddelta1)
table(recurrenceplus$haddelta1) ## 1668 no; 1190 yes
###

summary(recurrenceplus$year) ## 1975-2013

recurrenceplus <- recurrenceplus[which(
    recurrenceplus$year>=1989),]

dim(recurrenceplus) ## 6236 x 92

summary(recurrenceplus) ## ~5.5k missing in the yearly framing variables;
## 3.4k missing in the binary variables for any structural change

## what groups are in the NA sets
tst <- recurrenceplus[is.na(recurrenceplus$haddelta1),
               c("year", "dyadid","conflictid",
                 "location", "new_id", "name")] ## 3441

dim(tst) 

length(unique(tst$name)) ## 157 groups that are totally outside of my yearly framing data

tst2 <- as.data.frame(table(tst$new_id))
tst2 <- tst2[order(tst2$Freq),]

table(tst2$Freq) ##95 out of the 157 missing groups had 25 years of activity
## which implies 1970s-2000s?

##%%%%%%%%%%%%%%%%%%%%%%%
## Ambiguity Variable
##%%%%%%%%%%%%%%%%%%%%%%%

## what I want here is some measure of the
## ambiguity of presentation of a group
## current thought for the specification is to
## look at average yearly topic gap

gapmean <- recurrenceplus %>%
  group_by(new_id) %>%
    summarise_at(vars(propdiff),
                 list(gapmean = mean))

summary(gapmean) ## 

dim(recurrenceplus) ## 6236

recurrenceplus <- merge(x=recurrenceplus,
                         y=gapmean,
                         by="new_id",
                         all.x=TRUE)

summary(recurrenceplus) ## 6177 missing in mean of propdiff

## make a binary variable for "ambiguous"
## first cut: value of |.25| and |.50|?

## Has a gap in the range of [-.25, .25]
recurrenceplus$ambig25 <- ifelse(
    is.na(recurrenceplus$gapmean),
    NA,
    ifelse(abs(recurrenceplus$gapmean) <= .25,
           1, 0))
summary(recurrenceplus$ambig25)
table(recurrenceplus$ambig25) ##13 yes; 46 no. 

## has a gap of [-0.5, .5]
recurrenceplus$ambig50 <- ifelse(
    is.na(recurrenceplus$gapmean),
    NA, ifelse(abs(
        recurrenceplus$gapmean) <= .5,
               1, 0))

table(recurrenceplus$ambig50) ## 35 no; 24 yes

##%%%%%%%%%%%%%%%%%
## Summary statistics of change
## Match to the online appendix
##%%%%%%%%%%%%%%%%%%
## The unit for these is dyad-year:

table(recurrenceplus$haddelta1) ## 1643 no; 1152 yes dyad-years
table(recurrenceplus$haddelta15) ## 2062 no; 733 dyad-years
table(recurrenceplus$haddelta2) ## 2269 no; 526 dyad-years

### Recurrence X Islamist

round(prop.table(table(recurrenceplus$haddelta1)),2)## With the 10 event-10 word threshold, now 41% of dyad years occured in a group that had a framing change

round(prop.table(table(recurrenceplus$haddelta1,
                       recurrenceplus$islamist)), 2) ## Interestingly, the "change" groups are underrepresented among the groups coded as Islamist; which probably reflects the "stickyness" of framing 

## Make correlation plots:

save(recurrenceplus, recurrence,
     file="recurrencePlusforRep.Rdata")

## Format column names for STATA by removing periods
colnames(recurrenceplus)[which(
    colnames(recurrenceplus)=="propdif.L1")] <- "propdif_L1"
colnames(recurrenceplus)[which(
    colnames(recurrenceplus)=="propdif.L2")] <- "propdif_L2"
colnames(recurrenceplus)[which(
    colnames(recurrenceplus)=="altpropdiff.L1")] <- "altpropdiff_L1"
colnames(recurrenceplus)[which(colnames(recurrenceplus)=="delta1.5")] <-"delta15"
colnames(recurrenceplus)[which(colnames(recurrenceplus)=="delta1.5")] <-"delta15"

colnames(recurrenceplus)

write_dta(data=recurrenceplus,
          "recurrenceplus.dta")

print("Finished making recurrence data")