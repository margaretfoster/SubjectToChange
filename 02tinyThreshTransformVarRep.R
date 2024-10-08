rm(list=ls())

## load packages to clean and work with data
## experiment with modeling changes in behavior
## for groups across time
## using the source_original underlying article(s)


groundhog_day <- "2024-02-01" ##
groundhog.library(c("ggplot2", 'tidyverse', 'stargazer', 'dplyr'),
                  groundhog_day,
                  ignore.deps = c("MASS", "Matrix", "RcppArmadillo", 
                                  "fastmatch", "RcppParallel",
                                  "SnowballC", "stopwords", 
                                  'Rcpp', 'ISOcodes'),
                  tolerate.R.version='4.3.3')



outPath <- "./"

load(file=paste0(outPath,
         "01measurementScaleUpInProgressTiny55.Rdata"))

##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ls()

df.basedata <- do.call("rbind",tiny.basedata)
df.yearsum <- do.call("rbind",tiny.yearsum)


## first need to remove the rows
##  that have group years
## but for which there were no processed articles
## those are the years where the sum of the articles
## across both topics is 0
 ## variables are countT1 and countT

## Summarize number of articles modeled per year:
dim(df.yearsum) ##2180

df.yearsum$modeledarticles <- df.yearsum$countT1+
    df.yearsum$countT2

summary(df.yearsum$modeledarticles) ## 0-17735; median: 8; mean = 83

## Right tail:
#print(df.yearsum[which(df.yearsum$modeledarticles>1500),], n=23)
## With one exception, these are all very recent articles (after 2011)
## and seem to be Taliban (actor 303), Syrian insurgents (4456),
## IS (actor 234). The exception is the Serbian Republic of Bosnia-Herzogovina (actor 339) in 1992.

## Left tail: groups with no articles
#print(df.yearsum[which(df.yearsum$modeledarticles==0),], n=53) 

## Removing these from the data, because there isn't any post-modeling
## analysis that can be done in this set


## What do the 0 article years do to my lag variables:
## (A: introduce NAN values)

# print(df.yearsum[which(df.yearsum$groupID==223),],n=24)## for this group the no
## modeled article years are a cluster in the center of a set of years with
## only one article; ads one NAN year of propdif.L1 at the end of the streak

#print(df.yearsum[which(df.yearsum$groupID==169),],n=32)##

## Convert NAN and NA to 0.
## NAN are years with no articles. Don't want to drop them because that
## distorts the year structure of the data
## But equally need to skip over those zeros when I take the lag
## Note that there are 67 of these years; so pretty rare

df.yearsum[which(is.nan(df.yearsum$propT1)==TRUE|
                 is.na(df.yearsum$propT1)==TRUE), "propT1"] <- 0## No NAN or NA in prop difference


df.yearsum[which(is.nan(df.yearsum$propT2)==TRUE|
                 is.na(df.yearsum$propT2)==TRUE), "propT2"] <- 0## No NAN or NA in prop difference


df.yearsum[which(is.nan(df.yearsum$propdiff)==TRUE|
                 is.na(df.yearsum$propdiff)==TRUE), "propdiff"] <- 0## No NAN or NA in prop difference

df.yearsum[which(is.nan(df.yearsum$frexWords)==TRUE|
                 is.na(df.yearsum$frexWords)==TRUE), "frexWords"] <- "None"## No NAN or NA in prop difference


#df.yearsum[which(df.yearsum$modeledarticles==0),]

summary(df.yearsum$modeledarticles) ##0-17,735 ## now no NA; previous had 10 concentrated in groups 363, 805
 
summary(df.yearsum$propdiff) ##-1 to 1, median -0.34

##Reindex the lag to skip the rows with no modeled articles

zeros <- df.yearsum[which(df.yearsum$modeledarticles==0),]

zeros$propdif.L1 <- 0 ## placeholder for no change from previous year.

df.yearsum2 <- df.yearsum %>%
    group_by(groupID) %>%
    arrange(year, .by_group=TRUE) %>%
    filter(modeledarticles > 0) %>%
   dplyr::mutate(propdif.L1 = propdiff - dplyr::lag(propdiff, n=1L)) %>%
    dplyr::mutate(propdif.L2 = propdiff - dplyr::lag(propdiff, n=2L))

## Reattach the 0 article years:

df.yearsum <- rbind(df.yearsum2,
                    zeros)


## Convert NAN and NA to zero
## NAN are caused by years with no articles
## NA ae usually first years in the data

df.yearsum[is.finite(df.yearsum$propdif.L1)==FALSE,
           "propdif.L1"] <- 0

###%%%%%%%%%%%%%%%%%%%%%%%
## Create Delta measure
## Make an indicator for group-years with:
## propdiff L1:
## (a) greater than = |1|
## (b) Between |1| and |1.5|
## (c) between |1.5| and |2|
## (d) equal to |2|
## Generate summary statistics

df.yearsum$delta1 <- 0 ## group-years with change over 1:

df.yearsum$delta1<- ifelse(abs(df.yearsum$propdif.L1)>= 1, 1, 0)

table(df.yearsum$delta1) ## 343 years of change

round(prop.table(table(df.yearsum$delta1)), 2) ## 84% no; 16% yes

hadgap1 <- unique(df.yearsum[df.yearsum$delta1==1,]$groupID) ## 157

length(hadgap1)/length(unique(df.yearsum$groupID)) #0.6

## group-years with a lag-to-now change 1.5 or larger:
df.yearsum$delta1.5 <- 0
df.yearsum$delta1.5<- ifelse(abs(df.yearsum$propdif.L1) >= 1.5,
                           1, 0)

table(df.yearsum$delta1.5) ## 181 group-years; 
length(unique(df.yearsum[which(df.yearsum$delta1.5==1),]$groupID)) ## 102 unique groups

## list of groups with a 1.5 gap:
hadgap1.5 <- unique(df.yearsum[df.yearsum$delta1.5==1,]$groupID)

## group-years with a one year lag change
## of 2
df.yearsum$delta2 <- 0
df.yearsum$delta2<- ifelse(abs(df.yearsum$propdif.L1) == 2,
                           1, 0)

table(df.yearsum$delta2) ##118 years in 68 different groups

length(unique(df.yearsum[which(df.yearsum$delta2==1),]$groupID))

hadgap2 <- unique(df.yearsum[df.yearsum$delta2==1,]$groupID)

##%%%%%%%%%%%%%%%%%%%%%%%%%
## Two-year Lag Changes:
##%%%%%%%%%%%%%%%%%%%%%%%%
## make a window for groups that might not have made the change threshold
## across one year but did across two:

df.yearsum$delta1_L2 <- 0 ## group-years with a change of at least |1| over
## a two-year change:
df.yearsum[which(abs(df.yearsum$propdif.L2)>= 1 |
                 abs(df.yearsum$propdif.L1) >=1), "delta1_L2"] <- 1

table(df.yearsum$delta1_L2) ## 554 years with the two-year window of change
round(prop.table(table(df.yearsum$delta1_L2)),2) ## 0.25 of the total groups 

hadgap1.l2 <- unique(df.yearsum[df.yearsum$delta1_L2==1,]$groupID)

length(hadgap1.l2) ## 172 groups 
## setdiff(hadgap1.l2, hadgap1) ## 260 groups with a gap of 1 but not 2

## group-years with a lag-to-now change 1.5 or larger:
df.yearsum$delta15_L2 <- 0

df.yearsum[which(abs(df.yearsum$propdif.L2)>= 1.5 |
                 abs(df.yearsum$propdif.L1) >=1.5),
           "delta15_L2"] <- 1

table(df.yearsum$delta15_L2) ## 317 group-years; 

length(unique(df.yearsum[which(df.yearsum$delta15_L2==1),]$groupID)) ## 120 unique groups

## list of groups with a 1.5 gap:
hadgap15L2 <- unique(df.yearsum[df.yearsum$delta15_L2==1,]$groupID)

## group-years with a one year lag change
## of 2
df.yearsum$delta2_L2 <- 0

df.yearsum[which(abs(df.yearsum$propdif.L2)>= 2 |
                 abs(df.yearsum$propdif.L1) >=2), "delta2_L2"] <- 1


table(df.yearsum$delta2_L2) ##196 years in 68 different groups

#length(unique(df.yearsum[which(df.yearsum$delta2_L2==1),]$groupID)) ##80

#hadgap2L2 <- unique(df.yearsum[df.yearsum$delta2_L2==1,]$groupID)


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
df.yearsum$gap25 <- 0
df.yearsum[which(abs(df.yearsum$propdiff) <= .25 &
           df.yearsum$modeledarticles >0), "gap25"] <- 1

table(df.yearsum$gap25) ## 10 event threshold: 1882  no, 231  yes

length(unique(df.yearsum[which(df.yearsum$gap25==1),]$groupID))
## 140 group-years between the topics is less than .25

## Gap less than .5 & pos. number of articles modeled
df.yearsum$gap50 <- 0
df.yearsum[which(abs(df.yearsum$propdiff) <= .5 &
           df.yearsum$modeledarticles >0), "gap50"] <- 1


## gap between the topics is less than .5
#table(df.yearsum$gap50)## 10 threshold: 1664  no, 448 yes 

#length(unique(df.yearsum[which(df.yearsum$gap50==1),]$groupID)) ## in 176 groups

##%%%%%%%%%%%%%%%%%%%%%%%%%
## Per-group count
## years between framing change periods

colnames(df.yearsum)

cols <-  c("year",
           "countT1", "countT2",
           "propdiff","propdif.L1",
           "delta1", "delta1_L2","gap25")

## highlight the need for a two-year lag
## because the shifts in framing are complete over two years
## but just barely under the |1| framing
## 223: NSCN-IM (Naga Insurgent Group)
#print(df.yearsum[which(df.yearsum$groupID==223),
#                 cols ],n=24)

## Nested loops to iterate through the list of groups
## and create a counter:
## Counter has: 0 until a change
## 0 in the year that delta = 1
## plus 1 for every year until the next change 
## [Not efficient, but more interpretable]

#df.yearsum$yearsSinceChange <- NA

df.yearsum$counter <- NA
for(g in unique(df.yearsum$groupID)){
    print(g)
    for(r in 1:dim(df.yearsum[which(df.yearsum$groupID==g),])[1]){
        print(r)
        if(r==1){
            df.yearsum[which(df.yearsum$groupID==g),][r, "counter"] <- 0
        } 
        else{
            if(df.yearsum[which(df.yearsum$groupID==g),][r-1, "delta1"]==1){
                df.yearsum[which(df.yearsum$groupID==g),][r,"counter"] <- 1
            }else{
                if(df.yearsum[which(df.yearsum$groupID==g),][r-1, "counter"] > 0){
                    df.yearsum[which(df.yearsum$groupID==g),][r, "counter"] <- df.yearsum[which(df.yearsum$groupID==g),][r-1, "counter"] + 1
                }else{
                    if(df.yearsum[which(df.yearsum$groupID==g),][r-1, "counter"]==0){
                        df.yearsum[which(df.yearsum$groupID==g),][r, "counter"] <- 0
                    }
                }
            }
        }
    }
}


## now zero out "counter" for years of change:

df.yearsum[which(df.yearsum$delta1==1), "counter"] <- 0

#table(df.yearsum$delta1)
#table(df.yearsum$counter)

save(df.yearsum, df.basedata, hadgap2, hadgap1.5,
     file=paste0(outPath,"02dfyearsumAndRelatedTinyUpdate.Rdata"))

print("Script 2 finished, data saved")