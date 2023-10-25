### Script to call the entire development cycle
## to experiment with effect of changing minimum activity thresholds

rm(list=ls())

## Robustness from R2:
## Version of change measure that imposes a minimum threshold 
## number of documents per year

## Plan of attack:
## Create dataset of all groups which clear an 
## {N} threshold of articles/year
## run model on this set

## use that to run the replication; see if the results are similar
##Libraries and helper files:

library(ggplot2)
library(tidyverse)
library(readxl)


source("./00articleModelingRep.R") 
source("./00dominantFramingRep.R") ## wrapper to summarize yearly STM

dataPath <- "./data/"
outPath <- "./output/"

ucdp.ged<- read.csv(paste0(dataPath,"ged211.csv"))

ucdp.acd <- read.csv(paste0(dataPath, "ucdp-prio-acd-211.csv"))

ucdp.actor <- read_excel(paste0(dataPath, "ucdp-actor-211.xlsx"))

nonstate.actors <- ucdp.actor[which(ucdp.actor$Org==1),]
nonstate.actorList <- unique(nonstate.actors$ActorId)

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

## Frequency of events associated with actors:
## by year:

colnames(ucdp.subset)

actor.freq <- ucdp.subset %>% 
  dplyr::group_by(side_b, year, side_b_new_id) %>%
  summarize(year.events=n())

## FIRST adjust point:
## Keep only those groups with  >= N events for every year active
## Declare N:
N = c(1, 5, 10)

## Declare a threshold (Th) for number of years needed to be above N events

Thresh = c(1, .9, .75)

## Loop through N, Th:

for (n in N){  
      for (t in Thresh){

## declare threshold (Th):
## Th is % of group years above N.
## Default 1 (so all years above N articles)
## but can change
  N= n
  Th=t
  
  print(paste0("N =", N))
  print(paste0("Th =", Th))

  actor.freq$geqN <- ifelse(actor.freq$year.events >= N, 
                          "yes", "no")

  propSummary <- actor.freq %>% 
  group_by(side_b, side_b_new_id) %>%
  count(geqN) %>% mutate(prop = prop.table(n))
  activeN <- propSummary[which(propSummary$prop >= Th),]

## IDs for group
## OBSERVE: This pretty much gets rid of joint events
## but those are written about together-- one article, several groups
## so it's unclear how tightly the article would be linked to the 
## activities of each constituent org.

  subsetN <- unique(activeN$side_b_new_id)
  gedN <- ucdp.subset[which(
    ucdp.subset$side_b_new_id %in% subsetN),]

## extract data:
uidsN <- unique(gedN$side_b_new_id)

## Generate data for the N years with more than N events
## criteria:

print(paste0("number of groups is: ", length(uidsN)))

yearsumN <- list()
basedataN <- list()
i=1
  for(u in uidsN){
  print(paste0("iteration: ", i))
  analysis<- dominantFraming(groupID= u,
                             ucdpdata=gedN)
  
  yearsumN[[i]] <- analysis$yearsum
  basedataN[[i]] <- analysis$basedata
  i=i+1 
}

## Uncomment if want to save along the way:
##save(yearsumN, basedataN,
##     file=paste0(outPath,
##                 "Robustness_", N, "_", "Th", "_Data.Rdata"))

  print("Part 1 finished, data saved")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Data + Summary Stats
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## From Script 2:

  df.basedataN <- do.call("rbind",basedataN)
  df.yearsumN <- do.call("rbind",yearsumN) ## topic data

  df.yearsumN$modeledarticles <- df.yearsumN$countT1+
    df.yearsumN$countT2


  freqMA <- as.data.frame(table(df.yearsumN$modeledarticles))
  colnames(freqMA) <- c("numarticles", "Freq")
  freqMA <- freqMA[order(freqMA$numarticles),]

## Convert NAN and NA to 0.
## NAN are years with no articles. Don't want to drop them because that
## distorts the year structure of the data
## But equally need to skip over those zeros when I take the lag
## Note that there are 67 of these years; so pretty rare

df.yearsumN[which(is.nan(df.yearsumN$propT1)==TRUE|
                    is.na(df.yearsumN$propT1)==TRUE), "propT1"] <- 0
## No NAN or NA in prop difference


df.yearsumN[which(is.nan(df.yearsumN$propT2)==TRUE|
                    is.na(df.yearsumN$propT2)==TRUE), "propT2"] <- 0## No NAN or NA in prop difference


df.yearsumN[which(is.nan(df.yearsumN$propdiff)==TRUE|
                    is.na(df.yearsumN$propdiff)==TRUE), "propdiff"] <- 0
## No NAN or NA in prop difference

df.yearsumN[which(is.nan(df.yearsumN$frexWords)==TRUE|
                    is.na(df.yearsumN$frexWords)==TRUE), "frexWords"] <- "None"
## No NAN or NA in prop difference


##Reindex the lag to skip the rows with no modeled articles
zeros <- df.yearsumN[which(df.yearsumN$modeledarticles==0),]
zeros$propdif.L1 <- 0
## placeholder for no change from previous year.

df.yearsum2 <- df.yearsumN %>%
  group_by(groupID) %>%
  arrange(year, .by_group=TRUE) %>%
  filter(modeledarticles > 0) %>%
  mutate(propdif.L1 = propdiff - lag(propdiff, n=1L)) %>%
  mutate(propdif.L2 = propdiff - lag(propdiff, n=2L))

## Reattach the 0 article years:

df.yearsumN <- rbind(df.yearsum2,
                     zeros)


dim(df.yearsumN) ## 739 

## Convert NAN and NA to zero
## NAN are caused by years with no articles
## NA ae usually first years in the data

df.yearsumN[is.finite(df.yearsumN$propdif.L1)==FALSE,
            "propdif.L1"] <- 0

## only 6 years without articles
print(df.yearsumN[which(df.yearsumN$modeledarticles==0),]) 

###%%%%%%%%%%%%%%%%%%%%%%%
## Create Delta measure
## Make an indicator for group-years with:
## propdiff L1:
## (a) greater than = |1|
## (b) Between |1| and |1.5|
## (c) between |1.5| and |2|
## (d) equal to |2|


df.yearsumN$delta1 <- 0 ## group-years with change over 1:
df.yearsumN$delta1<- ifelse(abs(df.yearsumN$propdif.L1)>= 1, 1, 0)

table(df.yearsumN$delta1) ## 343 years of change in entire data; 94 in this version

round(prop.table(table(df.yearsumN$delta1)), 2) ## 84% no; 16% yes in base data
## 87% no, 13% yes in this data

hadgap1 <- unique(df.yearsumN[df.yearsumN$delta1==1,]$groupID)

length(hadgap1)/length(unique(df.yearsumN$groupID)) ## 37% had change in this data

## group-years with a lag-to-now change 1.5 or larger:
df.yearsumN$delta1.5 <- 0
df.yearsumN$delta1.5<- ifelse(abs(df.yearsumN$propdif.L1) >= 1.5,
                              1, 0)

table(df.yearsumN$delta1.5) ## 181 group-years in base data;
length(unique(df.yearsumN[which(df.yearsumN$delta1.5==1),]$groupID)) 
## 102 unique groups in base data; 36 in this data

## list of groups with a 1.5 gap:
hadgap1.5 <- unique(df.yearsumN[df.yearsumN$delta1.5==1,]$groupID)

## group-years with a one year lag change
## of 2
df.yearsumN$delta2 <- 0
df.yearsumN$delta2<- ifelse(abs(df.yearsumN$propdif.L1) == 2,
                            1, 0)

table(df.yearsumN$delta2) ##118 years in 68 different groups
## (versus 38 years in 33 different groups)

length(unique(df.yearsumN[which(df.yearsumN$delta2==1),]$groupID))

hadgap2 <- unique(df.yearsumN[df.yearsumN$delta2==1,]$groupID)

##%%%%%%%%%%%%%%%%%%%%%%%%%
## Two-year Lag Changes:
##%%%%%%%%%%%%%%%%%%%%%%%%
## make a window for groups that might not have made the change threshold
## across one year but did across two:

df.yearsumN$delta1_L2 <- 0 ## group-years with a change of at least |1| over
## a two-year change:
df.yearsumN[which(abs(df.yearsumN$propdif.L2)>= 1 |
                    abs(df.yearsumN$propdif.L1) >=1), "delta1_L2"] <- 1

table(df.yearsumN$delta1_L2) ## 554 years with the two-year window of change
round(prop.table(table(df.yearsumN$delta1_L2)),2)

hadgap1.l2 <- unique(df.yearsumN[df.yearsumN$delta1_L2==1,]$groupID) ## 172 groups

length(hadgap1.l2)
length(unique(df.yearsumN$groupID))

setdiff(hadgap1.l2, hadgap1) 

## group-years with a lag-to-now change 1.5 or larger:

df.yearsumN$delta15_L2 <- 0

df.yearsumN[which(abs(df.yearsumN$propdif.L2)>= 1.5 |
                    abs(df.yearsumN$propdif.L1) >=1.5),
            "delta15_L2"] <- 1

table(df.yearsumN$delta15_L2) ## 317 group-years; 

length(unique(df.yearsumN[which(df.yearsumN$delta15_L2==1),]$groupID)) ## 120 unque groups

## list of groups with a 1.5 gap:
hadgap15L2 <- unique(df.yearsumN[df.yearsumN$delta15_L2==1,]$groupID)

## group-years with a one year lag change
## of 2
df.yearsumN$delta2_L2 <- 0

df.yearsumN[which(abs(df.yearsumN$propdif.L2)>= 2 |
                    abs(df.yearsumN$propdif.L1) >=2), "delta2_L2"] <- 1


table(df.yearsumN$delta2_L2) ##196 years in 68 different groups

length(unique(df.yearsumN[which(df.yearsumN$delta2_L2==1),]$groupID)) ##80

hadgap2L2 <- unique(df.yearsumN[df.yearsumN$delta2_L2==1,]$groupID)


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
df.yearsumN$gap25 <- 0
df.yearsumN[which(abs(df.yearsumN$propdiff) <= .25 &
                    df.yearsumN$modeledarticles >0), "gap25"] <- 1

table(df.yearsumN$gap25) ## 10 event threshold: 1882  no, 231  yes

length(unique(df.yearsumN[which(df.yearsumN$gap25==1),]$groupID))
## 140 group-years between the topics is less than .25

## Gap less than .5 & pos. number of articles modeled
df.yearsumN$gap50 <- 0
df.yearsumN[which(abs(df.yearsumN$propdiff) <= .5 &
                    df.yearsumN$modeledarticles >0), "gap50"] <- 1


## gap between the topics is less than .5
table(df.yearsumN$gap50)## 10 threshold: 1664  no, 449 yes in 176 groups

length(unique(df.yearsumN[which(df.yearsumN$gap50==1),]$groupID))

##%%%%%%%%%%%%%%%%%%%%%%%%%
## Per-group count
## years between framing change periods

colnames(df.yearsumN)

cols <-  c("year",
           "countT1", "countT2",
           "propdiff","propdif.L1",
           "delta1", "delta1_L2","gap25")

## Nested loops to iterate through the list of groups
## and create a counter:
## Counter has: 0 until a change
## 0 in the year that delta = 1
## plus 1 for every year until the next change 
## [Not efficient, but more interpretable]

df.yearsumN$yearsSinceChange <- NA
df.yearsumN$counter <- NA
for(g in unique(df.yearsumN$groupID)){
  print(g)
  for(r in 1:dim(df.yearsumN[which(df.yearsumN$groupID==g),])[1]){
    print(r)
    if(r==1){
      df.yearsumN[which(df.yearsumN$groupID==g),][r, "counter"] <- 0
    } 
    else{
      if(df.yearsumN[which(df.yearsumN$groupID==g),][r-1, "delta1"]==1){
        df.yearsumN[which(df.yearsumN$groupID==g),][r,"counter"] <- 1
      }else{
        if(df.yearsumN[which(df.yearsumN$groupID==g),][r-1, "counter"] > 0){
          df.yearsumN[which(df.yearsumN$groupID==g),][r, "counter"] <- df.yearsumN[which(df.yearsumN$groupID==g),][r-1, "counter"] + 1
        }else{
          if(df.yearsumN[which(df.yearsumN$groupID==g),][r-1, "counter"]==0){
            df.yearsumN[which(df.yearsumN$groupID==g),][r, "counter"] <- 0
          }
        }
      }
    }
  }
}


## now zero out "counter" for years of change:

df.yearsumN[which(df.yearsumN$delta1==1), "counter"] <- 0

## Add a tag for what round this is:
df.yearsumN$N <- N
df.yearsumN$Th <- Th

df.basedataN$N <- N
df.basedataN$Th <- Th

## Checkpoint #2. Uncomment out if want to save:
##save(df.yearsumN, df.basedataN, hadgap2, hadgap1.5,
##     file=paste0(outPath,"yearsumAndRelated_", N, "_", Th, ".Rdata"))

print("Part 2 finished, data saved")

## Prep for termination rep:

  dataPath <- "./data/"
  termination <- read_dta(paste0(dataPath,
                               "Termination-data-ISQ.dta"))

  idkey <- read_csv(paste0(dataPath,"translate_actor.csv"))


  t.groups.after1989 <- unique(termination[which(
  termination$year>=1989),]$sidebid)

  c.groups <- unique(df.yearsumN$groupID)

  termination$sidebid <- as.numeric(termination$sidebid)
  ids.termination <- unique(termination$sidebid)
  
  termination <- merge(termination,
                       idkey,
                       by.x=("sidebid"),
                       by.y=("old_id"),
                       all.x=TRUE)
  
  dim(termination)
  
  colnames(termination)
  
  ids.yearsum <- unique(df.yearsumN$groupID)
  terminationplus1 <- merge(x=termination,
                            y=df.yearsumN,
                            by.x=c("new_id", "year"),
                            by.y=c("groupID", "year"),
                            all.x=TRUE)
  
  ## terminationplus1 is more or less their original data
  ## Need to limit terminationplus to 1989 and later
  ## becuase that is what I have change data for
  
  terminationplus <- terminationplus1[which(
    terminationplus1$year>=1989),]
  
  dim(terminationplus) ## 1229 x 80
  
  colnames(terminationplus)
  
  ## summarizing the deltas that I introduced
  ## (with an eye for the number of NAs:)
  summary(terminationplus$delta1.5) ## 111 NAs
  
  ## so what groups and dyads are NA in the post 1989 subset:
  terminationplus1 <- terminationplus[is.na(
    terminationplus$propdiff),] ## 295
  terminationplus2 <- terminationplus[is.na(
    terminationplus$delta1.5),] ## 404
  
  unique(terminationplus1$sideb) 
  
  ## Identify the armed groups that had a change of
  ## 1, 1.5 and greater than 2:
  
  haddelta1 <- unique(terminationplus[which(
    terminationplus$delta1==1),]$new_id)
  
  haddelta1.5 <- unique(terminationplus[which(
    terminationplus$delta1.5==1),]$new_id)
  
  haddelta2 <- unique(terminationplus[which(
    terminationplus$delta2==1),]$new_id)
  
  length(haddelta1) ## 85 groups
  length(haddelta1.5) ## 51 groups
  length(haddelta2) ## 27 groups
  
  ## Add in the indicator variable for whether
  ## the militant group in a dyad had a change
  ## In any year:
  
  terminationplus$haddelta1 <- 0
  terminationplus$haddelta1 <- ifelse(terminationplus$new_id
                                      %in% haddelta1, 1, 0)
  
  terminationplus$haddelta1.5 <- 0
  terminationplus$haddelta1.5 <- ifelse(terminationplus$new_id
                                        %in% haddelta1.5, 1, 0)
  terminationplus$haddelta2 <- 0
  terminationplus$haddelta2 <- ifelse(terminationplus$new_id
                                      %in% haddelta2, 1, 0)
  
  
  numchanges<- terminationplus %>%
    group_by(new_id) %>%
    summarise(numchanges=sum(delta1==1))
  
  summary(numchanges$numchanges) ## 0-4 (78 NA)
  
  hist(numchanges$numchanges)
  
  p <- ggplot(numchanges,
              aes(x=numchanges)) +
    geom_histogram() +
    xlab("Number of Frame Change Years") +
    ylab("Count")+
    ggtitle("Summary of Number of Changes Variable")+
    theme_bw()
  
  p
  
  ## Outlier identification:
  numchanges[which(numchanges$numchanges==4),]##2
  numchanges[which(numchanges$numchanges==3),] ## 8
  
  numchanges[which(numchanges$numchanges==2),] ##19
  
  
  ## 4 changes:
  cols <- c("sideb", "frexWords", 'year')
  terminationplus[which(terminationplus$new_id==277),cols]## Abu Sayyaf
  terminationplus[which(terminationplus$new_id==743), cols]##  FARC
  
  ## merge back in:
  terminationplus <- terminationplus %>%
    left_join(numchanges,
              by="new_id") 
  
  dim(terminationplus) ## 1229 x 90
  
  ##%%%%%%%%%%%%%%%%%%%%%%%%%
  ## summary of max years since change:
  
  ysC <-terminationplus %>%
    group_by(new_id) %>%
    summarise(max = max(counter))
  
  ysC <- ysC[rev(order(ysC$max)),]
  
  ysC <- na.omit(ysC)
  
  dim(ysC) ##213 x 2
  
  head(ysC) ## groups with a longest time after change:
  ## 326 (16 years); 338 (14 years); 551 (12 years); 169 (11 years); 325 (11 years)
  
  ## Some qualitative perspective, if desired:
  terminationplus[which(terminationplus$new_id==326),
                  c(cols, "counter", "delta1")]## UFLA
  
  terminationplus[which(terminationplus$new_id==338),
                  cols]## Republic of Nagorno-Karabakh
  
  terminationplus[which(terminationplus$new_id==551),
                  cols]## OLF
  
  terminationplus[which(terminationplus$new_id==169),
                  cols]## CPP
  
  terminationplus[which(terminationplus$new_id==325),
                  cols]## "Kashmir insurgents"
  
  
  ##%%%%%%%%%%%%%%%%%
  ## Summary statistics
  ## For change and also for the introduced variables
  ##%%%%%%%%%%%%%%%%%%
  
  ## The unit for these is dyad-year:
  table(terminationplus$haddelta1) ## 1397 no; 526 dyad-years
  table(terminationplus$haddelta1.5) ## 1647 no; 276 dyad-years
  table(terminationplus$haddelta2) ## 1775 no; 148 dyad-years
  
  ### Termination X Islamist
  round(prop.table(table(terminationplus$haddelta1)),2) ## 35% of the dyad-years have a delta of 1
  
  round(prop.table(table(terminationplus$haddelta1, terminationplus$islamist)), 2) ## Interestingly, the "change" groups are underrepresented among the groups coded as Islamist
  
  ## number of changes
  ## Count variable
  
  library(stargazer)
  
  ## format a small DF to report summary statistics:
  ## for the binary variables:
  
  binaries <- c("Change year",
                "Change in Prev. 2 years",
                "Had 'Low' Change",
                "Had 'Med.' Change",
                "Had 'High' Change")
  
  df <- as.data.frame(cbind(binaries,
                            rbind(table(terminationplus$delta1),
                                  table(terminationplus$delta1_L2),
                                  table(terminationplus$haddelta1),
                                  table(terminationplus$haddelta1.5),
                                  table(terminationplus$haddelta2))))
  
  ## Figure 3 in text:
  colnames(df) <- c("Variable", "No", "Yes")
  stargazer(df, summary=FALSE, rownames=FALSE)
  
  
  sum.df <- terminationplus[,c("numchanges","counter")]
  
  ## Figure 4 in text:
  stargazer(sum.df,
            covariate.labels =c("Number of Changes",
                                "Years Since Change"
            ))
  
  ##%%%%%%%%%%%%%%%%%%%%%
  ## Save and format for STATA
  ## (aka: remove periods from the column names)
  
  colnames(terminationplus)
  
  colnames(terminationplus)[which(
    colnames(terminationplus)=="propdif.L1")] <- "propdif_L1"
  colnames(terminationplus)[which(
    colnames(terminationplus)=="propdif.L2")] <- "propdif_L2"
  colnames(terminationplus)[which(
    colnames(terminationplus)=="altpropdif.L1")] <- "altpropdif_L1"
  colnames(terminationplus)[which(
    colnames(terminationplus)=="altpropdiff.L1")] <- "altpropdiff_L1"
  colnames(terminationplus)[which(
    colnames(terminationplus)=="delta1.5")] <-"delta15"
  colnames(terminationplus)[which(
    colnames(terminationplus)=="haddelta1.5")] <-"haddelta15"
  
  ## Passes out data
  ## clean up the Th less than zero (avoid period in file path)
  if(Th < 1){
    Thm= Th*100}else{Thm=Th}
  print(Thm)
  
  write_dta(data=terminationplus,
            paste0("./data/terminationplus_",N, "_", Thm, ".dta"))
  
  print("Part 3 done, ready for STATA")
  
print(paste0("Finished for Th= ", Th))
print(paste0("Finished for N= ", N))

}
}