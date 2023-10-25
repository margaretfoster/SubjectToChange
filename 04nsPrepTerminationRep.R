## Script to replicate and extend
## Nilsson and Svensson 2021
## with transformation measure

rm(list=ls())

library(tidyverse)
library(haven)
library(dplyr)
library(haven)

dataPath <- "./data/"

termination <- read_dta(paste0(dataPath,
                               "Termination-data-ISQ.dta"))

changes <- load(paste0("./output/",
                       "02dfyearsumAndRelatedTinyUpdate.Rdata"))

idkey <- read_csv(paste0(dataPath,"translate_actor.csv"))

dim(termination) ## 1923 x 64
dim(df.yearsum) ## 2180 x 16

## Add the new/old ID key:

t.groups.after1989 <- unique(termination[which(
    termination$year>=1989),]$sidebid)

c.groups <- unique(df.yearsum$groupID)

## Want to merge in change information based on
## groupID and year:

## Note that the termination group IDs are very different from the ones
## that I have. Nilsson and Svensson based their work on:
##UCDP Conflict Termination Dataset version 2–2015, which is before
## UCDP changed the id numbering scheme.
## Download an old id-new id table
## ("translate actor" csv)

termination$sidebid <- as.numeric(termination$sidebid)
ids.termination <- unique(termination$sidebid)

termination <- merge(termination,
                     idkey,
                     by.x=("sidebid"),
                     by.y=("old_id"),
                     all.x=TRUE)

dim(termination)

colnames(termination)

ids.yearsum <- unique(df.yearsum$groupID)
terminationplus1 <- merge(x=termination,
                         y=df.yearsum,
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
write_dta(data=terminationplus,
          "./data/terminationplus.dta")

##%%%%%%%%%%%%%%%%%%%%%%%
## Make correlation plots:
##%%%%%%%%%%%%%%%%%%%%%%
library(corrplot)

colnames(terminationplus)

##summary(terminationplus)

tocompare <- c("haddelta1",
               "haddelta1_5",
               "haddelta2",
               "transstart", ## 169 NA
               "islamist", "muslimid",
               "rebextpartdummy", ## 182 NA
               "strongstart", ##92 NA
               "forinvstart",
               "muslimajstart",
               "oilstart", ## 37 NA
               "duration" ,
               "lngdppc", ## 33 NA
               "lnpop",## 33 NA
               "youthstartap", ##112 NA
               "foreignfighter",
               "number_group",
               "secsup_govgov", ## 102 NA
               "anostart" , ## 96 na
               "anocracy", ## 39 NA
               "rebv",
               "lowcease",
               "territory" ,
               "pa", "ca","govv",
               ## "outcome",  ## 800 plus NA, distorts data avali.
               "intensitylevel")
 
## just variable for the termination model:


term.mod.vars <- c("term",# ""Termination"
              "islamist",# "Islamist claim"
              "haddelta1",# "Had Change |1|"
              "haddelta15",# "Had Change |1.5|"
              "haddelta2",# "Had Change |2|"
              "territory",# "Territory"
              "duration",# "Duration"
              "intensitylevel",# "War"
              "number_group",# "Number of groups"
              "strongstart",# "Strong rebels"
              "anostart",# "Anocracy"
              "lngdppcstart",# "GDP per capita"
              "lnpopstart",# "Population"
              "muslimajstart",# "Muslim majority"
              "oilstart",# "Oil"
              "youthstartap",# "Youth bulge/adult pop."
              "anocracy",# "Anocracy over time"
              "lngdppc",# "GDP per capita over time"
              "lnpop",# "Population over time"
              "foreignfighter",# "Foreign fighters"
              "govmilsupport",# "Government support"
              "leftist",# "Leftist"
              "nonislamistrel",# "Non-Islamist religious claims"
              "muslimid",# "Muslim identity"
              "secsup_govgov",# "Government secondary support"
              "rebextpartdummy")# "Rebel support"

## Names:
term.mod.names <- c("Termination",
                    "Islamist claim",
                    "Had Change |1|",
                    "Had Change |1.5|",
                    "Had Change |2|",
                    "Territory",
                    "Duration",
                    "War",
                    "Number of groups",
                    "Strong rebels",
                    "Anocracy",
                    "GDP per capita",
                    "Population",
                    "Muslim majority",
                    "Oil",
                    "Youth bulge",
                    "Anocracy",
                    "GDP p.c.",
                    "Population",
                    "Foreign fighters",
                    "Gvt support",
                    "Leftist",
                    "Non-I. Rel. claims",
                    "Muslim identity",
                    "Gvt sec. support",
                    "Rebel support") 

### Just the model variables
## Also need to rename for
## presentation:

##send out the termination plus data
## don't need to subset: stata will do that I think)

##%% Subset for some other correlation plots

datsubset <- terminationplus[,term.mod.vars]
colnames(datsubset) <- term.mod.names

head(datsubset)

corrmod2 <- cor(datsubset,
                use="complete.obs")

## Note: Hand crop b/c corrplot prints with huge margins
pdf(file="correlationPlotTermVars.pdf")
corrplot(corrmod2,
         method="shade",
        ## type="lower",
         order="hclust",
         tl.cex = .8,
         tl.col="black",
         addrect=2,
         mar=c(1,1,1,1))
dev.off()
 
##save(terminationplus, termination,
##     file="terminationPlusforRep.Rdata")

print("Finished making figures; Formatting data for STATA")