rm(list=ls())

## Make plots for each of the highlighted groups
## in the writeup


library(ggplot2)
library(tidyverse)
library(stargazer)
library(ggrepel)

load(file="./02dfyearsumAndRelatedTinyUpdate.Rdata")

##Which groups had a delta of at least |1|:
haddelta1 <- df.yearsum[which(
    abs(df.yearsum$propdif.L1) >= 1),]

dim(haddelta1) ## 335 x 16

c.groups <- unique(haddelta1$groupID) ## change groups
a.groups <- unique(df.yearsum$groupID) ## all groups

length(a.groups) ## 260
length(c.groups) ## 156

groups.nochange <- setdiff(a.groups,c.groups)

length(groups.nochange) ## 104

subset <- df.yearsum[which(
    df.yearsum$groupID %in% groups.nochange),]


## filter by length of operation:

subset.freqs <- as.data.frame(table(subset$groupID))
subset.freqs <- subset.freqs[order(subset.freqs$Freq),]

## Qualitative notes: longest operating group without "change"
## is 209= Hamas ;then 488 = LRA; 
##397 = ONLF (Ethiopian liberation front), 
##539: AQIM, 
##529 = MFDC, secessionist rebel group in Senegal

## what is the Hamas framing:
#print(subset[which(subset$groupID==209),], n=32)
#print(subset[which(subset$groupID==209),
#             c("year", "frexWords")], n=32)

## Other higher-activity; low change groups:
#print(subset[which(subset$groupID==488),], n=30) ## LRA
#print(subset[which(subset$groupID==497),], n=24) ## ONLF; 
#print(subset[which(subset$groupID==539),], n=24) ## AQIM framing as terrorism
##print(subset[which(subset$groupID==529),], n=24) ## MFC; framing as terrorist violence
##print(subset[which(subset$groupID==315),], n=24) ## UNLF; socialist separatist rebel group
#print(subset[which(subset$groupID==201),], n=24) ## KIO; Katrchin separatist group

## Some notes on the low change:
## LRA has a couple in the second topic;
## all of the ONLF's paragraphs are associated with
## the dominant topic, and the two topics are similar
## So that is a good example of a group with an exceptionally
## dominant framing

#####################
#####################
## AQAP Case
aqap.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==881),]

aqap.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 881), ]

## This graph has a big change in AQAP activities after 2015;
## Interestingly, so does UCPD's writeup: formerly the dyads were mostly AQAP-Ansaraullah
###then after 2015 they say that the activity is mostly tribal clashes between
## AQAP -Forces of Hadi 

t1 <- unique(aqap.sub.year[which(
    aqap.sub.year$propT1>.75),]$frexWords)

t2 <- unique(aqap.sub.year[which(
    aqap.sub.year$propT2>.75),]$frexWords)

## Ggplot version:

textanchor <- min(aqap.sub$year)+5
num.articles <- dim(aqap.sub)[1]
x.breaks <- min(unique(aqap.sub$year)):max(unique(aqap.sub$year))

## highlight examples of stories before and after:

colnames(aqap.sub) ## want:
cols <- c("id", "source_article",
          "year", "maxtopic","scaledvalue")

## want to highlight on both sides of the 2015 switch:

aqap.sub[which(aqap.sub$year==2015), cols]
aqap.sub[which(aqap.sub$year==2010), cols]

aqap.sub[which(aqap.sub$year==2009), cols]

## two articles on either side of the scale:
##  190499 (+.76) &  190503 (-.74);
## 190500 (+.75); 190512 (-.75)

sink(file="matchingPairAQAPStories.txt")

aqap.sub[which(aqap.sub$year==2015 &
               aqap.sub$scaledvalue==.75), cols]

aqap.sub[which(aqap.sub$year==2015 &
               aqap.sub$scaledvalue==-.75), cols]
sink()



aqap.sub[which(aqap.sub$year==2015 &
               aqap.sub$scaledvalue==.75), cols]

aqap.sub[which(aqap.sub$year==2015 &
               aqap.sub$scaledvalue==.75), cols]

aqap.sub[which(aqap.sub$year==2010 &
               aqap.sub$scaledvalue==-.54), cols]

aqap.sub[which(aqap.sub$year==2010 &
               aqap.sub$scaledvalue==.79), cols]

aqap.sub[which(aqap.sub$year==2020), cols]



t1.neg75 <- c("Al-Qaida kills 6 captured Yemeni rebels \n north of Aden")
t2.75 <- c("'Qaeda' ambush kills 5 Yemen soldiers")


## Make the subset and annotation columns:

aqap.sub$highlight <- 0
aqap.sub$caption <- ''

aqap.sub[which(aqap.sub$id==203501), "highlight"] <- 1
aqap.sub[which(aqap.sub$id==203501), "caption"] <- t2.75
aqap.sub[which(aqap.sub$id==205671), "highlight"] <- 1
aqap.sub[which(aqap.sub$id==205671), "caption"] <- t1.neg75

aqap.sub[which(aqap.sub$id==718), "highlight"] <- 1
aqap.sub[which(aqap.sub$id==718), "caption"] <- "Qaeda suspect kills two guards in Yemen hospital escape bid"

aqap.sub[which(aqap.sub$id==220), "highlight"] <- 1
aqap.sub[which(aqap.sub$id==220), "caption"] <- "Yemeni security forces said on alert for Qa'idah attacks in south"

aqap.sub[which(aqap.sub$id==367164), "highlight"] <- 1
aqap.sub[which(aqap.sub$id==367164), "caption"] <- "Al-Qaeda in Yemen claims attacks on Houthis despite defeat proclamation"


## Directly annotate the stories on the plot:

textanchor <- min(aqap.sub$year)+3

p.aqap2 <-ggplot(aqap.sub, aes(x=year, y=scaledvalue))+
    geom_jitter(width = 0.1, height = 0.1, alpha=.35)+
    geom_hline(yintercept=0, size=.75,
               color="red", alpha=.5) +
    geom_line(data=aqap.sub.year, aes(y=propdiff, x=year),
              color="darkgreen", linetype="dashed")+
    geom_label_repel(box.padding = 0.5,
                     max.overlaps = Inf,
                     data=as.data.frame(
                         aqap.sub[which(aqap.sub$highlight==1),]),
                     aes(label=caption))+
    geom_point(data=as.data.frame(
                   aqap.sub[which(aqap.sub$highlight==1),]),
               color = "red")+
    annotate("text", x = textanchor, y =1.15,
             label = paste0("Topic Two: ", t2))+
    annotate("text", x =textanchor, y = -1.15,
             label = paste0("Topic One: ", t1)) +
    xlab("Year")+
    scale_x_continuous(breaks=x.breaks,
                   guide = guide_axis(n.dodge = 2))+
    ylab("Proportion Topic 1 (-1) vs Topic 2 (1)") +
    ggtitle("AQAP Yearly Classification",
            subtitle=paste0("N = ", num.articles," Articles"))+
    theme_bw()

p.aqap2

ggsave(p.aqap2,
       width=12, 
       height=8,
       units=c("in"),
       file="annotedAQAPGraph.pdf")

#############
## Abu Sayyaf
############
## Do the Abu Sayyaf results look the same as in the version with fewer stopwords:
##A: no; more variability in polarity

as.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==277),]
as.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 277), ]
as.sub.year

print(as.sub.year, n=28)
as.sub.year$frexWords

### Topic 1: rebel,r,muslim,extremist,rescu,indonesian,moslem
## Topic 2: "sayyaf,abu,pna,sulu,wound,s,asg"
## Which sounds like kidnappings/hostage in Topic 1 ("resuce [from] extremist AS group.." and clashes in Topic 2

## The plot looks like more clashes and less hostage taking and hostage liberation
## after about 2010. But there is also a very clear period of oscellation between the two
## modes of operation, especially after the first leader was killed
## until about 2010, the results parallel the UCDP writeup. There isn't much in the UCDP writeup after 2010
## but the results look like some kidnapping and hostage but more rebel and insurgency

## FREX Words to overlay
t1 <- unique(as.sub.year[which(
    as.sub.year$propT1>.75),]$frexWords)

t2 <- unique(as.sub.year[which(
    as.sub.year$propT2>.75),]$frexWords)

textanchor <- max(as.sub$year)-10
num.articles <- dim(as.sub)[1]
x.breaks <- min(unique(as.sub$year)):max(unique(as.sub$year))


## Find a matching Abu Sayaaf pair:
## their first big switch was 2001-2002
dim(as.sub)
summary(as.sub$year)

cols <- c("id","source_article",
          "year", "maxtopic","scaledvalue")

## Build in the highlighted articles
## in a way that is efficient for ggplot:
as.sub$highlight <- 0
as.sub$caption <- ''

as.sub[which(as.sub$year==2000), cols]## 

as.sub[which(as.sub$year==2002), cols] ## 298896 (as miliants)

as.sub[which(as.sub$year==2007), cols]##

as.sub[which(as.sub$year==2013), cols]## max:  .94

as.sub[which(as.sub$year==2012), cols]## max:  .94
as.sub[which(as.sub$year==2014), cols]##
as.sub[which(as.sub$year==2015), cols]##
as.sub[which(as.sub$year==2019), cols]## max:  .88; .83.; .79
## min: -.83; -.78 x 2 (pull the .79; -.78 pair)

as.sub[which(as.sub$year==2020), cols]## max:  .94
## min: -.81

as.sub[which(as.sub$year==2000 & ##Pull 
              as.sub$scaledvalue==.74), cols]

as.sub[which(as.sub$year==2002 & ##Pull 
             as.sub$scaledvalue==.84), cols]

as.sub[which(as.sub$year==2002 &
             as.sub$scaledvalue==-.84), cols]

as.sub[which(as.sub$year==2013 & ##Pull 
              as.sub$scaledvalue==-.71), cols]


as.sub[which(as.sub$year==2015 & ## killed dutch hostage
             as.sub$scaledvalue==-.62), cols]


as.sub[which(as.sub$year==2019 & ## killed dutch hostage
             as.sub$scaledvalue==-.78), cols]

as.sub[which(as.sub$year==2019 & ## AS member killed
             as.sub$scaledvalue==.83), cols]

as.sub[which(as.sub$year==2020  & ## suicide bomber blasts
             as.sub$scaledvalue==-0.81), cols]


as.sub[which(as.sub$year==2020  & ## suicide bomber blasts
             as.sub$scaledvalue==0.94), cols]

## Captions:

as.sub[which(as.sub$id== 124923),
       "highlight"] <- 1
as.sub[which(as.sub$id==124923),
       "caption"] <- "Six Abu Sayyaf fighters, two soldiers dead in clash"

as.sub[which(as.sub$id==123716),
       "highlight"] <- 1
as.sub[which(as.sub$id==123716),
       "caption"] <- "Philippines foils rebel bid to escape hostage isle"

as.sub[as.sub$id==298896, "caption"] <- "IS claims killing soldier in southern Philippines"
as.sub[as.sub$id==298896, "highlight"] <- 1

as.sub[which(as.sub$id== 203613),
       "highlight"] <- 1
as.sub[which(as.sub$id==203613),
       "caption"] <- "Al-Qaeda-linked militants behead Philippine militiaman"


colnames(as.sub)


textanchor <- max(as.sub$year)-10
num.articles <- dim(as.sub)[1]
x.breaks <- min(unique(as.sub$year)):max(unique(as.sub$year))

p.as2 <-ggplot(as.data.frame(as.sub),
               aes(x = year, y = scaledvalue))+
    geom_jitter(width = 0.1, height = 0.1, alpha=.35)+
    geom_label_repel(box.padding = 0.5,
                    max.overlaps = Inf,
                    data=as.data.frame(
                        as.sub[which(as.sub$highlight==1),]),
                    aes(label=caption))+
    geom_point(data=as.data.frame(
                   as.sub[which(as.sub$highlight==1),]),
               color = "red")+
    geom_hline(yintercept=0, size=.75,
                   color="red", alpha=.5) +
    geom_line(data=as.sub.year, aes(y=propdiff, x=year),
              color="darkgreen", linetype="dashed")+
    xlab("Year")+
    scale_x_continuous(breaks=x.breaks,
                   guide = guide_axis(n.dodge = 2))+
    ylab("Proportion Topic 1 (-1) vs Topic 2 (1)") +
    ggtitle("Abu Sayyaf Yearly Classification",
            subtitle=paste0("N = ", num.articles," Articles"))+
    theme_bw()

p.as2

ggsave(p.as2,
       width=12, 
       height=8,
       units=c("in"),
       file="annotedAbuSayyaf.pdf")

#############
## ULFA
############

ulfa.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==326),]
ulfa.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 326), ]

ulfa.sub.year
ulfa.sub.year$frexWords

##Topic 1:blast,four,six,new,policemen,bomb,ambush"
## Topic 2: "rebel,trust,troop,cdps,forc,arm,least"

## Plot suggsts more terrorism in about ~1990
## then about 2010- 2015; then about 2016
##pdf(file="ULFAWithMaximalistStopwords.pdf")

dim(ulfa.sub) ## 526 x 53

## How many SATP articles:
dim(ulfa.sub[which(ulfa.sub$source_article=="SATP"),]) ## 71 articles out of 526
table(ulfa.sub[which(
    ulfa.sub$source_article=="SATP"),]$year) ## 71 articles out of 526

## Articles to highlight;

ulfa.sub$highlight <- 0
ulfa.sub$caption <- ''


ulfa.sub[which(ulfa.sub$year==1991), cols]

              
ulfa.sub[which(ulfa.sub$year==1990 & 
             ulfa.sub$scaledvalue > .5), cols]

ulfa.sub[which(ulfa.sub$year==2005 & 
             ulfa.sub$scaledvalue > .5), cols]

ulfa.sub[which(ulfa.sub$year==1993 & 
             ulfa.sub$scaledvalue <= -.5), cols]

ulfa.sub[which(ulfa.sub$year==2000 & 
               ulfa.sub$scaledvalue < -.75), cols]

ulfa.sub[which(ulfa.sub$year==2005 & 
             ulfa.sub$scaledvalue < -.75), cols]

ulfa.sub[which(ulfa.sub$year==2013 & 
             ulfa.sub$scaledvalue > .5), cols]


ulfa.sub[which(ulfa.sub$year==2014 & 
               ulfa.sub$scaledvalue <= .5), cols]

ulfa.sub[which(ulfa.sub$year==2015 & 
             ulfa.sub$scaledvalue > .5), cols]


ulfa.sub[which(ulfa.sub$year==2016 & 
             ulfa.sub$scaledvalue > .5), cols]


ulfa.sub[which(ulfa.sub$id==94984),"highlight"]  <- 1
ulfa.sub[which(ulfa.sub$id==94984), "caption"] <- "TWO KILLED AS INDIAN TROOPS HUNT FOR GUERRILLAS IN ASSAM"

ulfa.sub[which(ulfa.sub$id==95258), "highlight"] <- 1
ulfa.sub[which(ulfa.sub$id==95258), "caption"] <- "Indian troops kill 12 rebels in troubled northeast"

ulfa.sub[which(ulfa.sub$id==94990), "highlight"] <- 1
ulfa.sub[which(ulfa.sub$id==94990), "caption"] <- "TWO POLICEMEN KILLED BY SUSPECTED ASSAM MILITANTS"

ulfa.sub[which(ulfa.sub$id==95275), "highlight"] <- 1
ulfa.sub[which(ulfa.sub$id==95275), "caption"] <- "One killed, seven wounded in grenade attack in northeast India"

ulfa.sub[which(ulfa.sub$id==95084), "highlight"] <- 1
ulfa.sub[which(ulfa.sub$id==95084), "caption"] <- "Five policemen, civilian killed in Assam ambush"

ulfa.sub[which(ulfa.sub$id==156739), "highlight"] <- 1
ulfa.sub[which(ulfa.sub$id==156739), "caption"] <- "Trader shot dead in Kokrajhar"

ulfa.sub[which(ulfa.sub$id==67177), "highlight"] <- 1
ulfa.sub[which(ulfa.sub$id==67177), "caption"] <- "ULFA militant killed, arms recovered"



## FREX Words to overlay
t1 <- unique(ulfa.sub.year[which(
    ulfa.sub.year$propT1>.75),]$frexWords)

t2 <- unique(ulfa.sub.year[which(
    ulfa.sub.year$propT2>.75),]$frexWords)

textanchor <- max(ulfa.sub$year)-10
num.articles <- dim(ulfa.sub)[1]
x.breaks <- min(unique(ulfa.sub$year)):max(unique(ulfa.sub$year))

## Anotated graph

textanchor <- max(ulfa.sub$year)-10
num.articles <- dim(ulfa.sub)[1]
x.breaks <- min(unique(ulfa.sub$year)):max(unique(ulfa.sub$year))

p.ulfa2 <-ggplot(as.data.frame(ulfa.sub),
               aes(x = year, y = scaledvalue))+
    geom_jitter(width = 0.1, height = 0.1, alpha=.35)+
    geom_label_repel(box.padding = 0.5,
                    max.overlaps = Inf,
                    data=as.data.frame(
                        ulfa.sub[which(ulfa.sub$highlight==1),]),
                    aes(label=caption))+
    geom_point(data=as.data.frame(
                   ulfa.sub[which(ulfa.sub$highlight==1),]),
               color = "red")+
    geom_hline(yintercept=0, size=.75,
                   color="red", alpha=.5) +
    geom_line(data=ulfa.sub.year, aes(y=propdiff, x=year),
              color="darkgreen", linetype="dashed")+
    xlab("Year")+
    scale_x_continuous(breaks=x.breaks,
                   guide = guide_axis(n.dodge = 2))+
    ylab("Proportion Topic 1 (-1) vs Topic 2 (1)") +
    ggtitle("UFLA Yearly Classification",
            subtitle=paste0("N = ", num.articles," Articles"))+
    theme_bw()

p.ulfa2 

ggsave(p.ulfa2,
       width=12, 
       height=8,
       units=c("in"),
       file="annotedulfa.pdf")

#### Before/after change

ulfa.sub[which(ulfa.sub$year==2009), cols]

ulfa.sub[which(ulfa.sub$year==2009 &
               ulfa.sub$scaledvalue==.56), cols]

ulfa.sub[which(ulfa.sub$year==2009 &
               ulfa.sub$scaledvalue==-.57), cols]

##############################
## Pattani (365)
#############################

## 18 conflict years:

pt.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==365),]
pt.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 365), ]

pt.sub.year$frexWords

## FREX Words to overlay
t1 <- unique(pt.sub.year[which(
    pt.sub.year$propT1>.75),]$frexWords)

t2 <- unique(pt.sub.year[which(
    pt.sub.year$propT2>.75),]$frexWords)


textanchor <- max(pt.sub$year)-10
num.articles <- dim(pt.sub)[1]
x.breaks <- min(unique(pt.sub$year)):max(unique(pt.sub$year))


############################
## UNITA
############################

## NOTE: need to remove the 2013 entry;
## that is not UNITA as an armed conflict group
## it is UNITA-as-political party complaining about
## violence against them

unita.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==567),]

unita.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 567), ]

unita.sub <- unita.sub[which(unita.sub$year < 2005),]
unita.sub.year <- unita.sub.year[which(
    unita.sub.year$year < 2005),]

unita.sub.year ## 1986 - 2002

## Topic One FREX
t1 <- unique(unita.sub.year[which(
    unita.sub.year$propT1>.75),]$frexWords)

t2 <- unique(unita.sub.year[which(
    unita.sub.year$propT2>.75),]$frexWords)

##Topic 1:african,defenc,p,black,cockerel,resist,nacion
## Topic 2: kill,mon,oper,soldier,armi,kup,th
## Austin tells me that the two topics do make sense:
## Topic One is a framing that references their
## operation and origins as a national liberation movement
## Topic Two picks up perceptions of a violent armed group
## This does relate to tactical evolution: after the 1998 elections, UNITA was percieved to be more of a group that was violent for violences' sake. In fact, the switch over happens in the reporting data two years before the election [[look for some 1996 inflection point?]]

dim(unita.sub) ## 1455 x 55

## One inflection point around 1991;
## then a period of transition ~1996-2003
textanchor <- max(unita.sub$year)-5
num.articles <- dim(unita.sub)[1]
x.breaks <- min(unique(unita.sub$year)):max(unique(unita.sub$year))


#############
## ONLF
############
## Chosen as a long-running and very stable
## group; though this is a bad one becuase
## the framing is so stable that the #1 and #2 topics are
## literally teh same

onlf.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==497),]
onlf.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 497), ]

onlf.sub.year
onlf.sub.year$frexWords

## This is a very, very stable presentation
dim(onlf.sub) ## 700 x 54

## FREX Words to overlay
t1 <- unique(onlf.sub.year[which(
    onlf.sub.year$propT1>.75),]$frexWords)

t2 <- unique(onlf.sub.year[which(
    onlf.sub.year$propT2>.75),]$frexWords)


textanchor <- max(onlf.sub$year)-10
num.articles <- dim(onlf.sub)[1]
x.breaks <- min(unique(onlf.sub$year)):max(unique(onlf.sub$year))


#############
## LRA
############

lra.sub <- df.basedata[which(
    df.basedata$side_b_dset_id==488),]
lra.sub.year <- df.yearsum[which(
    df.yearsum$groupID == 488), ]

lra.sub.year
lra.sub.year$frexWords

## This is a very, very stable presentation
dim(lra.sub) ## 1038 x 54

summary(lra.sub.year$propT2)

## FREX Words to overlay
## both the LRA and the ONLF have very stable
## presentations; and they also have topics that are
## pretty similar
t1 <- unique(lra.sub.year[which(
    lra.sub.year$propT1>.75),]$frexWords)

## T2 words are pulled from the implementAlt.R
## script b/c the way that I processed the data
## makes it difficult to pull out FrexWords for topics
## that are never dominant in a year

## t2 <- unique(lra.sub.year[which(
##     lra.sub.year$propT2>.5),]$frexWords)

t2 <-c("london,kill,rebel,africa,armi,new,vision")

lra.sub.year[which(lra.sub.year$year==1996),]

textanchor <- max(lra.sub$year)-10
num.articles <- dim(lra.sub)[1]
x.breaks <- min(unique(lra.sub$year)):max(unique(lra.sub$year))


## Annoted

lra.sub$caption <- ''
lra.sub$highlight <- 0
              
lra.sub[which(lra.sub$year==1990 & 
             lra.sub$scaledvalue <= .5), cols]

lra.sub[which(lra.sub$year==2009 & 
             lra.sub$scaledvalue > .25), cols]

lra.sub[which(lra.sub$year==2017 & 
              lra.sub$scaledvalue <= .5), cols]

lra.sub[which(lra.sub$id==36752), "highlight"] <- 1
lra.sub[which(lra.sub$id==36752), "caption"] <- "UGANDAN REBELS KILL 20 PEOPLE"

lra.sub[which(lra.sub$id==239443), "highlight"] <- 1
lra.sub[which(lra.sub$id==239443), "caption"] <- "Uganda LRA rebels kill 'scores' in northern DR Congo"

lra.sub[which(lra.sub$id==37656), "highlight"] <- 1
lra.sub[which(lra.sub$id==37656), "caption"] <- "The Wizard of the Nile [The Hunt for Joseph Kony]"


p.lra2 <-ggplot(as.data.frame(lra.sub),
               aes(x = year, y = scaledvalue))+
    geom_jitter(width = 0.1, height = 0.1, alpha=.35)+
    geom_label_repel(size=2.5,
      box.padding = .5,
                    max.overlaps = Inf,
                     data=as.data.frame(
                        lra.sub[which(lra.sub$highlight==1),]),
                    aes(label=caption))+
    geom_point(data=as.data.frame(
                   lra.sub[which(lra.sub$highlight==1),]),
               color = "red")+
    geom_hline(yintercept=0, size=.75,
                   color="red", alpha=.5) +
    geom_line(data=lra.sub.year, aes(y=propdiff, x=year),
              color="darkgreen", linetype="dashed")+
    xlab("Year")+
  ylim(-1, 1)+
    scale_x_continuous(breaks=x.breaks,
                   guide = guide_axis(n.dodge = 2))+
    ylab("Proportion Topic 1 (-1) vs Topic 2 (1)") +
    ggtitle("LRA Yearly Classification",
            subtitle=paste0("N = ", num.articles," Articles"))+
    theme_bw()

p.lra2

ggsave(p.lra2,
       width=12, 
       height=8,
       units=c("in"),
       file="annotedLRA.pdf")
