rm(list=ls())


library(ggplot2)
library(ggridges)
library(tidyverse)
library(readxl)
library(countrycode)
library(sandwich)
library(lmtest)
library(plm)
library(ggplot2)

outPath <- "./output/"

load(file=paste0(outPath,
         "02dfyearsumAndRelatedTinyUpdate.Rdata"))

##%%%%%%%%%%%%%%%%%%%%%%%%
## Event clarity
##%%%%%%%%%%%%%%%%%%%%%%%%

##Question from OPSC:
## Is the approach less likely to find change
## in conflicts with more difficult reporting environments?

colnames(df.basedata)

dim(df.basedata) ## 190688

## Variables:
## event_clarity:
## 1 for more precisely described events
## 2 for events with aggregation already

## where_prec: precision of reporting of event
## 1 exact location known and coded (85359)
## 2 max 25km around a known point (40084))
## 3 only second-order administrative district known (30624))
## 4 only first-order admin district known (25445)
## 5 only genreal landscape term known (6081)
## 6 only country known (2920)
## 7 international waters or airspace (175)

table(df.basedata$where_prec)
## conflict-year version:

colnames(df.yearsum) ## group-year

## need group-country-year
## start by seeing how often group-year maps
## onto a single country

gcy <- unique(df.basedata[,c("year",
                           "side_b_new_id",
                           "country")])

dim(gcy) ## 2655 x 3

length(unique(gcy$year)) ## 32


## so any group ID with more than 32 entries has more than one
## country (NB: this is true, but not an exhaustive list of
## all groups with more than one country of operation.)

gcy2 <- as.data.frame(table(gcy$side_b_new_id))
gcy2 <- gcy2[rev(order(gcy2$Freq)),]

head(gcy2)
## 12 actors in the data >  32 times
## 234 = IS (140 entries)
## 323 = PKK
## 769 = al-Qaeda
## 488 = LRA
## 338 = Republic of Artsakh
## 539 = AQIM
## 303 = Taliban
## 208 = PIJ


## Convert from country-year to group-year
## Idea: go into the df.basedata and merge in FH indicators
## for the country and year that the event happened in
## then take a per-group average?
## Need year events x country and country scores

df.basedata <- rename(df.basedata,
                      groupID = side_b_new_id)

df <- df.basedata %>%
    group_by(year, country, groupID) %>%
    summarise(n=n())

dim(df) ## 2655

## Cross-check outcome via AQIM:
print(df[which(df$groupID==539),], n=38)

## event clarity:
## 1 no previous aggregation; 2 is previous aggregation
summary(df.basedata[which(
    df.basedata$groupID==539),]$event_clarity)
## min 1 max 2 for AQIM, mean = 1.048

summary(df.basedata[which(
    df.basedata$groupID==488),]$event_clarity)
## min 1 max 2; mean= 1.175;

## date precision
summary(df.basedata[which(
    df.basedata$groupID==488),]$date_prec)
## min 1 (exact day)  max 5 (between month - year); mean= 1.493

summary(df.basedata[which(
    df.basedata$groupID==488),]$where_prec)

## min 1 (exact location known) max 6 (only country known);
## mean= 2,727 (3= only second-order admin division known)

summary(df.basedata[which(
    df.basedata$groupID==539),]$where_prec)
## min 1, max 6; mean 2.4

## Want group-country-year summary of the precision codes
## but want to cut out the 7s on location precision
## b/c that will distort the clarity
##  Occurance of "7" -- 175 x 57
## (in data this is Somali, Sri Lankan, and Yemeni piracy)
##


df.basedata[which(df.basedata$where_prec==7),
            "where_prec"] <- 0
## 0 shouldn't mess up the averages


df.precision <- df.basedata %>%
    group_by(groupID, year, country_id) %>%
     summarise(
         mean.time.prec= mean(date_prec),
         mean.loc.prec = mean(where_prec),
         mean.clarity = mean(event_clarity)
         )
    

## Add in the country code:
## via ccodes library

df.precision$cow <- countrycode(df.precision$country_id,
                                origin = "gwn",
                                destination = "cowc")

## hand-add 345 Serbia (Yugoslavia) [Not in media dat]
## hand-add 678 Yemen (North Yemen)

df.precision[which(df.precision$country_id==345),
             "iso3c"] <- "YEM"

## Will not have Yugoslavia b/c of time window


## Data source for journalism access
## Goddes and Cary (2021 and 2017) dataset on journalist murders
## They also us Freedom House Measures on media restrictions
## (Presumably in their replication data)
## & V-Dem's  Freedom of Expression and Alternative Sources of Inf## ormation Index

summary(df.basedata$year) ## 1989-2020

##meddat <- read_csv(
##    paste0("../medrestrictionsdata/Carey-Godhes/",
##           "analysis-data-fullsample.csv"))


meddat <- read_csv(
    paste0("../medrestrictionsdata/solis-waggoner/",
           "MSFS-estimates_full-3x2000.csv"))


##fhdat <- read_excel(
##    paste0("../medrestrictionsdata/",
##           "FIW_1973-2022.xlsx"))

##dim(fhdat)

dim(meddat) ## 10739
summary(meddat)## country-year  1948-2017

colnames(meddat)
colnames(df.precision)

dim(df.precision)

df.media <- df.precision %>%
    left_join(meddat,
              by = c("year"= "year",
                  "cow" = "abbr"))

dim(df.media) ## 2655 x12

## Group-country-year average of
## UCDP precision

colnames(df.media)

summary(df.media$MSF)

## range is [0,1],
## min is 0.0127; mean is .5323; max .9972

nas <- df.media[is.na(df.media$MSF)==TRUE,]

dim(nas)
colnames(nas)

table(nas$iso3c)
table(nas$year) ## vast majority 2018, 2019, 2020
## Acceptable

## which are the missing years covered in their data:
print(nas[which(nas$year<2018),
          c("year", "country_id",
             "country",
            "cow", "ccode")],n=41)

## Bring group-year change info into the
## group-country-year df.precision + media info

df.precision2 <- df.media %>%
    left_join(df.yearsum,
              by=c("groupID"= "groupID",
                  "year"= "year"))

dim(df.precision2) ## 2655 x 30


## %%%%%%%%%%%%
## linear regression year-on-year change
## by event precision
## %%%%%%%%%%%%%

## Expectation: less precision, less change
## group clustered SE

## but need to first extract NA from the
## index:

dim(df.precision2[is.na(df.precision2$groupID)==TRUE,]) ##0

dim(df.precision2[is.na(df.precision2$year)==TRUE,]) ##0

dim(df.precision2[is.na(df.precision2$ccode)==TRUE,]) ##357

dim(df.precision2[is.na(df.precision2$country_id)==TRUE,]) ##00


## Need to group indices so that we can feed two to plm
## for the three-way index:

dat <-df.precision2 %>%
    group_by(groupID, country_id) %>%
    mutate(gcid = cur_group_id()) ## creates a grouping id for group-country

dim(dat)

print(dat[,c(1,2,3,29:31)], n=50)

## direct three-way index for group-country-year
## for the zero inflated neg. binomial

dat <-dat %>%
    group_by(groupID, country_id, year) %>%
    mutate(gcyid = cur_group_id()) ## creates a grouping id for group-country

## Know that we want effects, but fixed or random?
## Prep for the Hausman test

## Fixed effects by group
## for change from previous year's ratio
## given precision and media freedom
## BUT taking absolute value of the previous year change
## because otherwise the [-1, 1] scale washes out any possible signal:

mod1 <- plm(abs(propdif.L1) ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
            model=c("within"),## "fixed effects" by grouping
            index=c("year","gcid"),
            data=dat)


summary(mod1)


df <- broom::tidy(mod1)
ci95 <- confint(mod1, level=0.95) %>%
  data.frame() %>%
  rename("conf.low95" = "X2.5..",
         "conf.high95" = "X97.5..")

ci90 <- confint(mod1, level=0.9) %>%
  data.frame() %>%
  rename("conf.low90" = "X5..",
         "conf.high90" = "X95..")

results <- bind_cols(df,
                     ci95,
                     ci90) %>%
    rename(Variable = term,
           Coefficient = estimate,
           SE = std.error) %>%
           filter(Variable != "(Intercept)")

gg1 <- ggplot(results,
       aes(x = Variable, y = Coefficient)) +
        geom_hline(yintercept = 0,
                   colour = gray(1/2), lty = 2) +
        geom_point(aes(x = Variable,
                    y = Coefficient)) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low90,
                     ymax = conf.high90),
                   lwd = 1) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low95,
                     ymax = conf.high95),
                   lwd = 1/2) +
        ggtitle("Outcome: Yearly Proportion Changes") +
    coord_flip() +
    theme_bw()

plot(density(abs(dat$propdif.L1)))

## Random effects by group
## for change from previous year's ratio
## given precision and media freedom
mod2 <- plm(propdif.L1 ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
            model=c("random"),## random effects by grouping
            index=c("groupID"),
            data=dat)


summary(mod2) ## No substantive change in the takeaway; which is that there is
## basically no statistical connection between year-on-year lagged changes that are found
## and location precision, event clarity, country media score.
## small positive association between time precision of the report  and liklihood of a change
## being found (sig at .01)

## more formal: use the Hausman test
phtest(mod1, mod2)
## pvalue not <0.05, so random effects
## serial correlation:
pbgtest(mod2) ## serial correlation in errors, so:


## SE Grouped at group level:
mod2B<- plm(propdif.L1 ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
            model=c("random"),## random effects
            effect="time", ## focus on time
            index=c("groupID"),
            data=dat)

summary(mod2B)


df <- tidy(mod2B)
ci95 <- confint(mod2B, level=0.95) %>%
  data.frame() %>%
  rename("conf.low95" = "X2.5..",
         "conf.high95" = "X97.5..")

ci90 <- confint(mod2B, level=0.9) %>%
  data.frame() %>%
  rename("conf.low90" = "X5..",
         "conf.high90" = "X95..")

results <- bind_cols(df,
                     ci95,
                     ci90) %>%
    rename(Variable = term,
           Coefficient = estimate,
           SE = std.error) %>%
           filter(Variable != "(Intercept)")


gg1 <- ggplot(results,
       aes(x = Variable, y = Coefficient)) +
        geom_hline(yintercept = 0,
                   colour = gray(1/2), lty = 2) +
        geom_point(aes(x = Variable,
                    y = Coefficient)) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low90,
                     ymax = conf.high90),
                   lwd = 1) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low95,
                     ymax = conf.high95),
                   lwd = 1/2) +
    ggtitle("Outcome: Yearly Proportion Changes",
            subtitle= "S.E. Clustered by Group") +
    coord_flip() +
    theme_bw()

gg1
ggsave(gg1,
       file="lmAbsPropDifL1.pdf")

## Another view:
## Look at years since change and precision.
## Which asks if it is the case that
## presentation is more stable when information is
## less precise
## This is a zero inflated negative binomial since
## have 0s from no changes & also zeros from year after deltas


library("sandwich")
library(pscl)

## need to remove NA for clustered errors:

cols <- c("counter", "mean.loc.prec",
          "mean.clarity","mean.time.prec",
          "MSF", "groupID")

dat2 <- na.omit(dat[,cols]) ## 2298 x 6

mod3 <- zeroinfl(counter ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
                 data=dat2,
                 dist="negbin")

summary(mod3)

mod3ci <- coeftest(mod3,
                   vcov = vcovCL(mod3,
                                 cluster = dat2$groupID))


df <- broom::tidy(mod3ci) ## then exoponentiate the coef:

ci95 <- confint(mod3ci, level=0.95) %>%
    data.frame() %>%
    rename("conf.low95" = "X2.5..",
           "conf.high95" = "X97.5..")

ci90 <- confint(mod3ci, level=0.9) %>%
  data.frame() %>%
  rename("conf.low90" = "X5..",
         "conf.high90" = "X95..")

results <- bind_cols(df,
                     ci95,
                     ci90) %>%
    rename(Variable = term,
           Coefficient = estimate,
           SE = std.error) %>%
           filter(Variable != "(Intercept)")

results

gg2 <- ggplot(results,
       aes(x = Variable, y = Coefficient)) +
        geom_hline(yintercept = 0,
                   colour = gray(1/2), lty = 2) +
        geom_point(aes(x = Variable,
                    y = Coefficient)) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low90,
                     ymax = conf.high90),
                   lwd = 1) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low95,
                     ymax = conf.high95),
                   lwd = 1/2) +
    ggtitle("Outcome: Years Since Change Period",
            subtitle="S.E. Clustered by Group ID; Model Neg Bin") +
    coord_flip() +
    theme_bw()

gg2
ggsave(gg2,
       file="negBinomMedia.pdf")


## reintroduce the haddelta measure:

##Which groups had a delta of at least |1|

dat$haddelta1 <- 0
dat$haddelta1 <- ifelse(dat$groupID
                        %in% haddelta1, 1, 0)

table(dat$haddelta1) ## 625 no; 2030 yes



## Any connection between having change at all
## and the precision variables?
## Result: none at all (SE grouped at groupID)
table(dat$haddelta1) ## 625 no; 2030 yes
 
mod4 <- plm(haddelta1 ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
            model=c("random"),## "random effects"
            effects="year",
            index=c("groupID"), #by group
            data=dat)

summary(mod4)

df <- tidy(mod4)
ci95 <- confint(mod4, level=0.95) %>%
  data.frame() %>%
  rename("conf.low95" = "X2.5..",
         "conf.high95" = "X97.5..")

ci90 <- confint(mod4, level=0.9) %>%
  data.frame() %>%
  rename("conf.low90" = "X5..",
         "conf.high90" = "X95..")

results <- bind_cols(df,
                     ci95,
                     ci90) %>%
    rename(Variable = term,
           Coefficient = estimate,
           SE = std.error) %>%
           filter(Variable != "(Intercept)")

results <- results %>%
     mutate_if(is.numeric, ~round(., 1))

gg3 <- ggplot(results,
       aes(x = Variable, y = Coefficient)) +
        geom_hline(yintercept = 0,
                   colour = gray(1/2), lty = 2) +
        geom_point(aes(x = Variable,
                    y = Coefficient)) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low90,
                     ymax = conf.high90),
                   lwd = 1) +
        geom_linerange(aes(x = Variable,
                     ymin = conf.low95,
                     ymax = conf.high95),
                   lwd = 1/2) +
        ggtitle("Outcome: Had Change Year At All") +
    coord_flip() +
    theme_bw()

gg3

## more precision: slightly more llikelihood to have found a change
## more press freedom; more likely
## average clarity in reporting no connection
##

