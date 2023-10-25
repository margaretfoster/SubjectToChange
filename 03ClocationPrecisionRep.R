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
## 366 = 
## 325 = 
## 209 = 
## 180 = 


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

##print(df, nrow=100)

## Optional: check outcome via a known case
## AQIM:
##print(df[which(df$groupID==539),], n=38)


df.precision <- df.basedata %>%
    group_by(groupID, year, country_id) %>%
     summarise(
         mean.time.prec= mean(date_prec),
         mean.loc.prec = mean(where_prec),
         mean.clarity = mean(event_clarity)
         )
    
dim(df.precision) ## 2655 x 6

head(df.precision)

## add in the country code:
## via ccodes library

library(countrycode)

## bring in ccode:

##df.precision$iso3c <- countrycode(df.precision$country_id,
##                      origin = "gwn",
##                      destination = "iso3c")

df.precision$cow <- countrycode(df.precision$country_id,
                                origin = "gwn",
                                destination = "cowc")

## Missing: 345 Serbia (Yugoslavia) [Not in media dat]
##  678 Yemen (North Yemen)

##df.precision[which(df.precision$country_id==345),
##             "iso3c"] <- "YEM"
## Will not have Yugoslavia b/c not matched with a country in our data

## Data source for journalism access
## Goddes and Cary (2021 and 2017) dataset on journalist murders
## They also us Freedom House Measures on media restrictions
## (Presumably in their replication data)
## & V-Dem's  Freedom of Expression and Alternative Sources of Inf## ormation Index 

summary(df.basedata$year) ## 1989-2020

##meddat <- read_csv(
##    paste0("./data/",
##           "analysis-data-fullsample.csv"))


meddat <- read_csv(
    paste0("./data/",
    "MSFS-estimates_full-3x2000.csv"))


df.media <- df.precision %>%
    left_join(meddat,
              by = c("year"= "year",
                  "cow" = "abbr"))

dim(df.media) ## 2655 x12

## Group-country-year average of
## UCDP precision & FH media freedom score
summary(df.media$MSF)

## range is [0,1],
## min is 0.0127; mean is .5323; max .9972
## 357

nas <- df.media[is.na(df.media$MSF)==TRUE,]

table(nas$year) ## vast majority 2018, 2019, 2020


## which are the missing years covered in their data?:
##print(nas[which(nas$year<2018),
##          c("year", "country_id",
##             "country",
##            "cow", "ccode")],n=41)

## Bring group-year change info into the
## group-country-year df.precision + media info

df.precision2 <- df.media %>%
    left_join(df.yearsum,
              by=c("groupID"= "groupID",
                  "year"= "year"))


## linear regression year-on-year change
## by event precision

## expectation: less precision, less change
## group clustered SE

## Group indices so that we can feed two to plm
## for the three-way index:

dat <-df.precision2 %>%
    group_by(groupID, country_id) %>%
    mutate(gcid = cur_group_id()) ## creates a grouping id for group-country

## direct three-way index for group-country-year
## for the zero inflated neg. binomial

dat <-dat %>%
    group_by(groupID, country_id, year) %>%
    mutate(gcyid = cur_group_id()) ## creates a grouping id for group-country

dim(dat)

## fixed effects by group
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

library(broom)

df <- tidy(mod1)
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

results

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

gg1

plot(density(abs(dat$propdif.L1)))

## random effects by group
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

mod2A<- plm(propdif.L1 ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
            model=c("random"),## random effects
            effect="time", ## time fixed effects
            index=c("year","gcid"),
            data=dat)

summary(mod2A) 

## SE Grouped just at group level:
## b/c group-country-year is the unit of analysis
## so it's weird to group at that:

mod2B<- plm(propdif.L1 ~ mean.loc.prec +
            mean.clarity + mean.time.prec +  MSF,
            model=c("random"),## random effects
            effect="time", ## time fixed effects
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

results

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

gg1 ##Manuscript Fig Appendix 10(1)
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
                   vcov = vcovCL(mod3, cluster = dat2$groupID))

df <- tidy(mod3ci) ## then exoponentiate the coef:

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

gg2 ## Manuscript fig Appendix A10(2)
ggsave(gg2,
       file="negBinomMedia.pdf")

print("finished making figures for location precision questions")