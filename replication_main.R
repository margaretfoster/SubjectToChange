## Master Replication Script for:
## Foster, Subject to Change
## 2023

rm(list=ls()) 

library(renv)

renv::init() ## Version control;
## The important version to be aware of is that
## quanteda had a major change between versions 3 and 4
## The results replicate with quanteda V3, which was how it was developed


## Part One: Data Processing
source("./01measurementFullDataTinyThreshRep.R")
## Approximately 35 minutes of runtime on 32G Apple M1 2021 Macbook Pro

## Takes: 
## Helper scripts:
## 00articleModelingRep.R, 00dominantFramingRep.R
## previously-downloaded UCDP data:
## ged211.csv; ucdp-prio-acd-211.csv; ucdp-actor-211.xlsx
## Returns: 01measurementScaleUpInProgressTiny55.Rdata

##%%%%%%%%%%%%%%%%%%%%%%%%%%
## Part Two: Estimate Change
##%%%%%%%%%%%%%%%%%%%%%%%%%%

source("./02tinyThreshTransformVarRep.R")
## Runtime: Approximately 5 minutes on 32G Apple M1 2021 Macbook Pro

## Takes: 01measurementScaleUpInProgressTiny55.Rdata
## produces: 02dfyearsumAndRelatedTinyUpdate.Rdata

source("./03analysisFullDataTinyThreshRep.R")
## Takes: 02dfyearsumAndRelatedTinyUpdate.Rdata
## Produces: dfyearsumAndRelatedTinyUpdate.Rdata

source("./analysisGroupPlotsRep.R") 
## Takes: df.yearsumAndRelatedTinyUpdate.Rdata
## Produces: Illustrative plots showing article patterns
## but the versions in the manuscript were resized for aesthetics

## Robustness:
source("./03ClocationPrecisionRep.R")
## Takes:
## 02dfyearsumAndRelatedTinyUpdate.Rdata
## MSFS-estimates_full-3x2000.csv #(data from Solis Waggoner)

## Produces:
## Appendix Figures 10(1) and 10(2), lmAbsPropDifL1.pdf and negBinomMedia.pdf)
## Part Three: Replication

source("./04nsPrepTerminationRep.R")
## Takes: 
## - Nilsson & Svensson replication data (Termination-data-ISQ.dta)
## - 02dfyearsumAndRelatedTinyUpdate.Rdata
## - translate_actor.csv
## Produces:
## - Figures 4 & 5, Appendix Figure 10
## -  terminationplus.dta

source("04nsPrepRecurrance.R")

## Replication (Stata)
## script: 05replication-Termination-analysis-ISQ.do
## script: 05Replication-Recurrence-analysis.do

## Is the relationship between change and termination
## due to a few groups with sparse articles?

## Derive data for 9 conditions:
## minimum 1, 5, 10 articles for
## 100%, 90%, 75% of group-years

source("RobustnessPanel.R") 
## NB: takes about 4 hours to run

## A few descriptive figures: 
source("06ThresholdPanelDescriptives.R")

## Analysis (via Stata because of the replication)
## 06dRobustnessTerminationComparision_1_75.do
## 06dRobustnessTerminationComparision_1_9.do
## 06dRobustnessTerminationComparision_1_1.do
## 06dRobustnessTerminationComparision_5_75.do
## 06dRobustnessTerminationComparision_5_9.do
## 06dRobustnessTerminationComparision_5_1.do
## 06dRobustnessTerminationComparision_10_75.do
## 06dRobustnessTerminationComparision_10_9.do
## 06dRobustnessTerminationComparision_10_1.do

## Appendix Section 5: Comparison of Topic Configurations

## ONLF and PKK Searck K Plots

source("checkAlternateSpecTiny.R")
## calls: articleModelingAlt.R
## Takes the base data, runs STM's searchK() 
## Saves searchK plots
## NB: Several hours of runtime


## Appendix Figures PKK and AQAL Alternative K
source("implementAltPKKAQAP.R") 
## depends on: articleModelingAlt.R
## Takes: 
