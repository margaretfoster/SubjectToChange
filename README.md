# Subject to Change (Foster 2024)

Welcome! This repository contains code that uses machine learning to estimate "change" years for organized non-state violent conflict actors whose activities are tracked by the Uppsala Conflict Data Program.

You can find the slides for a high-level presentation of the project [here.](https://github.com/margaretfoster/slides/blob/main/Foster-TargetRWETalk_2024.pdf)
A link to the peer-reviewed manuscript is forthcoming.
## Project Overview

The output summarizes potential years in which third-party observers changed the underlying thematic presentation of non-state miltiant groups tracked in the UCDP's [Georeferenced Events Dataset v 21.1](https://ucdp.uu.se/downloads/olddw.html)).

A single group-level example looks like: 

![Activity modeling for Abu Sayyaf](https://github.com/margaretfoster/SubjectToChange/blob/master/images/annotedAbuSayyaf.png).

The model plots the Topic 1 vs Topic 2 assignment of news articles associated with ASG for each year and summaries the relative distribution with the green dotted line. Operationalizing "change" as a year with the majority topic changing from one year to the next, it estimates that the Philippines- based jihadi insurgency underwent "change" periods in approximately 2001, 2003, 2007/2008, and 2019.

One might use the measure to improve predictions of conflict dynamics, such as ![in this model](https://github.com/margaretfoster/SubjectToChange/blob/master/images/TerminationCoefPlotExtUp.pdf), which presents coefficient estimates of a Cox proportional hazards survival model indicating that non-state actors with any change years tend to be associated with longer conflicts, as do groups with more "change" periods.

## Results Summary

This plot produces a high-level visualization of the output, via an aggegation of the number of group "changes" assigned to regions of operation, by year:

![yearly count](https://github.com/margaretfoster/SubjectToChange/blob/master/images/fig_regional_count.png)

[The STC Visualizer dashboard](https://stc-visualizer.onrender.com/) allows interested users to drill down into the trajectory for a specific group. Selecting a region in the first drop-down will automatically populate the second drop down with the UCDP groups associated with that region (and which had sufficient articles to model).

The processed change variable can be downloaded in the [Data subdirectory](./data/group_years_regions.csv/). It includes the UCDP Name for each modeled non-state actor, the year, the proportion of documents assigned to Topic One, proportion assigned to Topic Two, and, where possible, the FREX words associated with the dominant topic.  The data is more fully characterized in the [summary Jupyter Notebook](https://github.com/margaretfoster/SubjectToChange/blob/master/07_Introductory_Viz.ipynb).

## Code Overview:

The code in this repository:

- Takes event-level news articles from the UCDP GED and aggregates them for non-state actors
- Runs a Structural Topic Model on the corpus of news snippets associated with each group.
- Aggregates the topic model output by group year
- Models the group-level trends to identify points where third-party writers change how they write about the activities of actors
- Creates a dataset that operationalizes the above into a:
    - binary "actor change" variable
    - time-series actor-country-year change records

- Inserts the variable in a previous study about the effects of uncertainty on the length of substate conflict, more directly capturing the dynamic of interest and increasing the precision of the study's estimate

## Data

The data used for the analysis (along with copies of code, plots, and full run logs) can be found in the [Harvard Dataverse repository associated with this project](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910%2FDVN%2F1HNSZR). The .zip file includes the underlying event data, media precision, and study extension data as well as the intermediate .Rdata files produced from the R code.

## Using the repository

This repository replicates the analysis needed to produce the data and results in the paper.

It also features Jupyter Notebook scripts that allow potential users to interact with the new "change" variable. 

### Replication
The workflow to do a full replication is:

1) Knit STC_R_Replication_Log.Rmd
This Markdown file calls each of the scripts in sequence to take in the UCDP GED data and produce the change measurement. It uses the GroundhogR dependency management framework to ensure library consistency. It concludes with some light directory cleanup.  Each R script called by STC_R_Replication_Log.Rmd produces an html log, housed in the Logs/ subdirectory. (STC_R_Replication_Log.Rmd takes 6 - 8 hours to run on an Apple M1 Pro laptop.)
  
2) Open STATA and call:
do STC_STATA_Replication.do
do STC_STATA_Rep_All.do

The application of the measurement is done via a replication of a STATA script, STC_STATA_Replication.do
An evaluation of the effects of changing inclusion thresholds is in STC_STATA_Rep_All.do. The log of these scripts is STC_STATA_Log.pdf.

3) Knit Replication_Figures.Rmd

The logs themselves are long and complex (with a lot of printouts), so for convenience, I have aggregated all of the tables and figures into a single printout.
Replication_Figures.Rmd, which produces a pdf (Replication_Figures.pdf) of the figures and tables that are featured in the Manuscript and appendix.

### Logs
Logs for the entire project are available at:

(3a) R-based analysis

./STC_R_Replication_Log.html
./Logs/[R_file_name_here].html 

(3b) STATA-based analysis

A log of the STATA run can be found in:

./Logs/STC_STATA_Log.pdf
./Logs/STC_STATA_Log.smcl



 
