Replication documents for Subject to Change (Foster 2024)

Subject to Change takes event-level news articles from the UCDP GED and aggregates them for non-state actors.  It then runs a Structural Topic Model on the corpus of news snippets associated with each group. The output is aggregated by group year and analyses underlying trends. 

The output summarizes potential years in which third-party observers changed the underlying thematic presentation of each militant group's activities.

This repository replicates the analysis needed to produce the data and results in the paper.
There are several ways to use the repository:

(1) A full replication is handled by STC_R_Replication_Log.Rmd (Master Log)
 
This Markdown file calls each of the scripts in sequence to take in the UCDP GED data and produce the change measurement. It uses the GroundhogR dependency management framework to ensure library consistency. It concludes with some light directory cleanup. 

Each R script called by STC_R_Replication_Log.Rmd produces an html log, housed in the Logs/ subdirectory.

STC_R_Replication_Log.Rmd takes 6 - 8 hours to run on an Apple M1 Pro laptop.

(2) STATA analysis

The application of the measurement is done via a replication of a STATA script, STC_STATA_Replication.do

An evaluation of the effects of changing inclusion thresholds is in STC_STATA_Rep_All.do.

To run them, you can open STATA and call:

do STC_STATA_Replication.do
do STC_STATA_Rep_All.do

The log of these scripts is STC_STATA_Log.pdf.

Note that a fresh replication needs to run the R scripts first, because the R scripts produce the original data. 

(3) Compilation of tables and figures

The logs themselves are long and complex (with a lot of printouts), so for convenience I have aggregated all of the tables and figures into a single printout.

Replication_Figures.Rmd, which produces a pdf (Replication_Figures.pdf) of the figures and tables that are featured in the Manuscript and appendix.

(4) Code Run Logs

Logs for the entire project are available at:

(3a) R-based analysis

./STC_R_Replication_Log.html
./Logs/[R_file_name_here].html 


(3b) STATA-based analysis

A log of the STATA run can be found in:

./Logs/STC_STATA_Log.pdf
./Logs/STC_STATA_Log.smcl





