Replication documents for Subject to Change (Foster 2023)

Subject to Change takes event-level news articles from the UCDP GED and aggregates them for non-state actors. 

It then runs a Structural Topic Model on the corpus of news snippets associated with each group. The output is aggregated at the group-year and produces an analysis of underlying trends. 

The output summarizes potential years in which third-party observers changed the underlying thematic presentation of the activities of each militant group.

To use this repository:

If you are interested in creating the change data, the workflow is contained in the replication_main.R script.

replication_main.R 
Completely replicates the R-based data generation and analysis.

STC_STATA_Rep_All.do 
Completely replicates the STATA analysis. It calls STC_STATA_Replication.do, which produces the threshold and recurrence replication. It also calls the 9 threshold analysis scripts that run the analysis for various inclusion thresholds.


A second set of files records a recent run of data generation and analysis via logs:

Logs for all of the data generating are available at:

./Logs/RmarkdownLog.pdf
The pdf was produced by RmarkdownLog.Rmd. this includes the replication log for all of the scripts that come from R. An .html log for each component part of the data generating is located in the Logs directory in .html format

A log of the STATA run can be found in:

./Logs/STC_STATA_Log.pdf
./Logs/STC_STATA_Log.smcl


