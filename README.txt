Replication documents for Subject to Change (Foster 2023)

Subject to Change takes event-level news articles from the UCDP GED and aggregates them for non-state actors. 

It then runs a Structural Topic Model on the corpus of news snippets associated with each group. The output is aggregated at the group-year and produces an analysis of underlying trends. 

The output summarizes potential years in which third-party observers changed the underlying thematic presentation of the activities of each militant group.

To use this repository:

If you are interested in creating the change data, the workflow is contained in the replication_main.R script.

replication_main.R (and the associated R Studio Project) lists the scripts and steps to completely replicate the documents.

If you are looking for the code that produces the tables and figures, then the code and replication logs are available in:

ReplicationRLog.html (produced by ReplicationRLog.Rmd)
STATAReplicationLog.html (produced by StataReplicationLog.txt)
ThresholdReplicationLog.html (produced by ThresholdReplicationLog.txt)
