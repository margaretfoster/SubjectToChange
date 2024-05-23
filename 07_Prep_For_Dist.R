
rm(list=ls())

## This script converts the R-native data structure into a CSV format for distribution 
## Load the data object that has:
## df.basedata -- UCDP GED base data with an additional column for the group-level topic proportion 
## associated with the entry. 
## NOTE that I am no saving this separately, because it is effectively the UCDP data.

## df.yearsum
changes <- load("./02dfyearsumAndRelatedTinyUpdate.Rdata")

## Pull group name out of the df.basedata:
## (Not pulling country, because some groups are multi-country)

name_key = unique(df.basedata[,c("side_b_dset_id","side_b")])

## pull region:
## some groups are in more than one region, so storing separately:
region_key = unique(df.basedata[,c("side_b_dset_id","region")])

## df.yearsum is a 2180 x 20 dataframe
## that is group-year framing summaries for 260 groups

## Reorder the columns:
colnames(df.yearsum)
length(unique(df.yearsum$groupID))

## Add group names:

group_years <- merge(df.yearsum,
                     name_key,
                     by.x="groupID",
                     by.y="side_b_dset_id",
                     all.x=TRUE)

colnames(group_years)[which(colnames(group_years)=="side_b")] <- "ucdp_name"
colnames(group_years)[which(colnames(group_years)=="groupID")] <- "ucdp_dset_id"

cols_order = c("ucdp_name", "ucdp_dset_id", "year", "modeledarticles",  
               "countT1", "countT2", "propT1",
               "propT2", "propdiff", "propdif.L1" , "propdif.L2",
               "delta1", "delta1.5", "delta2", "delta1_L2",
                "gap25",  "gap50", "counter", "frexWords")

group_years <- group_years[,cols_order]


write.csv(group_years, file="./data/group_years.csv")
write.csv(region_key, file="./data/region_key.csv")
## Save csv (for work in Python)


