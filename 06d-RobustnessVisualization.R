## Load the prepped-for-Stata data
## Draft of the comparison using R
## Won't exactly replicate b/c different programs. 
## But what we care about is similarity across data configurations

library(haven)
library(survival)

tmp <- read_dta("./data/terminationplus_5_1.dta")

colnames(tmp)

##stset end_of_segment, ## variable that specifies time of failure
##id(dyadid) ## index
##origin(time first_year_of_con) ## become "at risk" in the first year of conflict
##enter(time start_of_segment) ## variables enter at start_of_segment variable
##failure(term==1) ## failure variable  is when term == t
##exit(time .) ## subjects exit the "study" at . (periods are missing, so no exit variable?)
##
##stset time, failure(fail)
## stcox var1 var2

## declare controls:
controls1 <- c("territory", "strongstart", "oilstart", 
               "youthstartap", "muslimajstart")

vars <- paste(controls1, collapse = " + ")

##global X2 _yrs _yrs_sq _yrs_cu## need to convert these

tmp[1:10,]$end_of_segment-tmp[1:10,]$start_of_segment

coxph(Surv(start_of_segment, end_of_segment ~ as.formula(vars), term==1, 
           ties=c("breslow"), id="dyadid", data=tmp ) 
      
      ~ var1 + var2)
