
clear all

*To run the do-file you need to install the following:
*1. To generate summary statistics:
ssc install unique, replace all

*2. To generate graphs:
ssc install blindschemes, replace all
set scheme plottig, permanently

*3. To generate tables: 
ssc install outreg
ssc install outreg2

* Latex code
 ssc install estout, replace

* Coefplot
ssc install  coefplot  

****************************************************************************************************

* load data:
* Threshold 1 article/year for all years (basically same):
use "./data/terminationplus_10_1.dta"

sort dyadid year

************************************************
* STSET FOR SURVIVAL ANALYSIS
************************************************

stset end_of_segment, id(dyadid) origin(time first_year_of_con) enter(time start_of_segment) failure(term==1) exit(time .)


************************************************
* DESCRIPTIVE STATISTICS
************************************************
************************************************
* CREATE LABELS
************************************************

label variable term "Termination"
label variable islamist "Islamist claim"
label variable counter "Years From Change"
label variable delta1 "Change Year, Delta 1"
label variable delta1 "Change Year, Delta 1"
label variable delta1_L2 "Change in Prev 2 years"
label variable numchanges "Change Frequency"
label variable haddelta1 "Had Change |1|"
label variable haddelta15 "Had Change |1.5|"
label variable haddelta2 "Had Change |2|"
label variable territory "Territory"
label variable duration "Duration"
label variable intensitylevel "War"
label variable number_group "Number of groups"
label variable strongstart "Strong rebels"
label variable anostart "Anocracy"
label variable lngdppcstart "GDP per capita"
label variable lnpopstart "Population"
label variable muslimajstart "Muslim majority"
label variable oilstart "Oil"
label variable youthstartap "Youth bulge/adult pop."
label variable anocracy "Anocracy over time"
label variable lngdppc "GDP per capita over time"
label variable lnpop "Population over time"
label variable foreignfighter "Foreign fighters"
label variable govmilsupport "Government support"
label variable leftist "Leftist"
label variable nonislamistrel "Non-Islamist religious claims"
label variable muslimid "Muslim identity"
label variable secsup_govgov "Government secondary support"
label variable rebextpartdummy "Rebel support"



************************************************
* Summary of new variables
************************************************

su delta1 delta1_L2 numchanges counter haddelta1 haddelta15 haddelta2


************************************************
* CONTROL VARIABELS
************************************************

global X1 territory strongstart oilstart youthstartap muslimajstart
global X2 _yrs _yrs_sq _yrs_cu

************************************************
* TABLE 1
************************************************

*Model 1- Replication*     

stcox islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store RepModel

estimates store RepModel
outreg using termination-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) replace tex frag ctitle(Repl. Model)

//capture drop sch* sca*
//stcox islamist $X1, cluster(dyadid) strata(order) scaledsch(sca*) schoenfeld(sch*) nohr
//stphtest, rank detail

*Model 2.A- Binary for Change* 
    
stcox haddelta1 islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store SmallChange 

outreg using termination-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(Binary Change)

*Model 2.B- haddelta15*     

stcox haddelta15 islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store MedChange 

outreg using termination-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(Med Delta)

*Model 2.C- haddelta2*     

stcox haddelta2 islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store HighChange 

outreg using termination-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(High Delta) title(Model Comparisons)

//Model 4: Number of changes

stcox numchanges islamist $X1, cluster(dyadid) strata(order)  nolog 
estimates store NumChanges

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(NumChanges) title(Model Comparisons)
 
// Model with other covariates

stcox haddelta1 anocracy secsup_govgov rebextpartdummy govmilsupport islamist leftist $X1, cluster(dyadid) strata(order)  nolog 

estimates store Model2A

outreg, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(Model2A) title(Model Comparisons)

stcox numchanges anocracy secsup_govgov rebextpartdummy govmilsupport islamist leftist $X1, cluster(dyadid) strata(order)  nolog 

estimates store Model2B

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(NumChanges) title(Model Comparisons)


********************************************
** Results Plots
********************************************


* d/l lean2 plot for bw graph *
net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)
set scheme lean2 

** Figure A[X] of Document ** 

coefplot(RepModel, label(Replication))(SmallChange, label(Low Change))(MedChange, label(Med. Change))(HighChange, label(High Change)), drop(_cons) xline(0) graphregion(color(white)) bgcolor(white) title("N = 10, T= 100")
graph export thresh_10_1.pdf, as(pdf) name("Graph") replace
