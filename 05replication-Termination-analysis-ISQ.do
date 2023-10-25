clear all
****************************************************************************************************
		
Replication file 1 out of 3 for:
"The Intractability of Islamist Insurgencies: Islamist Rebels and the Recurrence of Civil War"
Desirée Nilsson & Isak Svensson
International Studies Quarterly
Last updated: 20 April 2021
Note that dyadep 18502 has been replaced with 28502 due to an error in original termination data

***************************************************************************************************/


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

* set working directory:
* MJF: Set to Replication directory; I'm using my own throughtout

cd "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\STC_Replication"

* load data:
use "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\STC_Replication\data\terminationplus.dta"

sort dyadid year

************************************************
* STSET FOR SURVIVAL ANALYSIS
************************************************

stset end_of_segment, id(dyadid) origin(time first_year_of_con) enter(time start_of_segment) failure(term==1) exit(time .)


************************************************
*

 DESCRIPTIVE STATISTICS
************************************************


*Islamist conflicts and termination
unique dyadid if islamist==1 | islamist==0 
*229 in MJF data subset
unique dyadid if islamist==1 
*57 in MJF data subset
unique dyadid if islamist==0
*242 in MJF subset
unique dyadid if term==1 
*280 in MJF subset


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

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(Med Delta)

*Model 2.C- haddelta2*     

stcox haddelta2 islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store HighChange 

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(High Delta) title(Model Comparisons)

//Model 3: Incorporating time structure

*Model 3.A: "Counter" that resets after a change: 

stcox counter $X1 if haddelta1==1 , cluster(dyadid) strata(order)  nolog 

estimates store YearsSinceLast

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(YearsSinceLast) title(Model Comparisons)

* Model 3.B: Year-embedded in data structure:
* Change in previous year, Not sig


stcox delta1 islamist $X1, cluster(dyadid) strata(order)  nolog 
estimates store YearofChange

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(YearofChange) title(Model Comparisons)

* Model 3.C: With Two Year Lag:
* Change in framing in previous two years. Not sig. Implies that framing changes are not isn't happening right before big changes in conflict dynamics.

stcox delta1_L2 islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store YearofChangeL2

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(YearofChangeL2) title(Model Comparisons)


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


* Compare Results:

* d/l lean2 plot for bw graph *
net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)
set scheme lean2 

** Figure 6 of Document ** 
coefplot(RepModel, label(Replication))(SmallChange, label(Low Change))(MedChange, label(Med. Change))(HighChange, label(High Change)), drop(_cons) xline(0) graphregion(color(white)) bgcolor(white)
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\TerminationCoefPlotUp.pdf", as(pdf) name("Graph")

*Appendix Figure 11*
coefplot(SmallChange, label(Low Change))(YearofChangeL2, label(Two-Year Window))(YearsSinceLast, label(Years Since Change))(NumChanges, label(Change Frequency)), drop(_cons) xline(0)
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\TerminationCoefPlotExtUp.pdf", as(pdf) name("Graph")

coefplot(Model2A, label(Binary Condition))(Model2B, label(Change Frequency)), drop(_cons) xline(0) graphregion(color(white)) bgcolor(white)
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\TerminationExtraModelsUp.pdf", as(pdf) name("Graph")

************************************************
* FIGURE 1
************************************************

*Kaplan meier graph with confidence intervals, strata not possible
sts graph, legend (cols(1)) by(islamist) ci 
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\KM_islamist.pdf", as(pdf) name("Graph")

sts graph, legend (cols(1)) by(haddelta1) ci 
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\KM_delta1.pdf", as(pdf) name("Graph")

sts graph, legend (cols(1)) by(haddelta15) ci 
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\KM_delta15.pdf", as(pdf) name("Graph")

sts graph, legend (cols(1)) by(haddelta2) ci 
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\KM_delta2.pdf", as(pdf) name("Graph")

sts graph, legend (cols(1)) by(numchanges) ci 
graph export "\\Client\H$\Dropbox (Personal)\TransformationEmpiricalModels\ReplicationsTransformationVar\NilssonSvensson2021\RepPlotsAndGraphs\KM_numchanges.pdf", as(pdf) name("Graph")
