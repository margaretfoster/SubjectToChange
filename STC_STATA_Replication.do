
****************************************************************************************************
		
*Replication for 
*"Subject to Change: Quantifying Transformation in Armed Conflict Actors At Scale Using Text"
*Margaret J. Foster
*Last updated: December 5, 2023

*The project extends Desirée Nilsson & Isak Svensson's "The Intractability of Islamist Insurgencies:
* Islamist Rebels and the Recurrence of Civil War"
*International Studies Quarterly

*As such, the analysis closely follows their replication scripts
*(Note that dyadep 18502 has been replaced with 28502 due to an error in original termination data)

***************************************************************************************************/

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
* set working directory:
* MJF: Set to Replication directory; I'm using my own throughtout

* load data:
use  "./data/terminationplus.dta"

sort dyadid year

************************************************
* STSET FOR SURVIVAL ANALYSIS
************************************************

stset end_of_segment, id(dyadid) origin(time first_year_of_con) enter(time start_of_segment) failure(term==1) exit(time .)

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
* Manuscript Figure 6 and Appendix Figure 11
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

* Model 3.C: With Two Year Lag:
* Change in framing in previous two years. Implies that framing changes are not isn't happening right before big changes in conflict dynamics.

stcox delta1_L2 islamist $X1, cluster(dyadid) strata(order)  nolog

estimates store YearofChangeL2

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(YearofChangeL2) title(Model Comparisons)

*Model 3.A: "Counter" that resets after a change: 

stcox counter $X1 if haddelta1==1 , cluster(dyadid) strata(order)  nolog 

estimates store YearsSinceLast

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(YearsSinceLast) title(Model Comparisons)


//Model 4: Number of changes

stcox numchanges islamist $X1, cluster(dyadid) strata(order)  nolog 
estimates store NumChanges

outreg using termination-t1.doc, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(NumChanges) title(Model Comparisons)

* Compare Results:

* d/l lean2 plot for bw graph *
net install gr0002_3, from(http://www.stata-journal.com/software/sj4-3)
set scheme lean2 

** Figure 6 of Document ** 
coefplot(RepModel, label(Replication))(SmallChange, label(Low Change))(MedChange, label(Med. Change))(HighChange, label(High Change)), drop(_cons) xline(0) graphregion(color(white)) bgcolor(white)
graph export "TerminationCoefPlotUp.pdf", as(pdf) name("Graph") replace


*Appendix Figure 11*
coefplot(SmallChange, label(Low Change))(YearofChangeL2, label(Two-Year Window))(YearsSinceLast, label(Years Since Change))(NumChanges, label(Change Frequency)), drop(_cons) xline(0)
graph export "TerminationCoefPlotExtUp.pdf", as(pdf) name("Graph") replace


*******************************************
*******************************************
* Replication of Recurrance Models
* Manuscript Appendix Figures 14 and 15
*******************************************


clear all 
ssc install unique
ssc install blindschemes, replace all
ssc install outreg
ssc install outreg2


*load data:

use "recurrenceplus.dta"

sort dyadid year


************************************************
* STSET FOR SURVIVAL ANALYSIS RECURRENCE as DV
************************************************

stset end_of_segment, id(dyadid) origin(time first_year_of_peace) enter(time start_of_segment) failure(firstrecur==1) exit(time .)

************************************************
* STSET FOR SURVIVAL ANALYSIS NEW-RECURRENCE as DV
************************************************

stset end_of_segment, id(dyadid) origin(time first_year_of_peace) enter(time start_of_segment) failure(newrecur1==1) exit(time .)

************************************************
* CREATE LABELS
************************************************

label variable firstrecur "Recurrence"
label variable newrecur1 "Recurrence-new"
label variable islamist "Islamist claim"
label variable delta1 "Change Year"
label variable delta1_L2 "Change in Two Years"
label variable haddelta1 "Had Change |1|" 
label variable haddelta15 "Had Change |1.5|"
label variable haddelta2 "Had Change |2|"
label variable ambig25 "Ambiguity |.25|" 
label variable ambig50 "Ambiguity |.5|"
label variable territory "Territory"
label variable duration "Duration"
label variable intensitylevel "War"
label variable number_group "Number of groups"
label variable transstart "Transnational constituency"
label variable forinvstart "Foreign involvement"
label variable strongstart "Strong rebels"
label variable pa "Peace agreement"
label variable ca "Ceasefire agreement"
label variable lowcease "Low activity"

label variable govv "Government victory"
label variable rebv "Rebel victory"
label variable pko "Peacekeeping presence"
label variable anostart "Anocracy"
label variable lngdppcstart "GDP per capita"
label variable lnpopstart "Population"
label variable muslimajstart "Muslim majority"
label variable oilstart "Oil"
label variable youthstartap "Youth bulge/adult pop."
label variable muslimid "Muslim identity"

label variable foreignfighter "Foreign fighters"
label variable govmilsupport "Government support"
label variable leftist "Leftist"
label variable nonislamistrel "Non-Islamist religious claims"
label variable muslimid "Muslim identity"

label variable anocracy "Anocracy over time"
label variable lngdppc "GDP pc over time"
label variable lnpop "Population over time"
label variable secsup_govgov "Government secondary support"
label variable rebext "Rebel support"


************************************************
* CONTROL VARIABLES
************************************************

global X1 territory strongstart oilstart youthstartap muslimajstart
global X2 _yrs _yrs_sq _yrs_cu


************************************************
* Appendix Figure 15
************************************************

//Model 1//

stset end_of_segment, id(dyadid) origin(time first_year_of_peace) enter(time start_of_segment) failure(firstrecur==1) exit(time .)

*Model 2*

** Base model specification:
stcox islamist $X1, cluster(dyadid) strata(order)  nolog
outreg using recurrence-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) replace tex frag ctitle(Base Model)

** Adding delta1 measure:
stcox haddelta1 islamist $X1, cluster(dyadid) strata(order)  nolog
outreg using recurrence-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(Small Change)

capture drop sch* sca*
stcox haddelta1 islamist $X1, cluster(dyadid) strata(order) scaledsch(sca*) schoenfeld(sch*) nohr
stphtest, rank detail

** Adding delta1.5 measure:
stcox haddelta15 islamist $X1, cluster(dyadid) strata(order)  nolog
outreg using recurrence-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(Medium Change)

** Adding delta2 measure:
stcox haddelta2 islamist $X1, cluster(dyadid) strata(order)  nolog
outreg using recurrence-t1.tex, se var hr starlevels(10 5 1) sigsymbols(+,*,**) note (Robust standard errors in parentheses clustered on dyad.) merge tex frag ctitle(High Change)
