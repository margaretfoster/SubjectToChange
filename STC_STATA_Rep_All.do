***************************************************************************************************
		
* Replication file for:
* Foster, 'Subject to Change: Quantifying Transformation in Armed Conflict Actors'
* This file compares inclusion criteria for minimum numbers of articles per group/year
* Includes a replication of
* 'The Intractability of Islamist Insurgencies: Islamist Rebels and the Recurrence of Civil War"
* Desiree Nilsson & Isak Svensson, International Studies Quarterly
***************************************************************************************************/

* Replication of Termination results (Manuscript Fig 6, Appendix Fig 11:
* Replication of Recurrence results (in Appendix Fig 14, 15)

do STC_STATA_Replication.do

* Appendix Figure 8:
* Comparision of threshold for document sparsity


* Threshold 1 word for .75, .90, 1 percent of years

*saves: thresh_1_75.pdf
do 06dRobustnessTerminationComparision_1_75.do 
* saves: thresh_1_90.pdf
do 06dRobustnessTerminationComparision_1_90.do
*saves: thresh_1_1.pdf
do 06dRobustnessTerminationComparision_1_1.do

* Threshold: 5 words for.75, .90, 1 percent of years

*saves: thresh_5_75.pdf
do 06dRobustnessTerminationComparision_5_75.do 
* saves: thresh_5_90.pdf
do 06dRobustnessTerminationComparision_5_90.do
*saves: thresh_5_1.pdf
do 06dRobustnessTerminationComparision_5_1.do

** Threshold: 10 words for.75, .90, 1 percent of years

*saves: thresh_10_75.pdf
do 06dRobustnessTerminationComparision_10_75.do 
* saves: thresh_10_90.pdf
do 06dRobustnessTerminationComparision_10_90.do
*saves: thresh_10_1.pdf
do 06dRobustnessTerminationComparision_10_1.do
