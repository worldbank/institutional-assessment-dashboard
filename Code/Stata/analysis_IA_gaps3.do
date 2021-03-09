
* created by serena, 2019 dec 06
* updated: 2019 dec 06
* updated: 2020 jan 21
* this version: 2020 june 05

clear all
set maxvar 32000

* set seed
set seed 20191204
set more off

cd "$mainpath"

* paths
global path_data_raw "data_raw"
global path_data_cleaned "data_cleaned"
global path_graphs "graphs2"
global path_dofiles "dofiles"


******************************************************************** 
* DATA IN
*********************************************************
use "$path_data_cleaned/merged_for_residuals.dta", clear


g FCV=.
replace FCV=1 if inlist(country, "Afghanistan", "Burkina Faso", "Cameroon","Central African Republic","Chad","Congo","Eritrea")
replace FCV=1 if inlist(country, "Gambia","Burundi","Guinea-Bissau","Haiti","Venezuela","Zimbabwe")
replace FCV=1 if inlist(country, "Sudan","South Sudan", "Lebanon","Palestine","Niger","Nigeria","Papua New Guinea")
replace FCV=1 if inlist(country, "Liberia", "Yemen","ksv","Myanmar","Mozambique","Mali","Iraq","Laos")
replace FCV=1 if inlist(country, "Congo,Rep.","Libya","Somalia","Syria")



******************************************************************** 
* choose indicators of interest 
******************************************************************** 

qui do "$path_dofiles/choose_indicators.do"



*********************************************************************
* Prepare dataset for distance to frontier and quantiles 
*********************************************************************

* keep only vars of interest
*keep country year lac lac6 oecd structural $vars $vars_control
keep country year lac lac6 oecd structural $vars 



*********************************************************************
* Identify different samples
*********************************************************************
* all
gen sample1 = 1
* oecd, structural, lac6
gen sample2 = 1 if oecd==1 | structural==1 | lac6==1 | country=="Uruguay"
* oecd, structural, all lac 
gen sample3 = 1 if oecd==1 | structural==1 | lac==1 | country=="Uruguay"
* oecd 
gen sample4 = 1 if oecd==1  | country=="Uruguay"



*(deleted standardize all vars (by year) FULL SAMPLE)


* identify worst and best performance in the last 10 years (2009-2019) 
*(in doing business they do it for the last 5 years, but here we do it only for the last 10 because for some indicators we have shorter time series)
* ref: https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf


* calculate distance to frontier using worst and best performance in the FULL SAMPLE, it is just a scaling anyway, it does not matter for the quantiles
**** NB: methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology

*EA: if we do this, we loose the obs, when using just last obs of sample?
* --> drop if year ==2018
	foreach v of varlist barriers_startups - protection_incumbents {
		replace `v'=. if year==2018
	}
break

******************************************************************
* WORST AND BEST PERFORMERS
******************************************************************
*SINCE 2013, FULL SAMPLE
global vars_minmax
foreach v of global vars {
	egen `v'_max = max(`v') if year >= 2013 
	egen `v'_min = min(`v') if year >= 2013 
	global vars_minmax "$vars_minmax `v'_max `v'_min " 
}

/*

*SINCE 2013, SAMPLE 2
global vars_minmax_s2
foreach v of global vars {
	egen `v'_max_s2 = max(`v') if year >= 2013 & sample2==1
	egen `v'_min_s2 = min(`v') if year >= 2013 & sample2==1
	global vars_minmax_s2 "$vars_minmax_s2 `v'_max_s2 `v'_min_s2 " 
}


*/

/* THIS CAN MATTER: EG W/POLITY
. sum e_p_polity e_p_polity_max e_p_polity_max_s2 e_p_polity_min e_p_polity_min_s2

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
  e_p_polity |        801    4.219725     6.19267        -10         10
e_p_polity~x |      1,489          10           0         10         10
e_p_pol~x_s2 |        273          10           0         10         10
e_p_polity~n |      1,489         -10           0        -10        -10
e_p_pol~n_s2 |        273          -4           0         -4         -4
*/


* collapse at country level, keeping the most recent data for each indicator


*********************************************************************
* DATA FROM LAST AVAILABLE YEAR
*********************************************************************
/*

* keep the most recent data for each indicator
foreach v of global vars {
	rename `v' temp
	sort country year
	bys country: gen order = _n if temp!=.
	bys country: egen max = max(order)
	gen `v' = temp if max == order
	drop temp order max
}

*/





*********************************************************************
* COLLAPSING TO CROSS-SECTION
*********************************************************************


* collapse at country level (EA: cleaned a bit)
collapse $vars $vars_minmax oecd lac lac6 structural sample*, by(country)

****DATA OUTPUT: cross-country w/most recent data
*compress
*save "$path_data_cleaned/cross-section_mostrecentdata.dta", replace



* count obs per sample
gen temp = 1
foreach i of numlist 1 2 3 4 {
	egen sample`i'_size = total(temp) if sample`i'==1
}
drop temp

* CALCULATE RANKS (deleted from old version)

*********************************************************************
* CALCULATE DISTANCE TO FRONTIER (DTF) BY VARIABLE
*********************************************************************
* ref: https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf
* calculate distance to frontier using worst and best performance in the full sample (it is just a scaling anyway, it does not matter for the quantiles)
* probably i should review this in order to use the max and min possible achievable values, rather than the ones in the sample. but again, this is just scaling, it does not affect the quantiles
foreach v of global vars {
	gen dtf_`v' = (`v'_min - `v' ) / (`v'_min - `v'_max)
		replace dtf_`v' =  0.01 if dtf_`v'==0 // to have it on the graph, if = 0
}
drop $vars_minmax



* calculate distance to frontier by institutional family 
* (mean of DTF of each indicator)
global dtf_vars_pol
global dtf_vars_leg 
global dtf_vars_transp 
global dtf_vars_publ 
global dtf_vars_social
global dtf_vars_mkt
global dtf_vars_lab
global dtf_vars_fin
global dtf_vars_service_del

*newlist signifies to foreach that the list is composed of new variables. 
* EA: don't know this code well. Ask :)
foreach g of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	foreach v of global `g' {
		global dtf_`g' "${dtf_`g'} dtf_`v'" 
	}
}
break 

foreach g of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	egen dtf_`g' = rowmean(${dtf_`g'})
		replace dtf_`g' =  0.01 if dtf_`g'==0 // to have it on the graph, if = 0
}

* calculate quantiles for different samples - 5 quintiles then grouped in 0-40, 40-60, 60+

* NB: quantile 1 = low; quantile 5 = high
* based on ranking
* NB: note that xtile does not behave well for indicators where there is parity. in those cases, it will not create 5 quantiles, but less depending on the number of ties
* set 1 of problematic variables: e_fh_pr e_fh_cl e_p_polity - for these variables, Uruguay has the max value, so I can simply assign temp == 5 always (missing for other countries)
* v2lgqugen, for sample2, no change: it gives back categories 1, 3 and 4, ok to recreate group low, middle, high, do not change
* v2lgqugen, for sample3, no change: it gives back categories 1, 3, 4 and 5, ok to recreate group low, middle, high, do not change
* v2lgqugen, for sample4, no change: it gives back categories 1 and 4, but if i use 6 quantiles the group definition is going to be the same and uruguay is assigned to high anyway
* contract_manag_score, for sample2, no change: if give back categories 1, 2, 3, 5. uruguay is in 2 anyway, fine
* evaluation_score, for sample2, no change: if give back categories 1, 2, 3, 5. uruguay is in 3 anyway, fine
* evaluation_score, for sample3, no change: if give back categories 1, 2, 3, 5. uruguay is in 3 anyway, fine
* evaluation_score, for sample4: it gives back categories 1, 2, 5, but 2 is the middle one, so reassign 2 as middle
* payments_score, for sample 4: ok
* no problem with vars_leg
* no problem with vars_transp
* no problem with vars_publ
* no problem with vars_part
* no problem with vars_mkt
* no problem with vars_lab
* some problematic vars in vars_fin
foreach v of global vars {
	foreach i of numlist 2 {
		* fixing set 1
		if "`v'"=="e_fh_pr" | "`v'"=="e_fh_cl" | "`v'"=="e_p_polity" {
		gen temp = 5 if country=="Uruguay" 
		}
		if "`v'"!="e_fh_pr" & "`v'"!="e_fh_cl" & "`v'"!="e_p_polity" {
 			xtile temp = `v' if `v'!=. & sample`i'==1, nq(5) // change quantiles to 3 for low, medium and bottom
		}
		if "`v'"=="evaluation_score" & `i'==4 {
			replace temp = 3 if temp == 2 // only category 1, 2 and 5, but 2 is the middle one
		}
		if "`v'"=="efw_free_foreign_curr" & `i'==1 {
			replace temp = 5 if `v'==10 // only category 1 and 3, to move to 1, 3 and 5
			replace temp = 3 if `v'==5
			replace temp = 1 if `v'==0
		}
		if "`v'"=="efw_free_foreign_curr" & `i'>=2 { // to recheck
			replace temp = 5 // only category 1 because they are all at the maximum
		}
		if "`v'"=="competition_rules_fin"  { // to recheck
			replace temp = 1 if `v'==0 | `v'==1 
			replace temp = 3 if `v'==2
			replace temp = 4 if `v'==3
			replace temp = 5 if `v'==4
		}
		if "`v'"=="efficiency_superv_bank"  { // to recheck
			replace temp = 4 if `v'==4 // maximum
		}
		if "`v'"=="efficiency_superv_fin"  { // to recheck
			replace temp = 4 if `v'==4 // maximum
			replace temp = 3 if `v'==3 // maximum
		}
		gen s`i'_q_`v' = 1 if temp==1 | temp==2 // bottom 40, 40-60, top 40, medium, high
		replace s`i'_q_`v' = 2 if temp==3
		replace s`i'_q_`v' = 3 if temp==4 | temp==5
		dis "var: `v' sample: `i'"
		tab temp // ok if all quantiles are defined
		drop temp
	}
}
* based on distance to frontier
* same fixes as above
foreach v of global vars {
	foreach i of numlist 1(1)4 {
		if "`v'"=="e_fh_pr" | "`v'"=="e_fh_cl" | "`v'"=="e_p_polity" {
			gen temp = 5 if country=="Uruguay" 
		}
		if "`v'"!="e_fh_pr" & "`v'"!="e_fh_cl" & "`v'"!="e_p_polity" {
 			xtile temp = dtf_`v' if dtf_`v'!=. & sample`i'==1, nq(5) // change quantiles to 3 for low, medium and bottom
		}
		if "`v'"=="evaluation_score" & `i'==4 {
			replace temp = 3 if temp == 2 // only category 1, 2 and 5, but 2 is the middle one
		}
		if "`v'"=="efw_free_foreign_curr" & `i'==1 {
			replace temp = 5 if `v'==10 // only category 1 and 3, to move to 1, 3 and 5
			replace temp = 3 if `v'==5
			replace temp = 1 if `v'==0
		}
		if "`v'"=="efw_free_foreign_curr" & `i'>=2 {
			replace temp = 5 // only category 1 because they are all at the maximum
		}
		if "`v'"=="competition_rules_fin"  {
			replace temp = 1 if `v'==0 | `v'==1 
			replace temp = 3 if `v'==2
			replace temp = 4 if `v'==3
			replace temp = 5 if `v'==4
		}
		if "`v'"=="efficiency_superv_bank"  {
			replace temp = 4 if `v'==4 // maximum
		}
		if "`v'"=="efficiency_superv_fin"  {
			replace temp = 4 if `v'==4 // maximum
			replace temp = 3 if `v'==3 // maximum
		}
		gen s`i'_qdtf_`v' = 1 if temp==1 | temp==2 // bottom 40, 40-60, top 40, medium, high
		replace s`i'_qdtf_`v' = 2 if temp==3
		replace s`i'_qdtf_`v' = 3 if temp==4 | temp==5
		drop temp
	}
}
* based on distance to frontier, for institutional families
* no issue, 5 quantiles for all families
foreach v of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	foreach i of numlist 1(1)4 {
 		xtile temp = dtf_`v' if dtf_`v'!=. & sample`i'==1, nq(5) // change quantiles to 3 for low, medium and bottom
		gen s`i'_qdtf_`v' = 1 if temp==1 | temp==2 // bottom 40, 40-60, top 40, medium, high
		replace s`i'_qdtf_`v' = 2 if temp==3
		replace s`i'_qdtf_`v' = 3 if temp==4 | temp==5
		tab temp
		drop temp
	}
}

* calculate quantiles for different samples - only for sample 2, 4 quintiles then grouped in 0-25, 25-5-, 50+
* based on ranking
foreach v of global vars {
	foreach i of numlist 2 {
		if "`v'"=="e_fh_pr" | "`v'"=="e_fh_cl" | "`v'"=="e_p_polity" | "`v'"=="v2lgqugen" {
			gen temp = 4 if country=="Uruguay" 
		}
		if "`v'"!="e_fh_pr" & "`v'"!="e_fh_cl" & "`v'"!="e_p_polity" & "`v'"!="v2lgqugen" {
			xtile temp = `v' if `v'!=. & sample`i'==1, nq(4) // change quantiles to 3 for low, medium and bottom
		}
		if "`v'"=="evaluation_score" & `i'==4 {
			replace temp = 3 if temp == 2 // only category 1, 2 and 5, but 2 is the middle one
		}
		if "`v'"=="efw_free_foreign_curr" {
			replace temp = 4 // only category 1 because they are all at the maximum
		}
		if "`v'"=="competition_rules_fin"  {
			replace temp = 1 if `v'==0 | `v'==1 | `v'==2
			replace temp = 2 if `v'==3
			replace temp = 4 if `v'==4
		}
		if "`v'"=="efficiency_superv_bank"  {
			replace temp = 4 if `v'==4 // maximum
		}
		if "`v'"=="efficiency_superv_fin"  {
			replace temp = 4 if `v'==4 // maximum
			replace temp = 3 if `v'==3 // maximum
		}
		gen s`i'_q50_`v' = 1 if temp==1 // bottom 25, 25-50, top 50
		replace s`i'_q50_`v' = 2 if temp==2
		replace s`i'_q50_`v' = 3 if temp==3 | temp==4
		dis "var: `v' sample: `i'"
		tab temp
		drop temp
	}
}

* based on distance to frontier: same fixes as above
foreach v of global vars {
	foreach i of numlist 2 {
		if "`v'"=="e_fh_pr" | "`v'"=="e_fh_cl" | "`v'"=="e_p_polity" | "`v'"=="v2lgqugen" {
			gen temp = 4 if country=="Uruguay" 
		}
		if "`v'"!="e_fh_pr" & "`v'"!="e_fh_cl" & "`v'"!="e_p_polity" & "`v'"!="v2lgqugen" {
			xtile temp = `v' if `v'!=. & sample`i'==1, nq(4) // change quantiles to 3 for low, medium and bottom
		}
		if "`v'"=="efw_free_foreign_curr" {
			replace temp = 4 // only category 1 because they are all at the maximum
		}
		if "`v'"=="competition_rules_fin"  {
			replace temp = 1 if `v'==0 | `v'==1 | `v'==2
			replace temp = 2 if `v'==3
			replace temp = 4 if `v'==4
		}
		if "`v'"=="efficiency_superv_bank"  {
			replace temp = 4 if `v'==4 // maximum
		}
		if "`v'"=="efficiency_superv_fin"  {
			replace temp = 4 if `v'==4 // maximum
			replace temp = 3 if `v'==3 // maximum
		}
		gen s`i'_q50dtf_`v' = 1 if temp==1  // bottom 25, 25-50, top 50
		replace s`i'_q50dtf_`v' = 2 if temp==2
		replace s`i'_q50dtf_`v' = 3 if temp==3 | temp==4
		drop temp
	}
}
* based on distance to frontier, for institutional families
* no issue, 5 quantiles for all families
foreach v of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	foreach i of numlist 2 {
		xtile temp = dtf_`v' if dtf_`v'!=. & sample`i'==1, nq(4) // change quantiles to 3 for low, medium and bottom
		gen s`i'_q50dtf_`v' = 1 if temp==1  // bottom 25, 25-50, top 50
		replace s`i'_q50dtf_`v' = 2 if temp==2
		replace s`i'_q50dtf_`v' = 3 if temp==3 | temp==4
		drop temp
	}
}




*(EA deleted residuals estimation)


* create average DTF across comparators
foreach v of global vars {
	egen dtf_`v'_s1 = mean(dtf_`v') if sample1==1
	egen dtf_`v'_s2 = mean(dtf_`v') if sample2==1
	egen dtf_`v'_s3 = mean(dtf_`v') if sample3==1
	egen dtf_`v'_s4 = mean(dtf_`v') if sample4==1
}
foreach v of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {	
	egen dtf_`v'_s1 = mean(dtf_`v') if sample1==1
	egen dtf_`v'_s2 = mean(dtf_`v') if sample2==1
	egen dtf_`v'_s3 = mean(dtf_`v') if sample3==1
	egen dtf_`v'_s4 = mean(dtf_`v') if sample4==1
}

* save
save "$path_data_cleaned/residuals_cleaned.dta", replace

