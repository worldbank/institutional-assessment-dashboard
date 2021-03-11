* 120720
* previous do file: analysis_IA_gaps3.does


clear all
set more off

cd "$mainpath"

* paths
global path_data_raw "data_raw"
global path_data_cleaned "data_cleaned"
global path_graphs "graphs2"

*******************************************************************
* GRAPHS
*******************************************************************

use "$path_data_cleaned/residuals_cleaned.dta", clear
keep dtf_*_s1 dtf_*_s2 dtf_*_s3 dtf_*_s4
collapse dtf_*_s1 dtf_*_s2 dtf_*_s3 dtf_*_s4
rename dtf_*_s1 dtf_s1_*
rename dtf_*_s2 dtf_s2_*
rename dtf_*_s3 dtf_s3_*
rename dtf_*_s4 dtf_s4_*
gen i = 1
reshape long dtf_s1_ dtf_s2_ dtf_s3_ dtf_s4_ , i(i) j(var) string
rename dtf_s*_ dtf_s*_mean
drop i
tempfile means
save `means'

use "$path_data_cleaned/residuals_cleaned.dta", clear

* reshape for graph with [residuals; deleted EA] and quantiles
drop dtf_*_s1 dtf_*_s2 dtf_*_s3 dtf_*_s4
*keep s*_q_* s*_rank_* s*_rgap_* country sample*_size dtf_* s*_qdtf_* s*_q50_* s*_q50dtf_*
keep s*_q_* country sample*_size dtf_* s*_qdtf_* s*_q50_* s*_q50dtf_*
reshape long s1_q_ s2_q_ s3_q_ s4_q_ s1_rank_ s2_rank_ s3_rank_ s4_rank_ s1_rgap_ s2_rgap_ s3_rgap_ s4_rgap_ ///
	s1_qdtf_ s2_qdtf_ s3_qdtf_ s4_qdtf_ dtf_ s2_q50dtf_ s2_q50_ , i(country) j(var) string
	rename s*_rank_ s*_rank
	rename s*_q_ s*_quant
	rename s*_rgap_ s*_rgap
	rename dtf_ dtf
	rename s*_qdtf_ s*_quant_dtf
	rename s*_q50dtf_ s2_quant50_dtf
	rename s*_q50_ s2_quant50
gen s1_dtf = dtf
gen s2_dtf = dtf
gen s3_dtf = dtf
gen s4_dtf = dtf
drop dtf
gen label_var = ""
foreach v of global vars {
	replace label_var = "${l_`v'}" if var=="`v'"	
}
replace label_var = "Legal institutions" if var=="vars_leg"
replace label_var = "Political institutions" if var=="vars_pol"
replace label_var = "Public sector institutions" if var=="vars_publ"
replace label_var = "Social institutions" if var=="vars_social"
replace label_var = "Accountability institutions" if var=="vars_transp"
replace label_var = "Business & trade institutions" if var=="vars_mkt" // `" "Business environment & trade" "institutions" "' if var=="vars_mkt"
replace label_var = "Labor market institutions" if var=="vars_lab"
replace label_var = "Financial institutions" if var=="vars_fin"
replace label_var = "Governance of SOEs" if var=="vars_service_del"

keep if country=="Uruguay"
merge 1:1 var using `means'
drop _m


*EA: what is inst_family 1? the entire families or what?

* identify groups
foreach g of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del vars_other {
	gen `g' = .
	foreach v of global `g' {
		replace `g' = 1 if var=="`v'"
	}
}
gen inst_family = 1 if vars_pol ==. & vars_leg ==. &  vars_transp ==. &  vars_publ ==. &  vars_social ==.  &  vars_mkt ==. &  vars_lab ==. ///
	&  vars_fin ==. &  vars_service_del ==. &  vars_other ==.


	
	
	
	
**** graphs with mean DTF by indicator family ****
*1 graph w/all families, where each bar is one family
* With diamonds for mean in comparator group
* (FIGURE 4 IN QER) [issue w/colors here]
#delimit ;
local note = "Uruguay, LAC6, Structural, OECD" ; local ytitle = "Closeness to frontier" ; local technote1 = "Closeness to frontier is calculated as (worst–y)/(worst–frontier)." ; local technote2 = "1 identifies the best performer and 0 the worst performer." ; local technote3 = "Diamonds represent the average closeness to frontier in the comparator group." ; local technote5="Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%." ;
#delimit cr
foreach v of newlist dtf { 
	foreach j of numlist 2 { 
		local legend1 = "Weak" // Bottom 25, quantile = 1
		local legend2 = "Emerging" // 25-50 quantile, quantile = 2
		local legend3 = "Advanced" //top 50, quantile = 3
		local cols = 3
		local col1 = "red"
		local col2 = "yellow"
		local col3 = "green"
	
		gen quant = s`j'_quant50_dtf if inst_family==1
		gen vsort = s`j'_dtf if inst_family==1

		gsort inst_family -s`j'_`v'
		gen sort = _n  if inst_family==1
		
		labmask sort  if inst_family==1, values(label_var) 
		separate s`j'_`v' if inst_family==1 , by(quant) veryshortlabel

		if `cols'==3 {
			#delimit ;
			twoway 	bar s`j'_`v'? sort if inst_family==1 & quant==1, horizontal bcolor(`col1') ylabel(1/9, valuelabel angle(0)) barw(0.8) || //
					bar s`j'_`v'? sort if inst_family==1 & quant==2, horizontal bcolor(`col2') ylabel(1/9, valuelabel angle(0)) || // don't know why this does not work for coloring the other bars
					bar s`j'_`v'? sort if inst_family==1 & quant==3, horizontal bcolor(`col3') ylabel(1/9, valuelabel angle(0)) ||
				scatter  sort dtf_s2_mean if inst_family==1, msymbol(diamond) msize(large) mcolor(black)
				legend(order(1 "`legend1'" 2 "`legend2'" 3 "`legend3'") col(`cols') size(small))
				xtitle("`ytitle'", size(small)) ytitle("") ylabel(, labsize(small))
				graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote3'" "`technote5'")
			;
			#delimit cr
		}
		graph export "$path_graphs/IA_gaps/families_s`j'_quantiles50_mean_plus.png", replace
		drop sort  s`j'_`v'? quant vsort
	}	
}



*********************************************************************
* (FIGURES 5-14 IN QER) FIGURE OF INDIVIDUAL INDICATORS BY INST FAMILY
*********************************************************************
**** graphs by indicators families: vars_`family'_s`j'_quantiles50_dtf
						*where `group' = eg, vars_fin , vars_leg, etc
* graph with quantiles, distance to frontier - only for comparison with oecd and lac6, bottom 25, 25-50, bottom 50


foreach group of varlist vars_mkt vars_publ vars_lab  vars_pol vars_leg  vars_social  vars_lab vars_fin vars_service_del vars_leg vars_publ vars_transp { //  
	local note = "Uruguay, LAC6, Structural, OECD"
	foreach v of newlist dtf { 
		if "`v'" == "dtf" {
			local ytitle = "Closeness to frontier" 
			local technote1 = "Closeness to frontier is calculated as (worst–y)/(worst–frontier)." 
			local technote2 = "1 identifies the best performer and 0 the worst performer." 
			local technote5="Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."
		}
		foreach j of numlist 2 { 
			if "`group'"!="vars_transp" & "`group'"!="vars_publ" & "`group'"!="vars_social" {
				local legend1 = "Weak" // Bottom 25, quantile = 1
				local legend2 = "Emerging" // 25-50 quantile, quantile = 2
				local legend3 = "Advanced" //top 50, quantile = 3
				local cols = 3
				local col1 = "red"
				local col2 = "yellow"
				local col3 = "green"
			}
			if "`group'"=="vars_transp" {
				local legend1 = "Emerging" // quantile = 2
				local legend2 = "Advanced" // quantile = 3
				local cols = 2
				local col1 = "yellow"
				local col2 = "green"
			}
			if "`group'"=="vars_publ" {
				local legend1 = "Weak" // quantile = 1
				local legend2 = "Emerging" // quantile = 2
				local cols = 2
				local col1 = "red"
				local col2 = "yellow"
			}
			if "`group'"=="vars_social" {
				local legend1 = "Emerging" // quantile = 2
				local legend2 = "Advanced" // quantile = 3
				local cols = 2
				local col1 = "yellow"
				local col2 = "green"
			}
			if "`group'"=="vars_service_del" {
				local legend1 = "Weak" // quantile = 1
				local legend2 = "Emerging" // quantile = 2
				local cols = 2
				local col1 = "red"
				local col2 = "yellow"
			}
			
			if "`v'" == "dtf" {
				gen quant = s`j'_quant50_dtf if `group'==1
				gen vsort = s`j'_dtf if `group'==1
			}

			gsort s`j'_`v'
			gen sort = _n  if `group'==1
			
			labmask sort  if `group'==1, values(label_var) 
			separate s`j'_`v' if `group'==1 , by(quant) veryshortlabel

			if `cols'==3 {
				#delimit ;
					graph hbar s`j'_`v'? if `group'==1, over(sort, sort(-vsort) lab(labsize(small)) ) 
					bar(1, color(`col1')) bar(2, color(`col2')) bar(3, color(`col3')) 
					legend(order(1 "`legend1'" 2 "`legend2'" 3 "`legend3'") col(`cols'))
					ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote5'")
				;
				#delimit cr
			}
			if `cols'==2 {
				#delimit ;
					graph hbar s`j'_`v'? if `group'==1, over(sort, sort(-vsort) lab(labsize(small))) bar(1, color(`col1')) bar(2, color(`col2')) 
					legend(order(1 "`legend1'" 2 "`legend2'") col(`cols'))
					ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote5'")
				;
				#delimit cr
			}
			graph export "$path_graphs/IA_gaps/`group'_s`j'_quantiles50_`v'_plus.png", replace
			drop sort  s`j'_`v'? quant vsort
		}	
	}
}

