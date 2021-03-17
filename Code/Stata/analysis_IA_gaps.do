
* created by serena, 2019 dec 06
* updated: 2019 dec 06
* updated: 2020 jan 21
* this version: 2020 june 05

clear all
set maxvar 32000

* set seed
set seed 20191204
set more off

* Set directories and main path
	if "`c(username)'" == "wb479311" {
		global mainpath	"C:\Users\wb479311\WBG\Serena Sara Daniela Cocciolo - IA_benchmarking\benchmarking\"
	}
	if "`c(username)'" == "wb406790" {
		global mainpath	"C:\Users\wb406790\OneDrive - WBG\IA_benchmarking\benchmarking"
	}
cd "$mainpath"

* paths
global path_data_raw "data_raw"
global path_data_cleaned "data_cleaned"
global path_graphs "graphs_test"


************************************************************************************************************************************************************
********************************************************************* import cleaned data *******************************************************************
************************************************************************************************************************************************************

use "$path_data_cleaned/merged_for_residuals.dta", clear

************************************************************************************************************************************************************
******************************************************************* basic additional cleaning **************************************************************
************************************************************************************************************************************************************

* rename variables
{
	rename telecommunicationinfrastructurei telecom_infr
	rename daigovernmentsubindex_2016 daigov_2016
	rename contract_management_score contract_manag_score
	rename performance_guarantee_score perf_guarantee_score
	rename mean_score proc_mean_score
	rename f6_regulatoryenforcement f6_regulatoryenf
	rename efw_integrityofthelegalsystem efw_integrity_legalsys
	rename efw_legalenforcementofcontracts efw_contracts_enf
	rename efw_businessregulations efw_businessreg
	rename efw_freedom_foreign_curr efw_free_foreign_curr
	rename protect_minority_overall protect_minority_ov
	rename efficiancy_supervision_banking efficiency_superv_bank
	rename efficiancy_supervision_finmkt efficiency_superv_fin
	rename bl getting_credit
	rename resolvinginsolvencystrength insolvency_framework
	rename gettingcreditcreditregistry credit_registry_cov // Peter McConaghy suggested to include this, but already included in access_credit_overall
	rename v2clrspct rigorous_impartial_pa
	rename barriers_trade_explicit barriers_trade_expl
	rename barriers_trade_other barriers_trade_oth
	rename lvau cbi
	rename efw_foreign_invest_restr efw_inv_restr
	rename efw_freedomofforeignerstovisit efw_tourist
}

* fix vars with opposite scale
	* reason: for the DTF methodology, we need for all indicators that "higher values" means "better performance"
foreach v of varlist e_fh_pr e_fh_cl {
	replace `v' = 8 - `v' // freedom house: Countries are graded between 1 (most free) and 7 (least free).
}
replace e_p_polity = . if e_p_polity<-10 // missing
foreach v of varlist governance_soe price_controls command_control complexity_procedures barriers_startups protection_incumbents barriers_trade_expl barriers_trade_oth {
	replace `v' = 6 - `v' // PRM indicators: Countries are graded between 0 (less control/involvement) and 6 (more control/involvement)
}

* label vars
{
	label var centregov_mean "Centre of Government, influence"
	label var nontariff_barriers "Non-tariffs trade barriers"
	label var property_rights "Property rights"
	label var protection_incumbents "Protection of incumbents"
	label var complexity_procedures "Complex regulatory procedures"
	label var barriers_startups "Admin burdens on start-ups"
	label var barriers_trade_expl "Explicit barriers to trade"
	label var barriers_trade_oth "Other barriers to trade"
	label var regulatory_governance "Regulatory governance"
	label var open_data_barometer "Open Data Barometer"
	label var rigorous_impartial_pa "Rigorous and impartial PA"
	label var legaleff_challenging "Challenging regulations" 
	label var legaleff_disputes "Settling disputes"
	label var insolvency_framework "Insolvency framework, strength"
	label var financial_institution "Financial Institutions"
	label var access_credit_overall "Getting credit"
	label var competition_rules_fin "Competition regulation"
	label var efficiency_superv_bank "Supervision efficiency-banking"
	label var efficiency_superv_fin "Supervision efficiency-financial"
	label var minimum_wage_ratio "Minimum to mean wage ratio"
	label var union_density "Union density"
	label var empl_protection_perm "Employment protection, regular"
	label var empl_protection_temp "Employment protection, temp."
	label var collective_barg "Collective bargaining coverage"
	label var govreg_burden "Burden of gov. regulation"
	label var efw_labor_mkt_reg "Labor market regulations"
	label var wsj_financialfreedom "Financial freedom"
	label var wsj_businessfreedom "Business freedom"
	label var wsj_propertyrights "Property rights"
	label var efw_property_rights "Property rights protection"
	label var efw_reg_trade_barr "Regulatory trade barriers"
	label var efw_controls_movement "Control capital/people movs"
	label var efw_businessreg "Business regulations"
	label var efw_credit_mkt_reg "Credit market regulations"
	label var efw_free_foreign_curr "Freedom, foreign bank accounts"
	label var f3_security "Order and security"
	label var trust_pol "Public trust in politician"
	label var v2dlengage "Engaged society"
	label var f2_corruption "Absence of corruption"
	label var e_ti_cpi "Perception of corruption"
	label var f4_rights "Fundamental rights"
	label var v2lgqugen "Lower chamber gender quota"
	label var v2pepwrsoc "Power by social group"
	label var v2pepwrses "Power by socioeconomic position"
	label var f1_govpowers "Constraints on Gov. Powers"
	label var e_fh_pr "Political rights"
	label var e_fh_cl "Civil liberties"
	label var e_p_polity "Polity IV score"
	label var f8_criminaljustice "Criminal justice"
	label var f7_civiljustice "Civil justice"
	label var f6_regulatoryenf "Regulatory enforcement"
	label var f5_opengov "Open government"
	label var dskills_value_2018 "Digital skills"
	label var onlineserviceindex "Egov, Online service"
	label var humancapitalindex "Egov, Human capital"
	label var telecom_infr "Egov, Telecom infr"
	label var prebid_score "Procurement-pre-bid"
	label var bid_submission_score "Procurement-submission"
	label var evaluation_score "Procurement-evaluation"
	label var contract_manag_score "Procurement-contract mgmt"
	label var perf_guarantee_score "Procurement-performance gntee"
	label var payments_score "Procurement-payment"
	label var proc_mean_score "Procurement score"
	label var val_direct_contracts_sh "Share of direct contracts (value)"
	label var wgi_voice_acc "Voice and accountability"
	label var wgi_pol_stability "Political stability"
	label var wgi_gov_effective "Government effectiveness"
	label var wgi_regulatory "Regulatory quality"
	label var wgi_rulelaw "Rule of law"
	label var wgi_control_corr "Control of corruption"
	label var v2x_cspart "Civil society participation"
	label var v2pepwrgen "Power by gender"
}

************************************************************************************************************************************************************
******************************************************************** choose indicators of interest *********************************************************
************************************************************************************************************************************************************


* SC: we can consider making this step interactive in the dashboard, allowing users to choose which indicator they want to look at using the DTF methodology (low priority) 

* EA: removed WGI


*1 POLITICAL INSTITUTIONS
global vars_pol e_fh_pr e_fh_cl e_p_polity f1_govpowers v2pepwrsoc v2pepwrses v2pepwrgen v2lgqugen f4_rights f3_security v2lgfemleg 
	* dropped EA: wgi_pol_stability

*2 SOCIAL INSTITUTIONS
global vars_social v2x_cspart v2dlengage v2xcs_ccsi trust_pol

*3 ACCOUTABILITY INSTITUTIONS
global vars_transp e_ti_cpi f2_corruption favoritism bribes diversion_pfunds transparency_polmak egovernmentindex eparticipationindex f5_opengov rigorous_impartial_pa open_data_barometer 
	* dropped EA: wgi_voice_acc wgi_control_corr

*4 CENTER OF GOV/PUBLIC SECTOR INSTITUTIONS
global vars_publ f6_regulatoryenf proc_mean_score eff_govspending regulatory_governance centregov_mean 
	* dropped EA: wgi_regulatory wgi_gov_effective 
	* dropped SC: gov_efficiency (GCI)

*5 LEGAL INSTITUTIONS
global vars_leg judicial_ind  f8_criminaljustice f7_civiljustice es_court_constraint   v2juaccnt  efw_integrity_legalsys legaleff_challenging  legaleff_disputes enf_contr_overall resolve_insolv_overall
	* dropped SC: fw_contracts_enf efw_impartialcourts
	*EA: judicial ind for LJI from LinzerStanton/VDEM?

*6 BUSINESS ENV. AND TRADE INSTITUTIONS
global vars_mkt govreg_burden gci_overall mkt_dominance eff_antimonopoly nontariff_barriers property_rights efw_inv_restr efw_capitalcontrols efw_tourist customs_burden lpi_clearance_eff wef_border_admin  complexity_procedures barriers_startups protection_incumbents barriers_trade_expl barriers_trade_oth start_bus_overall constr_perm_overall register_prop_overall   protect_minority_ov pay_taxes_overall trade_borders_overall
	* dropped SC: wsj_propertyrights, startbus_days, startbus_procedures, wsj_businessfreedom (already from source, WB DB), efw_property_rights 	efw_reg_trade_barr efw_businessreg (already from source, WEF, GCR: nontariff_barriers property_rights govreg_burden)
	* EA unpacked efw_controls_movement ->  efw_inv_restr efw_capitalcontrols efw_tourist

*7 LABOR MARKET INSTITUTIONS 
global vars_lab efw_labor_mkt_reg collective_barg empl_protection_perm empl_protection_temp union_density minimum_wage_ratio

*8 FINANCIAL INSTITUTITONS
global vars_fin efw_credit_mkt_reg efw_free_foreign_curr competition_rules_fin efficiency_superv_bank efficiency_superv_fin cbi access_credit_overall insolvency_framework
	* dropped SC: getting_credit, credit_registry_cov (subindicators of access_credit_overall)
	* dropped SC: wsj_financialfreedom (as suggested by Peter McConaghy), financial_institutions (it is an outcome)
	* added EA: cbi --> SC: what is it?

*9 SOE Governance/SERVICE DELIVERY INSTITUTIONS 
global vars_service_del governance_soe price_controls command_control 

* GLOBAL GLOBAL
global vars $vars_pol $vars_leg $vars_transp $vars_publ $vars_mkt $vars_social $vars_lab $vars_fin $vars_service_del 

* save variable label for all indicators selected for the analysis
foreach v of global vars {
	global l_`v' : variable label `v'	
}

* count non-missing - SC: this step might be unnecessary
local i = 1
foreach v of global vars {
	gen nonmissing_`i' = 1 if `v'!=.
	local i= `i'+1
}
egen nonmissing_tot=rowtotal(nonmissing_*)
egen nonmissing_tot_tot=total(nonmissing_tot)

* SC: methodological note for PRM indicates that 1998 and 2013 indicators are comparable, but not with 2018 due to change in methodology
	* --> drop if year ==2018
	foreach v of varlist barriers_startups - protection_incumbents {
		replace `v'=. if year==2018
	}

* keep only vars of interest
keep country year lac lac6 oecd structural $vars


************************************************************************************************************************************************************
************************************* calculate global closeness to frontier for each indicator and for each country ***************************************
************************************************************************************************************************************************************

/* methodological notes:
	closeness to frontier (CTF) is global, meaning that we identify the worst and best performance in the full sample (all countries)

	with the closeness to frontier methodology, for each indicator i, we compare the 7-year average (average since 2013) of indicator i with the worst and best performance for indicator i among all countries and in the last Y years (2013 - most recent data)
	
	in the graph that we want to produce, the length of the bars represents the closeness to frontier
*/


* identify worst and best performance for each indicator, for each country (2013 - most recent data)
	* methodological note: in doing business they consider the last 5 years, but here for some indicators we have shorter time series
	* reference: https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf
global vars_minmax
foreach v of global vars {
	egen `v'_max = max(`v') if year >= 2013
	egen `v'_min = min(`v') if year >= 2013
	global vars_minmax "$vars_minmax `v'_max `v'_min " 
}

* collapse at country level. for each country, keep only the average since 2013
* SC: in the long term, this step should be flexible adjusted in the dashboard (keep last 7 years, given the present time)
keep if year >= 2013

* collapse at country level
collapse $vars $vars_minmax oecd lac lac6 structural , by(country)

* calculate closeness to frontier at indicator level
foreach v of global vars {
	gen dtf_`v' = (`v'_min - `v' ) / (`v'_min - `v'_max)
		replace dtf_`v' =  0.01 if dtf_`v'==0 // small adjustments to display a very short bar on the graph, in case dtf = 0
}
drop $vars_minmax

* calculate closeness to frontier at institutional family level (mean of DTF of each indicator)
global dtf_vars_pol
global dtf_vars_leg 
global dtf_vars_transp 
global dtf_vars_publ 
global dtf_vars_social
global dtf_vars_mkt
global dtf_vars_lab
global dtf_vars_fin
global dtf_vars_service_del
foreach g of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	foreach v of global `g' {
		global dtf_`g' "${dtf_`g'} dtf_`v'" 
	}
}
foreach g of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	egen dtf_`g' = rowmean(${dtf_`g'})
		replace dtf_`g' =  0.01 if dtf_`g'==0 // small adjustments to display a very short bar on the graph, in case dtf = 0
}

****************************** the clean data used for the dashboard app should be created at this point!

************************************************************************************************************************************************************
************************************* define the quantile where country X belongs given the set of chosen comparator ***************************************
************************************************************************************************************************************************************

/* methodological notes:
	in the graph that we want to produce, the traffic light coloring is based on the percentile group where country X belongs, given the set of chosen comparator

	this depends on the 1) the country X which is the focus of the analysis, 2) the group of comparator countries, 3) how to define the percentile group. in the dashboard, we need to allow users to choose interactively both 1) and 2), and perhaps 3). 3) is low priority.
	
	this should be done at indicator level, and at institutional family level
	
	in this example:
		1) = uruguay
		2) = OECD countries + Argentina, Brazil, Chile, Colombia, Mexico, Peru
		3) = 0-25, 25-50, 50+
*/

* identify 1) the country X which is the focus of the analysis, 2) the group of comparator countries
global country = "Uruguay"
gen comparator = 1 if oecd==1 | country=="Argentina" | country=="Brazil" | country=="Chile" | country=="Colombia" | country=="Mexico" | country=="Peru" 
gen sample2 = 1 if comparator==1 | country=="${country}"

* consider to simply add "keep if sample==1" here

* count obs per sample - SC: this step may be unnecessary
gen temp = 1
egen sample2_size = total(temp) if sample2==1
drop temp

* calculate quantiles at institutional family level: 4 groups (0-25, 25-50, 50-75, 75-100), then grouped in 0-25, 25-50, 50+
foreach v of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {
	* divide in 4 groups of equale size
	xtile temp = dtf_`v' if dtf_`v'!=. & sample2==1, nq(4)
	* check if 4 groups are created. if not, put dtf to missing for this indicator. this depends 1), 2) and 3), therefore it cannot be fixed ex-ante
	bys temp: gen nvals = _n == 1 & temp!=.
	replace nvals = sum(nvals) 
    replace nvals = nvals[_N] 
	replace temp = . if nvals !=4
	* regroup
	gen s2_q50dtf_`v' = 1 if temp==1  // 0-25
	replace s2_q50dtf_`v' = 2 if temp==2 // 25-50
	replace s2_q50dtf_`v' = 3 if temp==3 | temp==4 // 50+
	drop temp nvals
}

* calculate quantiles at indicator level: 4 groups (0-25, 25-50, 50-75, 75-100), then grouped in 0-25, 25-50, 50+
foreach v of global vars {
	* divide in 4 groups of equale size
	xtile temp = `v' if `v'!=. & sample2==1, nq(4)
	* check if 4 groups are created. if not, put dtf to missing for this indicator. this depends 1), 2) and 3), therefore it cannot be fixed ex-ante
	bys temp: gen nvals = _n == 1 & temp!=.
	replace nvals = sum(nvals) 
	replace nvals = nvals[_N] 
	replace temp = . if nvals !=4
	* regroup
	gen s2_q50dtf_`v' = 1 if temp==1  // bottom 25, 25-50, top 50
	replace s2_q50dtf_`v' = 2 if temp==2
	replace s2_q50dtf_`v' = 3 if temp==3 | temp==4
	drop temp nvals
}

* gen average CTF across comparators
foreach v of global vars {
	egen dtf_`v'_s2 = mean(dtf_`v') if comparator==1
}
foreach v of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del {	
	egen dtf_`v'_s2 = mean(dtf_`v') if comparator==1
}


********************************************************************************************************************************************************
**************************************************************************** graphs *******************************************************************
********************************************************************************************************************************************************

* save tempfile with the average CTF across comparators (to be merged later)
preserve
keep dtf_*_s2
collapse dtf_*_s2
rename dtf_*_s2 dtf_s2_*
gen i = 1
reshape long dtf_s2_ , i(i) j(var) string
rename dtf_s2_ dtf_s2_mean
drop i
tempfile means
save `means'
restore

* reshape data from wide to for graph with residuals and quantiles: we need indicators on rows to make the graph in stata
	* note: this is "mechanisms" for making the graph in stata, it might be different in R
drop dtf_*_s2
keep country sample*_size dtf_* s*_q50dtf_*
reshape long dtf_ s2_q50dtf_ , i(country) j(var) string
* rename some vars after reshape long
	rename dtf_ dtf
	rename s2_q50dtf_ s2_quant50_dtf
* fixing some vars
	gen s2_dtf = dtf
	drop dtf
* create variable with var labels
gen label_var = ""
foreach v of global vars {
	replace label_var = "${l_`v'}" if var=="`v'"	
}
replace label_var = "Legal institutions" if var=="vars_leg"
replace label_var = "Political institutions" if var=="vars_pol"
replace label_var = "Public sector institutions" if var=="vars_publ"
replace label_var = "Social institutions" if var=="vars_social"
replace label_var = "Accountability institutions" if var=="vars_transp"
replace label_var = "Business & trade institutions" if var=="vars_mkt"
replace label_var = "Labor market institutions" if var=="vars_lab"
replace label_var = "Financial institutions" if var=="vars_fin"
replace label_var = "Governance of SOEs" if var=="vars_service_del"

* keep country of interest
keep if country=="${country}"

* merge with average CTF across comparators
merge 1:1 var using `means'
drop _m

* identify institutional families for graphs 
foreach g of newlist vars_pol vars_leg vars_transp vars_publ vars_social vars_mkt vars_lab vars_fin vars_service_del  {
	gen `g' = .
	foreach v of global `g' {
		replace `g' = 1 if var=="`v'"
	}
}
gen inst_family = 1 if vars_pol ==. & vars_leg ==. &  vars_transp ==. &  vars_publ ==. &  vars_social ==.  &  vars_mkt ==. &  vars_lab ==. ///
	&  vars_fin ==. &  vars_service_del ==. 


**** 1): graph with mean DTF by indicator family ****

local note = "${country}, LAC6, OECD" // this should be adapted given the group of comparators, but i am afraid of having a note with a long list of countries. maybe we can drop this note
local ytitle = "Closeness to frontier" 
local technote1 = "Closeness to frontier is calculated as (worst–y)/(worst–frontier)." 
local technote2 = "1 identifies the best performer and 0 the worst performer." 
local technote3="Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."
* define number of colors needed and which colors are needed (this is stata "mechanisms", there might be a better way in R)
	sum s2_quant50_dtf if inst_family==1 & s2_quant50_dtf==1
		local quant50_dtf_1 = r(N)
	sum s2_quant50_dtf if inst_family==1 & s2_quant50_dtf==2
		local quant50_dtf_2 = r(N)
	sum s2_quant50_dtf if inst_family==1 & s2_quant50_dtf==3
		local quant50_dtf_3 = r(N)
	if `quant50_dtf_1'>0 & `quant50_dtf_2'>0 & `quant50_dtf_3'>0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local legend2 = "Emerging" // 25-50 quantile, group = 2
		local legend3 = "Advanced" //top 50, group = 3
		local cols = 3
		local col1 = "red"
		local col2 = "yellow"
		local col3 = "green"
	}
	if `quant50_dtf_1'==0 & `quant50_dtf_2'>0 & `quant50_dtf_3'>0 {
		local legend1 = "Emerging" // 25-50 quantile, group = 2
		local legend2 = "Advanced" //top 50, group = 3
		local cols = 2
		local col1 = "yellow"
		local col2 = "green"
	}
	if `quant50_dtf_1'>0 & `quant50_dtf_2'==0 & `quant50_dtf_3'>0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local legend2 = "Advanced" //top 50, group = 3
		local cols = 2
		local col1 = "red"
		local col2 = "green"
	}
	if `quant50_dtf_1'>0 & `quant50_dtf_2'>0 & `quant50_dtf_3'==0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local legend2 = "Emerging" // 25-50 quantile, group = 2
		local cols = 2
		local col1 = "red"
		local col2 = "yellow"
	}
	if `quant50_dtf_1'>0 & `quant50_dtf_2'==0 & `quant50_dtf_3'==0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local cols = 1
		local col1 = "red"
	}
	if `quant50_dtf_1'==0 & `quant50_dtf_2'>0 & `quant50_dtf_3'==0 {
		local legend1 = "Emerging" // 25-50 quantile, group = 2
		local cols = 1
		local col1 = "yellow"
	}
	if `quant50_dtf_1'==0 & `quant50_dtf_2'==0 & `quant50_dtf_3'>0 {
		local legend1 = "Advanced" //top 50, group = 3
		local cols = 1
		local col1 = "green"
	}
*	
gen quant = s2_quant50_dtf if inst_family==1
gen vsort = s2_dtf if inst_family==1
*
gsort inst_family s2_dtf
gen sort = _n  if inst_family==1
*	
labmask sort  if inst_family==1, values(label_var) 
separate s2_dtf if inst_family==1 , by(quant) veryshortlabel
*
if `cols'==3 {
	#delimit ;
	graph hbar s2_dtf? if inst_family==1 & s2_quant50_dtf!=., over(sort, sort(-vsort) lab(labsize(small)) ) 
		bar(1, color(`col1')) bar(2, color(`col2')) bar(3, color(`col3')) 
		legend(order(1 "`legend1'" 2 "`legend2'" 3 "`legend3'") col(`cols') size(small))
		ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote3'")
	;
	#delimit cr
}
if `cols'==2 {
	#delimit ;
	graph hbar s2_dtf? if inst_family==1 & s2_quant50_dtf!=., over(sort, sort(-vsort) lab(labsize(small)) ) 
		bar(1, color(`col1')) bar(2, color(`col2')) 
		legend(order(1 "`legend1'" 2 "`legend2'" ) col(`cols') size(small))
		ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote3'")
	;
	#delimit cr
}
if `cols'==1 {
	#delimit ;
	graph hbar s2_dtf? if inst_family==1 & s2_quant50_dtf!=., over(sort, sort(-vsort) lab(labsize(small)) ) 
		bar(1, color(`col1'))   
		legend(order(1 "`legend1'" ) col(`cols') size(small))
		ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote3'")
	;
	#delimit cr
}
graph export "$path_graphs/families_s2_quantiles50.png", replace
drop sort  s2_dtf? quant vsort

* this is the graph we need, but i would like to add black diamonds to indicate the average CTF across comparators (low priority)



**** 2): graphs by indicators families (one graph per indicator family) ****

foreach group of varlist  vars_fin vars_mkt vars_publ vars_lab vars_pol vars_leg vars_social vars_lab vars_service_del vars_leg vars_publ vars_transp { 
	local note = "Uruguay, LAC6, Structural, OECD"
	local ytitle = "Closeness to frontier" 
	local technote1 = "Closeness to frontier is calculated as (worst–y)/(worst–frontier)." 
	local technote2 = "1 identifies the best performer and 0 the worst performer." 
	local technote5="Weak = bottom 25%; Emerging = 25%-50%; Advanced = top 50%."
	* define number of colors needed and which colors are needed (this is stata "mechanisms", there might be a better way in R)
	sum s2_quant50_dtf if `group'==1 & s2_quant50_dtf==1
		local quant50_dtf_1 = r(N)
	sum s2_quant50_dtf if `group'==1 & s2_quant50_dtf==2
		local quant50_dtf_2 = r(N)
	sum s2_quant50_dtf if `group'==1 & s2_quant50_dtf==3
		local quant50_dtf_3 = r(N)
	if `quant50_dtf_1'>0 & `quant50_dtf_2'>0 & `quant50_dtf_3'>0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local legend2 = "Emerging" // 25-50 quantile, group = 2
		local legend3 = "Advanced" //top 50, group = 3
		local cols = 3
		local col1 = "red"
		local col2 = "yellow"
		local col3 = "green"
	}
	if `quant50_dtf_1'==0 & `quant50_dtf_2'>0 & `quant50_dtf_3'>0 {
		local legend1 = "Emerging" // 25-50 quantile, group = 2
		local legend2 = "Advanced" //top 50, group = 3
		local cols = 2
		local col1 = "yellow"
		local col2 = "green"
	}
	if `quant50_dtf_1'>0 & `quant50_dtf_2'==0 & `quant50_dtf_3'>0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local legend2 = "Advanced" //top 50, group = 3
		local cols = 2
		local col1 = "red"
		local col2 = "green"
	}
	if `quant50_dtf_1'>0 & `quant50_dtf_2'>0 & `quant50_dtf_3'==0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local legend2 = "Emerging" // 25-50 quantile, group = 2
		local cols = 2
		local col1 = "red"
		local col2 = "yellow"
	}
	if `quant50_dtf_1'>0 & `quant50_dtf_2'==0 & `quant50_dtf_3'==0 {
		local legend1 = "Weak" // Bottom 25, group = 1
		local cols = 1
		local col1 = "red"
	}
	if `quant50_dtf_1'==0 & `quant50_dtf_2'>0 & `quant50_dtf_3'==0 {
		local legend1 = "Emerging" // 25-50 quantile, group = 2
		local cols = 1
		local col1 = "yellow"
	}
	if `quant50_dtf_1'==0 & `quant50_dtf_2'==0 & `quant50_dtf_3'>0 {
		local legend1 = "Advanced" //top 50, group = 3
		local cols = 1
		local col1 = "green"
	}
	* mechanics for graphs	
	gen quant = s2_quant50_dtf if `group'==1
	gen vsort = s2_dtf if `group'==1
	*
	gsort s2_dtf
	gen sort = _n  if `group'==1
	*		
	labmask sort  if `group'==1, values(label_var) 
	separate s2_dtf if `group'==1 , by(quant) veryshortlabel
	*
	if `cols'==3 {
		#delimit ;
		graph hbar s2_dtf? if `group'==1 & s2_quant50_dtf!=., over(sort, sort(-vsort) lab(labsize(small)) ) 
			bar(1, color(`col1')) bar(2, color(`col2')) bar(3, color(`col3')) 
			legend(order(1 "`legend1'" 2 "`legend2'" 3 "`legend3'") col(`cols'))
			ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote5'")
		;
		#delimit cr
	}
	if `cols'==2 {
		#delimit ;
			graph hbar s2_dtf? if `group'==1 & s2_quant50_dtf!=., over(sort, sort(-vsort) lab(labsize(small)) ) 
			bar(1, color(`col1')) bar(2, color(`col2')) 
			legend(order(1 "`legend1'" 2 "`legend2'") col(`cols'))
			ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote5'")
		;
		#delimit cr
	}
	if `cols'==1 {
		#delimit ;
			graph hbar s2_dtf? if `group'==1 & s2_quant50_dtf!=., over(sort, sort(-vsort) lab(labsize(small)) ) 
			bar(1, color(`col1') ) 
			legend(order(1 "`legend1'" ) col(`cols'))
			ytitle("`ytitle'") graphregion(color(white)) bgcolor(white)  note("Note: `note'." "`technote1'" "`technote2'" "`technote5'")
		;
		#delimit cr
	}
	graph export "$path_graphs/`group'_s2_quantiles50_dtf.png", replace
	drop sort  s2_dtf? quant vsort
}

