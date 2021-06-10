# Packages ===================================================================================

packages <-
  c("tidyverse",
    "tidyr",
    "here")

pacman::p_load(packages,
               character.only = TRUE)

# Data =======================================================================================
data_selected <-
  read_rds(file.path("data",
                     "raw_data.rds"))

source(file.path("auxiliary",
                 "vars-by-family.R"))

# Inputs =====================================================================================
variable_names <-
  data_selected %>%
  select(
    all_of(vars_all)
  ) %>%
  pivot_longer(
    everything()
  ) %>%
  select(variable=name,-value) %>%
  unique %>%
  arrange(variable) %>%
  add_row(
    variable = family_names
  ) %>%
  mutate(
    var_level = ifelse(variable %in% family_names,"family","indicator"),
    var_name =
      case_when(
        variable == "centregov_mean" ~ "Centre of Government, influence",
        variable == "nontariff_barriers" ~ "Non-tariffs trade barriers",
        variable == "property_rights" ~ "Property rights",
        variable == "protection_incumbents" ~ "Regulatory protection of incumbents",
        variable == "complexity_procedures" ~ "Complexity of regulatory procedures",
        variable == "barriers_startups" ~ "Administrative burdens on startups",
        variable == "barriers_trade_expl" ~ "Explicit barriers to trade and investment",
        variable == "barriers_trade_oth" ~ "Other barriers to trade",
        variable == "regulatory_governance" ~ "Consolidated regulatory governance score",
        variable == "open_data_barometer" ~ "Publicized laws and government data",
        variable == "rigorous_impartial_pa" ~ "Rigorous and impartial public administration",
        variable == "legaleff_challenging" ~ "Efficiency of legal framework in challenging regs",
        variable == "legaleff_disputes" ~ "Efficiency of legal framework in settling disputes",
        variable == "insolvency_framework" ~ "Resolving insolvency: Strength of insolvency framework index",
        variable == "financial_institution" ~ "Financial Institutions",
        variable == "access_credit_overall" ~ "Ease of getting credit",
        variable == "competition_rules_fin" ~ "Financial sector: competition regulation",
        variable == "efficiency_superv_bank" ~ "Efficiency of the banking supervisory authority",
        variable == "efficiency_superv_fin" ~ "Efficiency of the financial market supervisory authority",
        variable == "minimum_wage_ratio" ~ "Minimum to mean wage ratio",
        variable == "union_density" ~ "Union density",
        variable == "empl_protection_perm" ~ "Employment protection, regular",
        variable == "empl_protection_temp" ~ "Employment protection, temp.",
        variable == "collective_barg" ~ "Collective bargaining coverage",
        variable == "govreg_burden" ~ "Burden of government regulation, 1-7 (best)",
        variable == "efw_labor_mkt_reg" ~ "Restrictive Labor Regulations",
        variable == "wsj_financialfreedom" ~ "Index of economic freedom score",
        variable == "wsj_businessfreedom" ~ "Business freedom",
        variable == "wsj_propertyrights" ~ "Property rights",
        variable == "efw_property_rights" ~ "Property rights protection",
        variable == "efw_reg_trade_barr" ~ "Regulatory trade barriers",
        variable == "efw_controls_movement" ~ "Control capital/people movs",
        variable == "efw_businessreg" ~ "Business regulations",
        variable == "efw_credit_mkt_reg" ~ "Credit market regulations",
        variable == "efw_free_foreign_curr" ~ "Foreign Currency Regulations",
        variable == "f3_security" ~ "Law  and order",
        variable == "trust_pol" ~ "Public trust in politicians",
        variable == "v2dlengage" ~ "Engaged society",
        variable == "f2_corruption" ~ "Absence of corruption (Global States of Democracy)",
        variable == "e_ti_cpi" ~ "Corruption",
        variable == "f4_rights" ~ "Fundamental rights",
        variable == "v2lgqugen" ~ "Lower chamber gender quota",
        variable == "v2pepwrsoc" ~ "Power distributed by social group",
        variable == "v2pepwrses" ~ "Power distributed by socio-economic position",
        variable == "f1_govpowers" ~ "Executive constraints",
        variable == "e_fh_pr" ~ "Political Rights",
        variable == "e_fh_cl" ~ "Civil Liberties",
        variable == "e_p_polity" ~ "Revised Combined Polity Score",
        variable == "f8_criminaljustice" ~ "Criminal justice",
        variable == "f7_civiljustice" ~ "People can access and afford civil justice",
        variable == "f6_regulatoryenf" ~ "Regulatory enforcement",
        variable == "f5_opengov" ~ "Open government",
        variable == "dskills_value_2018" ~ "Digital skills",
        variable == "onlineserviceindex" ~ "Egov, Online service",
        variable == "humancapitalindex" ~ "Egov, Human capital",
        variable == "telecom_infr" ~ "Egov, Telecom infr",
        variable == "prebid_score" ~ "Procurement-pre-bid",
        variable == "bid_submission_score" ~ "Procurement-submission",
        variable == "evaluation_score" ~ "Procurement-evaluation",
        variable == "contract_manag_score" ~ "Procurement-contract mgmt",
        variable == "perf_guarantee_score" ~ "Procurement-performance gntee",
        variable == "payments_score" ~ "Procurement-payment",
        variable == "proc_mean_score" ~ "Procurement",
        variable == "val_direct_contracts_sh" ~ "Share of direct contracts (value)",
        variable == "wgi_voice_acc" ~ "Voice and accountability",
        variable == "wgi_pol_stability" ~ "Political stability",
        variable == "wgi_gov_effective" ~ "Government effectiveness",
        variable == "wgi_regulatory" ~ "Regulatory quality",
        variable == "wgi_rulelaw" ~ "Rule of law",
        variable == "wgi_control_corr" ~ "Control of corruption",
        variable == "v2x_cspart" ~ "Civil society participation",
        variable == "v2pepwrgen" ~ "Power distributed by gender",
        variable == "cbi" ~ "Central Bank independence",
        variable == "bribes" ~ "Irregular payments and bribes",
        variable == "command_control" ~ "Use of command and control regulation",
        variable == "constr_perm_overall" ~ "Dealing with construction permits: Procedures",
        variable == "customs_burden" ~ "Burden of customs procedures",
        variable == "diversion_pfunds" ~ "Diversion of public funds",
        variable == "eff_antimonopoly" ~ "Effectiveness of antimonopoly policy",
        variable == "eff_govspending" ~ "Wastefulness of government spending",
        variable == "efw_capitalcontrols" ~ "Capital controls",
        variable == "efw_integrity_legalsys" ~ "Integrity of the legal system",
        variable == "efw_inv_restr" ~ "Foreign Investment Restrictions",
        variable == "efw_tourist" ~ "Freedom of entry for foreigners",
        variable == "egovernmentindex" ~ "Government Online Service Index",
        variable == "enf_contr_overall" ~ "Enforcing contracts: Cost",
        variable == "eparticipationindex" ~ "E-Participation Index, 0-1 (best)",
        variable == "es_court_constraint" ~ "Corruption / Percent of firms identifying the courts system as a major constraint",
        variable == "favoritism" ~ "Favoritism in decisions of government officials",
        variable == "gci_overall" ~ "GCI 4.0: Global Competitiveness Index 4.0",
        variable == "governance_soe" ~ "Governance of state-owned enterprises",
        variable == "lpi_clearance_eff" ~ "GCI 4.0: Efficiency of the clearance process",
        variable == "mkt_dominance" ~ "Extent of market dominance",
        variable == "pay_taxes_overall" ~ "Paying taxes: Time",
        variable == "price_controls" ~ "Price controls",
        variable == "protect_minority_ov" ~ "Ease of protecting minority investors",
        variable == "register_prop_overall" ~ "Registering property: Cost",
        variable == "resolve_insolv_overall" ~ "Resolving insolvency Outcome",
        variable == "start_bus_overall" ~ "Ease of starting a business",
        variable == "trade_borders_overall" ~ "Trading across borders",
        variable == "transparency_polmak" ~ "Transparency of government policymaking",
        variable == "v2juaccnt" ~ "Judicial accountability",
        variable == "v2lgfemleg" ~ "Lower chamber female legislators",
        variable == "v2xcs_ccsi" ~ "Core civil society index",
        variable == "wef_border_admin" ~ "GCI 4.0: Border clearance efficiency",
        variable == "vars_leg" ~ "Legal institutions",
        variable == "vars_pol" ~ "Political institutions",
        variable == "vars_publ" ~ "Public sector performance institutions",
        variable == "vars_social" ~ "Social institutions",
        variable == "vars_transp" ~ "Accountability institutions",
        variable == "vars_mkt" ~ "Business environment and trade institutions",
        variable == "vars_lab" ~ "Labor market institutions",
        variable == "vars_fin" ~ "Financial market institutions",
        variable == "vars_service_del" ~ "Institutions for service delivery"
      ),
    family_var =
      case_when(
        variable %in% vars_leg | variable == "vars_leg" ~ "vars_leg",
        variable %in% vars_pol | variable == "vars_pol" ~ "vars_pol",
        variable %in% vars_publ | variable == "vars_publ" ~ "vars_publ",
        variable %in% vars_social | variable == "vars_social" ~ "vars_social",
        variable %in% vars_transp | variable == "vars_transp" ~ "vars_transp",
        variable %in% vars_mkt | variable == "vars_mkt" ~ "vars_mkt",
        variable %in% vars_lab | variable == "vars_lab" ~ "vars_lab",
        variable %in% vars_fin | variable == "vars_fin" ~ "vars_fin",
        variable %in% vars_service_del | variable == "vars_service_del" ~ "vars_service_del"
      ),
    family_name =
      case_when(
        variable %in% vars_leg | variable == "vars_leg" ~ "Legal institutions",
        variable %in% vars_pol | variable == "vars_pol" ~ "Political institutions",
        variable %in% vars_publ | variable == "vars_publ" ~ "Public sector performance institutions",
        variable %in% vars_social | variable == "vars_social" ~ "Social institutions",
        variable %in% vars_transp | variable == "vars_transp" ~ "Accountability institutions",
        variable %in% vars_mkt | variable == "vars_mkt" ~ "Business environment and trade institutions",
        variable %in% vars_lab | variable == "vars_lab" ~ "Labor market institutions",
        variable %in% vars_fin | variable == "vars_fin" ~ "Financial market institutions",
        variable %in% vars_service_del | variable == "vars_service_del" ~ "Institutions for service delivery"
      )
  ) %>%
  arrange(family_var)

# Save datasets ###########################################################################

  write_rds(variable_names,
            file.path("data",
                      "variable_names.rds"))

