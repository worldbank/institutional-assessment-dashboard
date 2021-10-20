# Packages ===================================================================================

packages <-
  c("tidyverse",
    "tidyr",
    "here")

pacman::p_load(packages,
               character.only = TRUE)

# Data =======================================================================================
data_selected <-
  read_rds(here("data",
                "data_cleaned",
                "selected_vars.rds"))

source(file.path("app/auxiliary",
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
        # New additions out gov360
        variable == "gtmi" ~ "GovTech Maturity Index (GTMI)",
        #variable == "cgsi" ~ "Core Government Systems Index",
        #variable == "psdi" ~ "Public Service Delivery Index",
        #variable == "cei" ~ "Citizen Engagement Index",
        variable == "proff1" ~ "Patronage",
        variable == "close2" ~ "Entry via examination",
        variable == "v2stfisccap" ~ "State fiscal source of revenue",
        variable == "v2stcritrecadm" ~ "Criteria for appointment decisions in the state administration",
        variable == "scopeofstateownedenterprises" ~ "Scope of state-owned enterprises",
        variable == "governmentinvolvementinnetworkse" ~ "Government involvement in network sectors",
        variable == "directcontroloverbusinessenterpr" ~ "Direct control over business enterprises",
        variable == "governanceofstateownedenterprise" ~ "Governance of state-owned enterprises",
        variable == "useofcommandcontrolregulation" ~ "Use of command & control regulation",
        variable == "pricecontrols" ~ "Price controls",
        # Old data
        variable == "centregov_mean" ~ "Centre of Government, influence",
        variable == "nontariff_barriers" ~ "Non-tariffs trade barriers",
        variable == "property_rights" ~ "Property rights",
        variable == "protection_incumbents" ~ "Protection of incumbents",
        variable == "complexity_procedures" ~ "Complex regulatory procedures",
        variable == "barriers_startups" ~ "Admin burdens on start-ups",
        variable == "barriers_trade_expl" ~ "Explicit barriers to trade",
        variable == "barriers_trade_oth" ~ "Other barriers to trade",
        variable == "regulatory_governance" ~ "Regulatory governance",
        variable == "open_data_barometer" ~ "Open Data Barometer",
        variable == "rigorous_impartial_pa" ~ "Rigorous and impartial PA",
        variable == "legaleff_challenging" ~ "Challenging regulations",
        variable == "legaleff_disputes" ~ "Settling disputes",
        variable == "insolvency_framework" ~ "Insolvency framework, strength",
        variable == "financial_institution" ~ "Financial Institutions",
        variable == "access_credit_overall" ~ "Getting credit",
        variable == "competition_rules_fin" ~ "Competition regulation",
        variable == "efficiency_superv_bank" ~ "Supervision efficiency-banking",
        variable == "efficiency_superv_fin" ~ "Supervision efficiency-financial",
        variable == "minimum_wage_ratio" ~ "Minimum to mean wage ratio",
        variable == "union_density" ~ "Union density",
        variable == "empl_protection_perm" ~ "Employment protection, regular",
        variable == "empl_protection_temp" ~ "Employment protection, temp.",
        variable == "collective_barg" ~ "Collective bargaining coverage",
        variable == "govreg_burden" ~ "Burden of gov. regulation",
        variable == "efw_labor_mkt_reg" ~ "Labor market regulations",
        variable == "wsj_financialfreedom" ~ "Financial freedom",
        variable == "wsj_businessfreedom" ~ "Business freedom",
        variable == "wsj_propertyrights" ~ "Property rights",
        variable == "efw_property_rights" ~ "Property rights protection",
        variable == "efw_reg_trade_barr" ~ "Regulatory trade barriers",
        variable == "efw_controls_movement" ~ "Control capital/people movs",
        variable == "efw_businessreg" ~ "Business regulations",
        variable == "efw_credit_mkt_reg" ~ "Credit market regulations",
        variable == "efw_free_foreign_curr" ~ "Freedom, foreign bank accounts",
        variable == "f3_security" ~ "Order and security",
        variable == "trust_pol" ~ "Public trust in politician",
        variable == "v2dlengage" ~ "Engaged society",
        variable == "f2_corruption" ~ "Absence of corruption",
        variable == "e_ti_cpi" ~ "Perception of corruption",
        variable == "f4_rights" ~ "Fundamental rights",
        variable == "v2lgqugen" ~ "Lower chamber gender quota",
        variable == "v2pepwrsoc" ~ "Power by social group",
        variable == "v2pepwrses" ~ "Power by socioeconomic position",
        variable == "f1_govpowers" ~ "Constraints on Gov. Powers",
        variable == "e_fh_pr" ~ "Political rights",
        variable == "e_fh_cl" ~ "Civil liberties",
        variable == "e_p_polity" ~ "Polity IV score",
        variable == "f8_criminaljustice" ~ "Criminal justice",
        variable == "f7_civiljustice" ~ "Civil justice",
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
        variable == "proc_mean_score" ~ "Procurement score",
        variable == "val_direct_contracts_sh" ~ "Share of direct contracts (value)",
        variable == "wgi_voice_acc" ~ "Voice and accountability",
        variable == "wgi_pol_stability" ~ "Political stability",
        variable == "wgi_gov_effective" ~ "Government effectiveness",
        variable == "wgi_regulatory" ~ "Regulatory quality",
        variable == "wgi_rulelaw" ~ "Rule of law",
        variable == "wgi_control_corr" ~ "Control of corruption",
        variable == "v2x_cspart" ~ "Civil society participation",
        variable == "v2pepwrgen" ~ "Power by gender",
        variable == "cbi" ~ "Central bank independence",
        variable == "bribes" ~ "Irregular payments and bribes",
        #variable == "command_control" ~ "Use of command & control regulation",
        variable == "constr_perm_overall" ~ "Construction permits",
        variable == "customs_burden" ~ "Burden of customs procedures",
        variable == "diversion_pfunds" ~ "Diversion of public funds",
        variable == "eff_antimonopoly" ~ "Effectiveness of antimonopoly policy",
        variable == "eff_govspending" ~ "Efficient government spending",
        variable == "efw_capitalcontrols" ~ "Capital controls",
        variable == "efw_integrity_legalsys" ~ "Integrity of the legal system",
        variable == "efw_inv_restr" ~ "Foreign Investment Restrictions",
        variable == "efw_tourist" ~ "Freedom of foreigners to visit",
        #variable == "egovernmentindex" ~ "E-government Index",
        variable == "enf_contr_overall" ~ "Enforcing contracts",
        #variable == "eparticipationindex" ~ "E-participation Index",
        variable == "es_court_constraint" ~ "Courts as major constraint",
        variable == "favoritism" ~ "Favoritism in decisions of government officials",
        variable == "gci_overall" ~ "Global Competitiveness Index",
        variable == "governance_soe" ~ "Regulatory Governance score",
        variable == "lpi_clearance_eff" ~ "Efficiency of the clearance process",
        variable == "mkt_dominance" ~ "Extent of market dominance",
        variable == "pay_taxes_overall" ~ "Paying taxes",
        variable == "protect_minority_ov" ~ "Protecting minority investors",
        variable == "register_prop_overall" ~ "Registering property",
        variable == "resolve_insolv_overall" ~ "Resolving insolvencies",
        variable == "start_bus_overall" ~ "Starting a business",
        variable == "trade_borders_overall" ~ "Trading across borders",
        variable == "transparency_polmak" ~ "Transparency of government policymaking",
        variable == "v2juaccnt" ~ "Judicial accountability",
        variable == "v2lgfemleg" ~ "Lower chamber female legislator",
        variable == "v2xcs_ccsi" ~ "Core civil society index",
        variable == "wef_border_admin" ~ "Efficiency and transparency of boarder administration",
        variable == "vars_leg" ~ "Legal institutions",
        variable == "vars_pol" ~ "Political institutions",
        variable == "vars_publ" ~ "Public sector institutions",
        variable == "vars_social" ~ "Social institutions",
        variable == "vars_transp" ~ "Anti-Corruption, Transparency and Accountability institutions",
        variable == "vars_mkt" ~ "Business environment and trade institutions",
        variable == "vars_lab" ~ "Labor market institutions",
        variable == "vars_fin" ~ "Financial market institutions",
        variable == "vars_service_del" ~ "SOE Corporate Governance"
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
        variable %in% vars_transp | variable == "vars_transp" ~ "Anti-Corruption, Transparency and Accountability institutions",
        variable %in% vars_mkt | variable == "vars_mkt" ~ "Business environment and trade institutions",
        variable %in% vars_lab | variable == "vars_lab" ~ "Labor market institutions",
        variable %in% vars_fin | variable == "vars_fin" ~ "Financial market institutions",
        variable %in% vars_service_del | variable == "vars_service_del" ~ "SOE Corporate Governance"
      )
  ) %>%
  arrange(family_var)

# Save datasets ###########################################################################

  write_rds(variable_names,
            here("app",
                 "data",
                 "variable_names.rds"))

