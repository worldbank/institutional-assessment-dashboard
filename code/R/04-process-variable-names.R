# Packages ===================================================================================

packages <-
  c("tidyverse",
    "tidyr",
    "here")

pacman::p_load(packages,
               character.only = TRUE)

# Inputs =====================================================================================
variable_names <-
  data.frame(
    names = c(
        # Family names
        "vars_leg; Legal institutions",
        "vars_pol; Political institutions",
        "vars_publ; Public sector institutions",
        "vars_social; Social institutions",
        "vars_transp; Accountability institutions",
        "vars_mkt; Business & trade institutions",
        "vars_lab; Labor market institutions",
        "vars_fin; Financial institutions",
        "vars_service_del; Governance of SOEs",
        # Variables
        "centregov_mean; Centre of Government, influence",
        "nontariff_barriers; Non-tariffs trade barriers",
        "property_rights; Property rights",
        "protection_incumbents; Protection of incumbents",
        "complexity_procedures; Complex regulatory procedures",
        "barriers_startups; Admin burdens on start-ups",
        "barriers_trade_expl; Explicit barriers to trade",
        "barriers_trade_oth; Other barriers to trade",
        "regulatory_governance; Regulatory governance",
        "open_data_barometer; Open Data Barometer",
        "rigorous_impartial_pa; Rigorous and impartial PA",
        "legaleff_challenging; Challenging regulations",
        "legaleff_disputes; Settling disputes",
        "insolvency_framework; Insolvency framework, strength",
        "financial_institution; Financial Institutions",
        "access_credit_overall; Getting credit",
        "competition_rules_fin; Competition regulation",
        "efficiency_superv_bank; Supervision efficiency-banking",
        "efficiency_superv_fin; Supervision efficiency-financial",
        "minimum_wage_ratio; Minimum to mean wage ratio",
        "union_density; Union density",
        "empl_protection_perm; Employment protection, regular",
        "empl_protection_temp; Employment protection, temp.",
        "collective_barg; Collective bargaining coverage",
        "govreg_burden; Burden of gov. regulation",
        "efw_labor_mkt_reg; Labor market regulations",
        "wsj_financialfreedom; Financial freedom",
        "wsj_businessfreedom; Business freedom",
        "wsj_propertyrights; Property rights",
        "efw_property_rights; Property rights protection",
        "efw_reg_trade_barr; Regulatory trade barriers",
        "efw_controls_movement; Control capital/people movs",
        "efw_businessreg; Business regulations",
        "efw_credit_mkt_reg; Credit market regulations",
        "efw_free_foreign_curr; Freedom, foreign bank accounts",
        "f3_security; Order and security",
        "trust_pol; Public trust in politician",
        "v2dlengage; Engaged society",
        "f2_corruption; Absence of corruption",
        "e_ti_cpi; Perception of corruption",
        "f4_rights; Fundamental rights",
        "v2lgqugen; Lower chamber gender quota",
        "v2pepwrsoc; Power by social group",
        "v2pepwrses; Power by socioeconomic position",
        "f1_govpowers; Constraints on Gov. Powers",
        "e_fh_pr; Political rights",
        "e_fh_cl; Civil liberties",
        "e_p_polity; Polity IV score",
        "f8_criminaljustice; Criminal justice",
        "f7_civiljustice; Civil justice",
        "f6_regulatoryenf; Regulatory enforcement",
        "f5_opengov; Open government",
        "dskills_value_2018; Digital skills",
        "onlineserviceindex; Egov, Online service",
        "humancapitalindex; Egov, Human capital",
        "telecom_infr; Egov, Telecom infr",
        "prebid_score; Procurement-pre-bid",
        "bid_submission_score; Procurement-submission",
        "evaluation_score; Procurement-evaluation",
        "contract_manag_score; Procurement-contract mgmt",
        "perf_guarantee_score; Procurement-performance gntee",
        "payments_score; Procurement-payment",
        "proc_mean_score; Procurement score",
        "val_direct_contracts_sh; Share of direct contracts (value)",
        "wgi_voice_acc; Voice and accountability",
        "wgi_pol_stability; Political stability",
        "wgi_gov_effective; Government effectiveness",
        "wgi_regulatory; Regulatory quality",
        "wgi_rulelaw; Rule of law",
        "wgi_control_corr; Control of corruption",
        "v2x_cspart; Civil society participation",
        "v2pepwrgen; Power by gender",
        "cbi; Central bank independence"
      )
  ) %>%
  tidyr::separate(names, into = c("variable","name_variable"), sep=";")


# Save datasets ###########################################################################

  write_rds(variable_names,
            here("app",
                 "auxiliary",
                 "variable_names.rds"))
