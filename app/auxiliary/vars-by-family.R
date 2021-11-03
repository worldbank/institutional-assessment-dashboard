  # 1 POLITICAL INSTITUTIONS
  vars_pol <-
    c("e_fh_pr",
      "e_fh_cl",
      "e_p_polity",
      "f1_govpowers",
      "v2pepwrsoc",
      "v2pepwrses",
      "v2pepwrgen",
      "v2lgqugen",
      "f4_rights",
      "f3_security",
      "v2lgfemleg")

  # 2 SOCIAL INSTITUTIONS
  vars_social <-
    c("v2x_cspart",
      "v2dlengage",
      "v2xcs_ccsi",
      "trust_pol")

  # 3 ACCOUTABILITY INSTITUTIONS
  vars_transp <-
    c(
      "gtmi",
      #"e_ti_cpi",
      "f2_corruption",
      "favoritism",
      "bribes",
      "diversion_pfunds",
      "transparency_polmak",
      #"egovernmentindex",
      #"eparticipationindex",
      "f5_opengov",
      "rigorous_impartial_pa",
      "open_data_barometer")

  # 4 CENTER OF GOV/PUBLIC SECTOR INSTITUTIONS
  vars_publ <-
    c(
      "v2stfisccap",
      "v2stcritrecadm",
      "close2",
      "proff1",
      "f6_regulatoryenf",
      "f6_regulatoryenf",
      "proc_mean_score",
      "eff_govspending",
      "regulatory_governance",
      "centregov_mean"
    )

  # 5 LEGAL INSTITUTIONS
  vars_leg <-
    c("f8_criminaljustice",
      "f7_civiljustice",
      "es_court_constraint",
      "v2juaccnt",
      "efw_integrity_legalsys",
      "legaleff_challenging",
      "legaleff_disputes"#,
      #"enf_contr_overall",
      #"resolve_insolv_overall"
      )

  # 6 BUSINESS ENV. AND TRADE INSTITUTIONS
  vars_mkt <-
    c("govreg_burden",
      #"gci_overall",
      "mkt_dominance",
      "eff_antimonopoly",
      "nontariff_barriers",
      "property_rights",
      #"efw_tourist",
      "customs_burden",
      "lpi_clearance_eff",
      "wef_border_admin",
      "complexity_procedures",
      "barriers_startups",
      "protection_incumbents",
      "barriers_trade_expl",
      "barriers_trade_oth"#,
      #"start_bus_overall",
      #"constr_perm_overall",
      #"register_prop_overall",
      #"protect_minority_ov",
      #"pay_taxes_overall",
      #"trade_borders_overall"
      )

  # 7 LABOR MARKET INSTITUTIONS
  vars_lab <-
    c("efw_labor_mkt_reg",
      "collective_barg",
      "empl_protection_perm",
      "empl_protection_temp",
      "union_density",
      "minimum_wage_ratio")

  # 8 FINANCIAL INSTITUTITONS
  vars_fin <-
    c("efw_credit_mkt_reg",
      "efw_free_foreign_curr",
      "efw_capitalcontrols",
      "efw_inv_restr",
      #"competition_rules_fin",
      #"efficiency_superv_bank",
      #"efficiency_superv_fin",
      "cbi"#,
      #"access_credit_overall",
      #"insolvency_framework"
      )

  # 9 SOE Governance/SERVICE DELIVERY INSTITUTIONS
  vars_service_del <-
    c(
      "scopeofstateownedenterprises",
      "governmentinvolvementinnetworkse",
      "directcontroloverbusinessenterpr",
      #"governanceofstateownedenterprise",
      #"useofcommandcontrolregulation",
      "governance_soe",
      "price_controls",
      "command_control"
      )

    vars_all <-
      c(vars_service_del,
        vars_transp,
        vars_mkt,
        vars_fin,
        vars_lab,
        vars_leg,
        vars_pol,
        vars_publ,
        vars_social
      )

    family_names <-
      c("vars_service_del",
        "vars_transp",
        "vars_mkt",
        "vars_fin",
        "vars_lab",
        "vars_leg",
        "vars_pol",
        "vars_publ",
        "vars_social")
