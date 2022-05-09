  # 1 POLITICAL INSTITUTIONS
  vars_pol <-
    c(
      "e_p_polity",
      "f1_govpowers",
      "f3_security",
      "f4_rights",
      "v2lgfemleg",
      "v2lgqugen",
      "v2pepwrgen",
      "v2pepwrses",
      "v2pepwrsoc",
      "e_fh_cl",
      "e_fh_pr"
    )
  
  # 2 SOCIAL INSTITUTIONS
  vars_social <-
    c(
      "opinion_freedom",
      "trust_pol",
      "v2clacfree",
      "v2cseeorgs",
      "v2dlengage",
      "v2x_cspart",
      "v2xcs_ccsi"
    )
  
  # 3 ACCOUTABILITY INSTITUTIONS
  vars_transp <-
    c(
      "bribes",
      "complaint_mechan",
      "diversion_pfunds",
      "f2_corruption",
      "f5_opengov",
      "right_to_info",
      "v2clrspct", #This variable contains all missings for the year 2014-16
      # "rigorous_impartial_pa",
      "transparency_polmak"
    )
  
  # 4 CENTER OF GOV/PUBLIC SECTOR INSTITUTIONS
  vars_publ <-
    c(
      "f6_regulatoryenf",
      "eff_govspending",
      "favoritism",
      "v2stcritrecadm"
    )

  # 5 LEGAL INSTITUTIONS
  vars_leg <-
    c(
      # "es_court_constraint",
      "f7_civiljustice",
      "f8_criminaljustice",
      "legaleff_challenging",
      "legaleff_disputes",
      "v2juaccnt"#,
      # "efw_integrity_legalsys"
      )

  # 6 BUSINESS ENV. AND TRADE INSTITUTIONS
  vars_mkt <-
    c(
      "customs_burden",
      "eff_antimonopoly",
      "lpi_clearance_eff",
      "mkt_dominance",
      "efw_tourist",
      "property_rights"#,
      # "wef_border_admin"
      )
  
  # 7 LABOR MARKET INSTITUTIONS
  vars_lab <-
    c(
      "efw_labor_mkt_reg"
    )

  # 8 FINANCIAL INSTITUTITONS
  vars_fin <-
    c(
      "efw_capitalcontrols",
      "efw_credit_mkt_reg",
      "efw_free_foreign_curr",
      "efw_inv_restr"
    )

  # 9 SOE Governance/SERVICE DELIVERY INSTITUTIONS
  vars_service_del <-
    c(
      # "scopeofstateownedenterprises",
      # "governmentinvolvementinnetworkse",
      # "directcontroloverbusinessenterpr",
      #"governanceofstateownedenterprise",
      #"useofcommandcontrolregulation",
      # "governance_soe",
      # "price_controls",
      # "command_control",
      # "soe_board",
      #"soe_corporate",
      # "soe_government",
      # "soe_annual_report",
      # "soe_financial",
      # "soe_report_legislative"
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

    # From Excel file
    # vars_all <-
    # c("bribes",
    # "complaint_mechan",
    # "diversion_pfunds",
    # "f2_corruption",
    # "f5_opengov",
    # "right_to_info",
    # "rigorous_impartial_pa",
    # "transparency_polmak",
    # 
    # "customs_burden",
    # "eff_antimonopoly",
    # "lpi_clearance_eff",
    # "mkt_dominance",
    # "property_rights",
    # "wef_border_admin",
    # 
    # # "es_court_constraint",
    # "f7_civiljustice",
    # "f8_criminaljustice",
    # "legaleff_challenging",
    # "legaleff_disputes",
    # "v2juaccnt",
    # 
    # "hiring_pract",
    # 
    # "e_p_polity",
    # "f1_govpowers",
    # "f3_security",
    # "f4_rights",
    # "v2lgfemleg",
    # "v2lgqugen",
    # "v2pepwrgen",
    # "v2pepwrses",
    # "v2pepwrsoc",
    # 
    # "f6_regulatoryenf",
    # "favoritism",
    # "v2stcritrecadm",
    # 
    # "opinion_freedom",
    # "trust_pol",
    # "v2clacfree",
    # "v2cseeorgs",
    # "v2dlengage",
    # "v2x_cspart",
    # "v2xcs_ccsi")
