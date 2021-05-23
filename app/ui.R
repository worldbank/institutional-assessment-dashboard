# Load packages ###########################################################################

  library(shiny)
  library(shinyBS)
  library(shinyWidgets)
  library(bslib)
  library(DT)
  library(shinyjs)
  library(plotly)
  library(tidyverse)

# Inputs ################################################################################

plot_height <- "500px"

# Data sets ---------------------------------------------------------------------------

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  variable_names <-
    read_rds(file.path("data",
                       "variable_names.rds"))

  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds"))

  # Auxiliary functions -----------------------------------------------------------------

  source(file.path("auxiliary",
                   "vars-by-family.R"))

# UI ###################################################################################

  ui <- navbarPage(

    "Global institutional assessment",

    theme = bs_theme(bootswatch = "minty"),

    # About page ---------------------------------------------
    tabPanel(
      "Home",
      useShinyjs(),

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),

      tags$div(
        class = "container",

        h3("About"),

        p("The World Bank recognizes institutional strengthening as key ingredient for progress of its members countries along income categories. While there are numerous diagnostic and assessment tools for specific functional areas such as financial management and tax administration, there is no analytical tool for country-level institutional assessment."),
        p("The Institutional Assessment (IA) benchmarking aims at partially filling this gap by providing a standard methodology to summarize information from a large set of institutional indicators. This dashboard offers a user-friendly interface that can easily and speedily be used for the country-level IA benchmarking."),
        p("The dashboard provides a visualization of a country’s profile based on a set of international institutional indicators, highlighting a given country’s institutional strengths and weaknesses. It is recommended to use these empirical results as guides for further quantitative or qualitative in-depth analysis in the specific areas of interest."),
        p("This benchmarking is part of a larger analytical effort.
          For full details about the project, see the Approach paper: Marco Larizza, Serena Sara Daniela Cocciolo, Eric Braian Arias and Peter Siegenthaler (forthcoming), ",
          tags$em("Country Level Institutional Assessment: a 3-steps analytical framework."),
          "Users of this resource should cite this approach paper.
          Further, any publications using data drawn from this resource could include citations to the original source(s) of the data used.
          Citation information for each component dataset is included in the methodology page."),

        h3("How to use this dashboard"),
        p("This dashboard aims to enable its users to interact with the country-level IA benchmarking in a few different ways:"),
        tags$ul(
          tags$li("The ",
                  tags$b("country benchmarking"),
                  "tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator."

          ),
          tags$li("The ",
                  tags$b("world map"),
                  "tab shows the closeness to frontier of a given indicator for all contries with available data."

          ),
          tags$li("The ",
                  tags$b("browse the data"),
                  "tab provides an interactive table containing the closeness to frontier data for all countries.
                  It also allows users to download the data in different formats."

          )
          ,
          tags$li("The ",
                  tags$b("methodology"),
                  "tab includes metadata on the indicators, country groups and methods used in the analysis."

          )
        ),

        HTML('<center><img src="wb-logo.jpg" width="200" style="padding-top:50px"></center>')

      )
    ),

    # Benchmarking tab ===================================================================================================
    tabPanel("Country benchmarking",
             id = "country",

             fluidRow(
               column(width = 1),

               column(width = 10,

                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     selectInput("country",
                                                 label = "Select a based country",
                                                 choices = c("", country_list$country_name %>% unique %>% sort),
                                                 selected = "Uruguay",
                                                 multiple = FALSE),

                                     checkboxGroupButtons(
                                       "groups",
                                       label = "Select a comparison group",
                                       choiceNames = country_groups$group_name,
                                       choiceValues = country_groups$group_code,
                                       selected = "OECD",
                                       checkIcon = list(
                                         yes = icon("ok",
                                                    lib = "glyphicon")),
                                       direction = "vertical",
                                       justified = TRUE,
                                       size = "sm"
                                      ),

                                     pickerInput(
                                       "countries",
                                       label = NULL,
                                       choices = global_data$country_name %>% unique %>% sort,
                                       multiple = TRUE,
                                       options = list(
                                        `count-selected-text` = "{0} countries selected",
                                        `selected-text-format` = "count > 5",
                                        #`actions-box` = TRUE,
                                        `live-search` = TRUE,
                                        size = 10)
                                     ),

                                     actionButton(
                                       "select",
                                       "Apply selection",
                                       icon = icon("check"),
                                       class = "btn-success",
                                       width = "100%"
                                     ),

                                     br(),br(),

                                     downloadButton("report",
                                                    "Download editable report",
                                                    style = "width:100%;"
                                     )
                        ),

                        mainPanel(width = 8,
                                  tabsetPanel(id = "tabsetpanel_id",
                                    tabPanel("Overview",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("overview", height = plot_height)
                                             )
                                    ),

                                    tabPanel("Labor market institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Labor", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('labor_def'))
                                    ),

                                    tabPanel("Financial institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Financial", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('fin_def'))
                                    ),

                                    tabPanel("Legal institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Legal", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('legal_def'))
                                    ),

                                    tabPanel("Political institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Political", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('political_def'))
                                    ),

                                    tabPanel("Social institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Social", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('social_def'))
                                    ),

                                    tabPanel("Business & Trade institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Trade", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('business_def'))
                                    ),

                                    tabPanel("Public sector institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Public", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('perf_def'))
                                    ),

                                    tabPanel("Institutions for service delivery",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Governance", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('serv_def'))
                                    ),

                                    tabPanel("Accountability institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Account", height = plot_height)
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('account_def'))
                                    )

                        )
                      )
               ),
               br(),
               br(),
               br()
             )
             )
    ),

    # Map tab ===============================================================================================================

    tabPanel("World map",
             id = "world_map",

             fluidRow(
               column(width = 1),

               column(width = 10,

                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          pickerInput(
                            "vars_map",
                            label = NULL,
                            choices = list(
                              `Family level` = c(sort(unique(variable_names$family_name))),
                              `Accountability institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_fin") %>% .$var_name),
                              `Business & trade institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_mkt") %>% .$var_name),
                              `Financial institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_fin") %>% .$var_name),
                              `Governance of SOEs` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_service_del") %>% .$var_name),
                              `Labor market institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_lab") %>% .$var_name),
                              `Legal institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_leg") %>% .$var_name),
                              `Political institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_pol") %>% .$var_name),
                              `Public sector institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_publ") %>% .$var_name),
                              `Social institutions` = c(variable_names %>% filter(var_level=="indicator" & family_var=="vars_social") %>% .$var_name)
                            ),
                            options = list(
                              `live-search` = TRUE,
                              size = 20,
                              title = "Select indicator"
                            ),
                            width = "100%"
                          )
                        ),

                        mainPanel(
                          width = 10,
                          plotlyOutput("map",
                                       height = "600px")
                        )
                      )
               )
             )
    ),

    # Data tab ===============================================================================================================

    tabPanel("Browse the data",

             fluidRow(
               column(width = 1),

               column(width = 10,

                      sidebarLayout(

                        sidebarPanel(id = "met_sidebar",
                                     width = 3,

                                     selectInput("data",
                                                 label = HTML("<b>Select a dataset</b>"),
                                                 choices = c("Closeness to frontier",
                                                             "Compiled indicators"),
                                                 multiple = FALSE),

                                     checkboxGroupInput("vars",
                                                        label = HTML("<b>Select institutional families</b>"),
                                                        choiceNames = c("Governance of SOEs",
                                                                        "Accountability institutions",
                                                                        "Business & trade institutions",
                                                                        "Financial institutions",
                                                                        "Labor market institutions",
                                                                        "Legal institutions",
                                                                        "Political institutions",
                                                                        "Public sector institutions  ",
                                                                        "Social institutions"
                                                        ),
                                                        choiceValues = family_names,
                                                        selected = family_names,
                                                        width = "100%"

                                     ),

                                     downloadButton("download_global_rds", "Download .rds"),
                                     br(),
                                     br(),
                                     downloadButton("download_global_csv", "Download .csv"),
                                     br(),
                                     br(),
                                     downloadButton("download_global_dta", "Download .dta")
                        ),

                        mainPanel(width = 9,
                                  dataTableOutput("benchmark_datatable")

                        )
                      )
               )
             )
    ),

    # Methodology tab ===============================================================================================================

    tabPanel("Methodology",

             fluidRow(
               column(width = 1),

               column(width = 10,

                      sidebarLayout(
                        sidebarPanel(id = "met_sidebar",
                                     style = "position:fixed; width:inherit; width:12%;",
                                     width = 2,

                                     p(tags$a(href = "#met_families",
                                              "Institutional families")),
                                     p(tags$a(href = "#met_ctf",
                                              "Closeness to frontier")),
                                     p(tags$a(href = "#met_benchmark",
                                              "Percentile analysis and comparator countries")),
                                     p(tags$a(href = "#def_country",
                                              "Country group definitions")),
                                     p(tags$a(href = "#def_indicators",
                                              "List of indicators"))
                        ),

                        mainPanel(width = 10,

                                  h3("Institutional families",
                                     id = "met_families",
                                     style = "padding-top: 0"),
                                  p("The dashboard uses established well-institutional indicators, clustered into nine main institutional families:",
                                    tags$ul(
                                      tags$li("Accountability institutions"),
                                      tags$li("Business & trade institutions"),
                                      tags$li("Financial institutions"),
                                      tags$li("Governance of SOEs"),
                                      tags$li("Labor market institutions"),
                                      tags$li("Legal institutions"),
                                      tags$li("Political institutions"),
                                      tags$li("Public sector institutions"),
                                      tags$li("Social institutions")
                                    )
                                  ),
                                  p("There is no agreed theoretical framework that could guide the categorization process. The proposed families are based on an effort to capture key functions that different institutions perform. In so doing, the categorization process faces a trade-off between aggregation and narrowness, where the categories ought to be broad enough to capture enough indicators and policy spaces, but narrow enough to guide a deep qualitative analysis as well as a fruitful and engaged conversation with the country."),
                                  p('All country-level indicators can be downloaded in the “Browse data” tab.'),


                                  h3("Closeness to frontier",
                                     id = "met_ctf"),
                                  p('The dashboard uses the “Closeness to Frontier” methodology, which is adapted from the Doing Business’s',
                                    a("Distance to Frontier methodology",
                                      href = "https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf"),
                                    '. The CTF methodology allows to assess country’s performance across institutional indicators by comparing it with the “global frontier”, where the global frontier is the world’s best performer. For each indicator, a country’s performance is rescaled on a 0-1 scale using the linear transformation (worst–y)/(worst–frontier), where 1 represents the best performer and 0 the worst performer. The higher the score, the closer a country is to the best performer and the lower the score, the closer a country is to the worst performer, and more distant to the frontier. The best and worst performers are identified using available data from the global sample (i.e., considering all countries for which data is available) since 2013. Thus, a country may set the frontier for an indicator even though it is no longer at the frontier in the most recent year for which the indicator is available.'),
                                  p('For each institutional family, the CTF scores obtained for each indicator are aggregated through simple averaging into one CTF score at family level. This captures the overall performance for an institutional family relatively to the “global frontier”, while the performance across the indicators will help identify the most challenging areas for institutional strengthening.'),


                                  h3("Percentile analysis and comparator countries",
                                     id = "met_benchmark"),
                                  p('The CTF scores compare the country’s performance with the best and worst performers at global level. However, how does the country compare relatively to a set of chosen comparators?'),
                                  p('The dashboard uses percentile distribution and traffic light coloring to capture the areas where the largest institutional gaps exist, ',
                                    HTML('<b>relative to the set of country comparators</b>'),
                                    '. Relative institutional weaknesses and strengths are defined based on the percentile in which each country indicator belongs. This methodology requires teams to make an informed decision on the set of comparator countries used for the benchmarking, since institutional weaknesses and strengths are identified relatively to those comparator countries.'),
                                  p('The “Closeness to Frontier” (length of the bar) and the percentile analysis (color of the bar) capture two related but different performance dimensions. The CTF compares the country’s performance with the best and worst performers. The percentile analysis benchmarks the country’s performance with all the set of other comparator countries. For example, it could be that for one indicator or institutional cluster the CTF score is relatively high and close to 1 (indicating in fact ‘closeness to the frontier’) but, at the same time, this dimension is marked as an institutional weakness (red coloring) because the country’s performance is still worse than the majority of comparator countries.'),
                                  p('The percentile analysis effectively drops those indicators whose distribution precludes this percentile classification (i.e., low variance).'),


                                  h3("Country group definitions",
                                     id = "def_country"),
                                  p(
                                    "Country group definitions are extracted from the",
                                    a(
                                      "World Bank Country and Lending Groups.",
                                      href = "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"
                                    ),
                                    "It classifies all 189 World Bank member countries, and all other economies with populations of more than 30,000.",
                                    "Economies are divided among income groups according to 2019 gross national income (GNI) per capita,",
                                    "calculated using the World Bank Atlas method."
                                  ),
                                  p(
                                    "The groups are:",
                                    tags$ul(
                                      tags$li(HTML("<b>Low income:</b> $1,035 or less")),
                                      tags$li(HTML("<b>Lower middle income:</b> $1,036 - 4,045")),
                                      tags$li(HTML("<b>Upper middle income:</b> $4,046 - 12,535")),
                                      tags$li(HTML("<b>High income:</b> $12,536 or more"))
                                    )
                                  ),
                                  p(
                                    HTML("The term <i>country</i>, used interchangeably with <i>economy,</i>"),
                                    "does not imply political independence but refers to any territory for which authorities report separate social or economic statistics.",
                                    "Income classifications set on 1 July 2020 remain in effect until 1 July 2021.",
                                    "Argentina, which was temporarily unclassified in July 2016 pending release of revised national accounts statistics,",
                                    "was classified as upper middle income for FY17 as of 29 September 2016 based on alternative conversion factors."
                                  ),


                                  h3("List of indicators",
                                     id = "def_indicators"),
                                  p("The indicators used to benchmark the institutional families are extracted from multiple public data sources.
                                    For a full list of the indicators used, their sources, and their definitions, download the metadata below."),
                                  downloadButton("download_indicators", "Download indicator definitions"),
                                  br(),
                                  br(),
                                  br()
                        )
                      )
               )
             )
    )


  ) # closing dashboard page
