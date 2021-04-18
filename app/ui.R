# Load packages ###########################################################################

  library(shiny)
  library(shinyBS)
  library(bslib)
  library(DT)
  library(shinyjs)
  library(plotly)
  library(tidyverse)

# Inputs ################################################################################

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

        h3("How to use this dashboard"),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit")

      )
    ),

    # Benchmarking tab ===================================================================================================
    tabPanel("Country benchmarking",

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


                                     checkboxGroupInput("groups",
                                                        label = "Select a comparison group",
                                                        choiceNames = country_groups$group_name,
                                                        choiceValues = country_groups$group_code,
                                                        selected = "OED"),

                                     actionButton(
                                       "select",
                                       "Apply selection",
                                       icon = icon("check"),
                                       class = "btn-success",
                                       style="color: #fff"
                                     )
                        ),

                        mainPanel(width = 8,
                                  tabsetPanel(
                                    tabPanel("Overview",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("overview")
                                             )
                                    ),

                                    tabPanel("Labor market institutions",
                                             conditionalPanel("input.select !== 0",
                                                              plotlyOutput("Labor")
                                             ),
                                             bsCollapsePanel("See indicator definitions",
                                                             tableOutput('labor_def'))
                                    )
                        )
                      )
               )
             )
             )
    ),

    # Map tab ===============================================================================================================

    tabPanel("World map",

             fluidRow(
               column(width = 1),

               column(width = 10,

                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          selectInput("vars_map",
                                      label = "Select indicator to display",
                                      choice = c("", sort(variable_names$var_name)),
                                      width = "100%")
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
                                     downloadButton("download_global_csv", "Download .csv"),
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
                                      tags$li("Political institutions"),
                                      tags$li("Social institutions"),
                                      tags$li("Percentile analysis and comparator countries"),
                                      tags$li("Country group definitions")
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
                                  p("Country group definitions are extracted from the"),
                                  downloadButton("download_indicators", "Download indicator definitions"),
                        )
                      )
               )
             )
    )


  ) # closing dashboard page
