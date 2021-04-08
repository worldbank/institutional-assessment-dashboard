  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(leaflet)

  box_width <- 11
  graph_width <- "70%"
  graph_height <- "500px"

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  variable_names <-
    read_rds(file.path("data",
                       "variable_names.rds")) %>%
    arrange(var_name)

  source(file.path("auxiliary",
                   "vars-by-family.R"))

  ui <- dashboardPage(

    dashboardHeader(title = h3("Global Institutional Assessment"),
                    titleWidth = 1800),

    # Sidebar -----------------------------------------------------------------------------------
    dashboardSidebar(
      width = "18%",

      sidebarMenu(id = "tab",
                  menuItem("Home",
                           tabName = "home",
                           icon = icon("home")),
                  menuItem("Methodology",
                           tabName = "methodology",
                           icon = icon("sticky-note")),
                  menuItem("Country overview",
                           tabName = "overview",
                           icon = icon("globe")),
                  menuItem("Labor market institutions",
                           tabName = "labor",
                           icon = icon("building")),
                  menuItem("Financial institutions",
                           tabName = "financial",
                           icon = icon("money-bill")),
                  menuItem("Legal institutions",
                           tabName = "legal",
                           icon = icon("balance-scale")),
                  menuItem("Political institutions",
                           tabName = "political",
                           icon = icon("vote-yea")),
                  menuItem("Social institutions",
                           tabName = "social",
                           icon = icon("fist-raised")),
                  menuItem("Business & trade institutions",
                           tabName = "trade",
                           icon = icon("comments-dollar")),
                  menuItem("Public sector institutions",
                           tabName = "public",
                           icon = icon("university")),
                  menuItem("Governance of SOEs institutions" ,
                           tabName = "governance",
                           icon = icon("hand-holding-usd")),
                  menuItem("Accountability institutions",
                           tabName = "account",
                           icon = icon("receipt")),
                  menuItem("Map",
                           tabName = "map",
                           icon = icon("map")),
                  menuItem("Browse the data",
                           tabName = "data",
                           icon = icon("database"))
                  )
    ),

    # Body -----------------------------------------------------------------------------------------
    dashboardBody(

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),

      tabItems(

      # Home tab ----------------------------------------------------------------------------------
        tabItem(tabName = "home",
                useShinyjs(),


                fluidRow(
                  box(
                    solidHeader = TRUE,
                    width = box_width,

                    h3("About"),

                    p("The World Bank recognizes institutional strengthening as key ingredient for progress of its members countries along income categories. While there are numerous diagnostic and assessment tools for specific functional areas such as financial management and tax administration, there is no analytical tool for country-level institutional assessment."),
                    p("The Institutional Assessment (IA) benchmarking aims at partially filling this gap by providing a standard methodology to summarize information from a large set of institutional indicators. This dashboard offers a user-friendly interface that can easily and speedily be used for the country-level IA benchmarking."),
                    p("The dashboard provides a visualization of a country’s profile based on a set of international institutional indicators, highlighting a given country’s institutional strengths and weaknesses. It is recommended to use these empirical results as guides for further quantitative or qualitative in-depth analysis in the specific areas of interest."),

                    h3("How to use this dashboard"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit")
                  )
                ),

                fluidRow(
                  box(
                    solidHeader = TRUE,
                    width = 3,
                    title = "Select a base country",
                    selectInput(
                      "country",
                      label = NULL,
                      choices = c("", country_list$country_name %>% unique %>% sort),
                      selected = "Uruguay",
                      multiple = FALSE
                    )
                  ),

                  box(
                    solidHeader = TRUE,
                    width = box_width - 3,
                    title = "Select comparison group",
                    collapsible = TRUE,
                    class = "multicol-3",
                    checkboxGroupInput(
                      "groups",
                      label = NULL,
                      choiceNames = country_groups$group_name,
                      choiceValues = country_groups$group_code,
                      selected = "OED"
                    )
                  )
                ),

                box(
                  solidHeader = TRUE,
                  title = "All countries",
                  width = box_width,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  class = "multicol-7",
                  selected = "NULL",
                  checkboxGroupInput(
                    "countries",
                    label = NULL,
                    choices = country_list$country_name %>% unique
                  )
                ),

                actionButton(
                  "select",
                  "Apply selection",
                  icon = icon("check"),
                  class = "btn-success",
                  style="color: #fff"
                )

        ),

      # Country overview tab -----------------------------------------------------------------------
      tabItem(tabName = "overview",
              box(solidHeader = TRUE,
                  width = box_width,
                  title = "Country institutional assessment",
                  plotlyOutput("overview",
                               width = graph_width,
                               height = graph_height)
              )
      ),

      # Labor market institutions tab --------------------------------------------------------------
      tabItem(tabName = "labor",
              box(solidHeader = TRUE,
                  width = box_width,
                  title = "Labor market institutions",
                  plotlyOutput("Labor",
                               width = graph_width,
                               height = graph_height)
              ),

              box(solidHeader = TRUE,
                  width = box_width,
                  title = "Indicator definitions",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  tableOutput('labor_def')
              ),
      ),
        # Financial institutions tab --------------------------------------------------------------
        tabItem(tabName = "financial",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Financial institutions",
                    plotlyOutput("Financial",
                                 width = graph_width,
                                 height = graph_height)
                ),

                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('fin_def')
                ),
        ),

        # Legal institutions tab --------------------------------------------------------------
        tabItem(tabName = "legal",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Legal institutions",
                    plotlyOutput("Legal",
                                 width = graph_width,
                                 height = graph_height)
                ),
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('legal_def')
                )
        ),

        # Political institutions tab --------------------------------------------------------------
        tabItem(tabName = "political",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Political institutions",
                    plotlyOutput("Political",
                                 width = graph_width,
                                 height = graph_height)
                ),
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('political_def')
                )
        ),

        # Social institutions tab --------------------------------------------------------------
        tabItem(tabName = "social",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Social institutions",
                    plotlyOutput("Social",
                                 width = graph_width,
                                 height = graph_height)
                ),
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('social_def')
                )
        ),

        # Business & Trade institutions tab --------------------------------------------------------------
        tabItem(tabName = "trade",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Business environment and trade institutions",
                    plotlyOutput("Trade",
                                 width = graph_width,
                                 height = "600px")
                ),
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('business_def')
                )
        ),

        # Public sector institutions tab --------------------------------------------------------------
        tabItem(tabName = "public",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Center of government and public sector institutions",
                    plotlyOutput("Public",
                                 width = graph_width,
                                 height = graph_height)
                ),
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('perf_def')
                )
        ),

        # Governance of SOEs tab --------------------------------------------------------------
        tabItem(tabName = "governance",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Governance of SOEs and service delivery institutions",
                    plotlyOutput("Governance",
                                 width = graph_width,
                                 height = graph_height)
                ),

                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('serv_def')
                )
        ),

        # Accountability institutions tab --------------------------------------------------------------
        tabItem(tabName = "account",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Accountability institutions",
                    plotlyOutput("Account",
                                 width = graph_width,
                                 height = graph_height)
                ),
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Indicator definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('account_def')
                ),

        ),

        # World map tab --------------------------------------------------------------
        tabItem(tabName = "map",

                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Select indicator to display",
                    collapsible = TRUE,
                    selectInput(
                      "vars_map",
                      label = NULL,
                      choices = c("", variable_names$var_name),
                      width = "100%"
                    )
                ),

                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Map",
                    leafletOutput("map_plot", height="600")
                )
        ),

      # Data tab ----------------------------------------------------------------------------------
      tabItem(tabName = "data",

              fluidRow(

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Browse the data",
                    dataTableOutput("dataset")
                ),

                # box(solidHeader = TRUE,
                #     width = 3,
                #     title = "Select countries to display",
                #     collapsible = TRUE,
                #     selectInput(
                #       "vars",
                #       label = NULL,
                #       choices = c("All",
                #                   "Current comparison"
                #       ),
                #       selected = 1,
                #       width = "100%"
                #     )
                # ),

                box(solidHeader = TRUE,
                    width = 9,
                    title = "Select indicators to display",
                    collapsible = TRUE,
                    class = "multicol-5",
                    checkboxGroupInput(
                      "vars",
                      label = NULL,
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
                    )
                )

              )
        ),

      # Methodology tab ---------------------------------------------------------------------------
        tabItem(tabName = "methodology",

              fluidRow(

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Institutional families",
                    collapsible = TRUE,
                    p("The dashboard uses established well-institutional indicators, clustered into nine main institutional families:",
                      tags$ul(
                        tags$li("Political institutions"),
                        tags$li("Social institutions"),
                        tags$li("Accountability institutions"),
                        tags$li("Center of government and public sector institutions"),
                        tags$li("Legal institutions"),
                        tags$li("Business environment and trade institutions"),
                        tags$li("Labor market institutions"),
                        tags$li("Financial institutions"),
                        tags$li("Governance of SOEs and service delivery institutions")
                      )
                    ),
                    p("There is no agreed theoretical framework that could guide the categorization process. The proposed families are based on an effort to capture key functions that different institutions perform. In so doing, the categorization process faces a trade-off between aggregation and narrowness, where the categories ought to be broad enough to capture enough indicators and policy spaces, but narrow enough to guide a deep qualitative analysis as well as a fruitful and engaged conversation with the country."),
                    p('All country-level indicators can be downloaded in the “Browse data” tab.')

                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Closeness to Frontier",
                    collapsible = TRUE,
                    p('The dashboard uses the “Closeness to Frontier” methodology, which is adapted from the Doing Business’s',
                      a("Distance to Frontier methodology",
                      href = "https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf"),
                      '. The CTF methodology allows to assess country’s performance across institutional indicators by comparing it with the “global frontier”, where the global frontier is the world’s best performer. For each indicator, a country’s performance is rescaled on a 0-1 scale using the linear transformation (worst–y)/(worst–frontier), where 1 represents the best performer and 0 the worst performer. The higher the score, the closer a country is to the best performer and the lower the score, the closer a country is to the worst performer, and more distant to the frontier. The best and worst performers are identified using available data from the global sample (i.e., considering all countries for which data is available) since 2013. Thus, a country may set the frontier for an indicator even though it is no longer at the frontier in the most recent year for which the indicator is available.'),
                    p('For each institutional family, the CTF scores obtained for each indicator are aggregated through simple averaging into one CTF score at family level. This captures the overall performance for an institutional family relatively to the “global frontier”, while the performance across the indicators will help identify the most challenging areas for institutional strengthening.')

                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Percentile analysis and comparator countries",
                    collapsible = TRUE,
                    p('The CTF scores compare the country’s performance with the best and worst performers at global level. However, how does the country compare relatively to a set of chosen comparators?'),
                    p('The dashboard uses percentile distribution and traffic light coloring to capture the areas where the largest institutional gaps exist, ',
                      HTML('<b>relative to the set of country comparators</b>'),
                      '. Relative institutional weaknesses and strengths are defined based on the percentile in which each country indicator belongs. This methodology requires teams to make an informed decision on the set of comparator countries used for the benchmarking, since institutional weaknesses and strengths are identified relatively to those comparator countries.'),
                    p('The “Closeness to Frontier” (length of the bar) and the percentile analysis (color of the bar) capture two related but different performance dimensions. The CTF compares the country’s performance with the best and worst performers. The percentile analysis benchmarks the country’s performance with all the set of other comparator countries. For example, it could be that for one indicator or institutional cluster the CTF score is relatively high and close to 1 (indicating in fact ‘closeness to the frontier’) but, at the same time, this dimension is marked as an institutional weakness (red coloring) because the country’s performance is still worse than the majority of comparator countries.'),
                    p('The percentile analysis effectively drops those indicators whose distribution precludes this percentile classification (i.e., low variance).')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Country group definitions",
                    collapsible = TRUE,
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
                      "does not imply political independence but refers to any territory for
                      which authorities report separate social or economic statistics.",
                      "Income classifications set on 1 July 2020 remain in effect until 1 July 2021.",
                      "Argentina, which was temporarily unclassified in July 2016 pending release of revised national accounts statistics,",
                      "was classified as upper middle income for FY17 as of 29 September 2016 based on alternative conversion factors."
                    )
                )
          )
        )


    # Closing parenthesis -------------------------------------------------------------------------

      ) # Closing tab item
    ) # Closing dashboard body
  ) # closing dashboard page
