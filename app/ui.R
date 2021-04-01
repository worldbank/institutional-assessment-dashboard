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
                       "variable_names.rds"))

  source(file.path("auxiliary",
                   "vars-by-family.R"))

  ui <- dashboardPage(

   # Header ------------------------------------------------------------------------------------
    dashboardHeader(title = h3("Global Institutional Assessment"),
                    titleWidth = 1800),

    # Sidebar -----------------------------------------------------------------------------------
    dashboardSidebar(
      width = "18%",

      sidebarMenu(menuItem("Home",
                           tabName = "home",
                           icon = icon("home")),
                  menuItem("Country overview",
                           tabName = "overview",
                           icon = icon("globe")),
                  menuItem("Labor",
                           tabName = "labor",
                           icon = icon("building")),
                  menuItem("Financial",
                           tabName = "financial",
                           icon = icon("money-bill")),
                  menuItem("Legal ",
                           tabName = "legal",
                           icon = icon("balance-scale")),
                  menuItem("Political",
                           tabName = "political",
                           icon = icon("vote-yea")),
                  menuItem("Social",
                           tabName = "social",
                           icon = icon("fist-raised")),
                  menuItem("Business & Trade",
                           tabName = "trade",
                           icon = icon("comments-dollar")),
                  menuItem("Public sector",
                           tabName = "public",
                           icon = icon("university")),
                  menuItem("Governance of SOEs",
                           tabName = "governance",
                           icon = icon("hand-holding-usd")),
                  menuItem("Accountability",
                           tabName = "account",
                           icon = icon("receipt")),
                  menuItem("Map",
                           tabName = "map",
                           icon = icon("map")),
                  menuItem("Browse the data",
                           tabName = "data",
                           icon = icon("database")),
                  menuItem("Methodological notes",
                           tabName = "methodology",
                           icon = icon("sticky-note"))
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

                fluidRow(
                  box(
                    solidHeader = TRUE,
                    width = box_width,
                    h3("About"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit"),
                    h3("How to use this dashboard"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit"),
                    h3("Methodological notes"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit.")
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
                  collapsible = TRUE,
                  collapsed = TRUE,
                  class = "multicol-8",
                  width = "100%",
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
                               height = graph_height),
                  h5("About the data and indicators"),
                  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                        Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum.")
              )
      ),

      # Labor market institutions tab --------------------------------------------------------------
      tabItem(tabName = "labor",
              box(solidHeader = TRUE,
                  width = box_width,
                  title = "Labor market institutions",
                  plotlyOutput("Labor",
                               width = graph_width,
                               height = graph_height),
                  h5("About the data and indicators"),
                  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                        Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum.")
              )
      ),
        # Financial institutions tab --------------------------------------------------------------
        tabItem(tabName = "financial",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Financial institutions",
                    plotlyOutput("Financial",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                Excepteur sint occaecat cupidatat non proident,
                                sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Legal institutions tab --------------------------------------------------------------
        tabItem(tabName = "legal",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Legal institutions",
                    plotlyOutput("Legal",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Political institutions tab --------------------------------------------------------------
        tabItem(tabName = "political",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Political institutions",
                    plotlyOutput("Political",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Social institutions tab --------------------------------------------------------------
        tabItem(tabName = "social",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Social institutions",
                    plotlyOutput("Social",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Business & Trade institutions tab --------------------------------------------------------------
        tabItem(tabName = "trade",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Business & Trade institutions",
                    plotlyOutput("Trade",
                                 width = graph_width,
                                 height = "600px"),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Public sector institutions tab --------------------------------------------------------------
        tabItem(tabName = "public",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Public sector institutions",
                    plotlyOutput("Public",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Governance of SOEs tab --------------------------------------------------------------
        tabItem(tabName = "governance",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Governance of SOEs",
                    plotlyOutput("Governance",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
        ),

        # Accountability institutions tab --------------------------------------------------------------
        tabItem(tabName = "account",
                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Accountability institutions",
                    plotlyOutput("Account",
                                 width = graph_width,
                                 height = graph_height),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                                      sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                                      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                                      Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                                      Excepteur sint occaecat cupidatat non proident,
                                      sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )
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
                      choices = c("",sort(variable_names$var_name)),
                      width = "100%"
                    )
                ),

                box(solidHeader = TRUE,
                    width = box_width,
                    title = "Map",
                    leafletOutput("map_plot", height="600")
                    #h5("About the data and indicators"),
                    #p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                    #                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                    #                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                    #                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                    #                        Excepteur sint occaecat cupidatat non proident,
                    #                        sunt in culpa qui officia deserunt mollit anim id est laborum.")
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

                box(solidHeader = TRUE,
                    width = 3,
                    title = "Select countries to display",
                    collapsible = TRUE,
                    selectInput(
                      "vars",
                      label = NULL,
                      choices = c("All",
                                  "Current comparison"
                      ),
                      selected = 1,
                      width = "100%"
                    )
                ),

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

                box(solidHeader = TRUE,
                    width = 4,
                    title = "Country group definitions",
                    collapsible = TRUE,
                    collapsed = TRUE,
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
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Accountability institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('account_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Business environemt and trade institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('business_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Financial institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('fin_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Sevices delivery institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('serv_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Labor market institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('labor_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Legal institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('legal_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Political institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('political_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Public sector performance institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('perf_def')
                ),

                box(solidHeader = TRUE,
                    width = 12,
                    title = "Social institutions",
                    collapsible = TRUE,
                    collapsed = TRUE,
                    tableOutput('social_def')
                )


        )


    # Closing parenthesis -------------------------------------------------------------------------

      ) # Closing tab item
    ) # Closing dashboard body
  ) # closing dashboard page
