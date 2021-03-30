

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(DT)
  library(plotly)

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))


  source(file.path("auxiliary",
                   "vars-by-family.R"))



  ui <- dashboardPage(

   # Header ------------------------------------------------------------------------------------
    dashboardHeader(title = h3("Title"),
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

                fluidRow(
                  box(
                    solidHeader = TRUE,
                    width = 12,
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
                      multiple = FALSE
                    )
                  ),

                  box(
                    solidHeader = TRUE,
                    width = 9,
                    title = "Select comparison group",
                    collapsible = TRUE,
                    class = "multicol-3",
                    checkboxGroupInput(
                      "groups",
                      label = NULL,
                      choiceNames = country_groups$group_name,
                      choiceValues = country_groups$group_code
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

                actionButton("select",
                             "Apply selection")

        ),

      # Country overview tab -----------------------------------------------------------------------
      tabItem(tabName = "overview",
              box(solidHeader = TRUE,
                  width = 9,
                  title = "Country institutional assessment",
                  plotlyOutput("Overview",
                               width = "65%"),
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
                  width = 9,
                  title = "Labor market institutions",
                  plotlyOutput("Labor",
                               width = "65%"),
                  h5("About the data and indicators"),
                  p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                        Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum.")
              )
      ),

      # Data tab ----------------------------------------------------------------------------------
      tabItem(tabName = "data",

              box(solidHeader = TRUE,
                  width = 3,
                  title = "Select countries to display",
                  collapsible = TRUE,
                  collapsed = TRUE,
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
                  collapsed = TRUE,
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
                    inline = TRUE,
                    selected = family_names,
                    width = "100%"
                  )
              ),



              box(solidHeader = TRUE,
                  width = 12,
                  title = "Browse the data",
                  dataTableOutput("dataset")
              )
        ),

      # Methodology tab ---------------------------------------------------------------------------
      tabItem(tabName = "methodology",

              box(solidHeader = TRUE,
                  width = 9,
                  title = "Country group definitions",
                  collapsible = TRUE,
                  collapsed = TRUE,
                  p(
                    paste(
                      "Country group definitions are extracted from the",
                      a(
                        "World Bank Country and Lending Groups.",
                        href = "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"
                      ),
                      "It classifies all 189 World Bank member countries, and all other economies with populations of more than 30,000.",
                      "Economies are divided among income groups according to 2019 gross national income (GNI) per capita,",
                      "calculated using the World Bank Atlas method."
                    )
                  )
                  # ),
                  #         "The groups are:"
                  #         tags$ul(
                  #           tags$li("<b>Low income:</b> $1,035 or less"),
                  #           tags$li("<b>Lower middle income:</b> $1,036 - 4,045"),
                  #           tags$li("<b>Upper middle income:</b> $4,046 - 12,535"),
                  #           tags$li("<b>High income:</b> $12,536 or more")
                  #         ),
                  #         "The term <i>country</i>, used interchangeably with <i>economy,</i>",
                  #         "does not imply political independence but refers to any territory for
                  #         which authorities report separate social or economic statistics.",
                  #         "Income classifications set on 1 July 2020 remain in effect until 1 July 2021.",
                  #         "Argentina, which was temporarily unclassified in July 2016 pending release of revised national accounts statistics,",
                  #         "was classified as upper middle income for FY17 as of 29 September 2016 based on alternative conversion factors.")
                  # )
              )
      )


    # Closing parenthesis -------------------------------------------------------------------------

      ) # Closing tab item
    ) # Closing dashboard body
  ) # closing dashboard page
