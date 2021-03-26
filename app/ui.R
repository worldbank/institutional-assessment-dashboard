

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(DT)
  library(plotly)

  country_list <-
    read_csv(file.path("data",
                       "wb_country_list.csv"))

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

      tabItems(
        tabItem(tabName = "home",
                  box(solidHeader = TRUE,
                      width = 9,
                      h3("About"),
                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                        Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum."),
                      h3("How to use this dashboard"),
                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                        Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum."),
                      h3("Methodological notes"),
                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                        sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                        quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                        Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                        Excepteur sint occaecat cupidatat non proident,
                        sunt in culpa qui officia deserunt mollit anim id est laborum.")
                  ),

                box(title = "Select countries",
                    solidHeader = TRUE,
                    width = 9,
                    box(title = "Select a base country",
                      selectInput(
                        "country",
                        label = NULL,
                        choices = c("", country_list),
                        multiple = FALSE,
                        width = "50%"
                      )
                    ),
                    box(title = "Select comparison group",
                        width = 12,
                        box(title = "Regions",
                            collapsible = TRUE),
                        box(title = "Country groups",
                            collapsible = TRUE),
                        box(title = "Countries",
                            collapsible = TRUE,
                            collapsed = TRUE,
                            width =12),
                        actionButton("button", "Apply selection")
                        )
                )
        ),

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

        tabItem(tabName = "data",
                box(solidHeader = TRUE,
                    width = 12,
                    title = "Browse the data",
                    dataTableOutput("dataset")
                )
        )
      )
    )
  )
