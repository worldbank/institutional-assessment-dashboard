

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(DT)

  country_list <-
    read_csv("data/wb_country_list.csv")

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
                  menuItem("Finance",
                           tabName = "fin",
                           icon = icon("chart-line")),
                  menuItem("Labor",
                           tabName = "labor",
                           icon = icon("building")),
                  menuItem("Law",
                           tabName = "law",
                           icon = icon("gavel"))
                  ),
      headerPanel(""),
      headerPanel(""),
      selectInput(
        "country",
        label = "Select a country",
        choices = c("", country_list),
        multiple = FALSE
      ),
      headerPanel(""),
      selectInput(
        "comparison",
        label = "Select a comparison group",
        choices = c("", "OECD", "Latin America & the Caribbean"),
        multiple = FALSE
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
                  )
        ),
        tabItem(tabName = "overview",
                box(solidHeader = TRUE,
                    width = 9,
                    title = "Country institutional assessment",
                    plotOutput("Overview",
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
        tabItem(tabName = "fin",
               box(solidHeader = TRUE,
                   width = 9,
                   title = "Financial institutions",
                   plotOutput("Finance",
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
                    plotOutput("Labor",
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
        tabItem(tabName = "law",
                box(solidHeader = TRUE,
                    width = 9,
                    title = "Legal institutions",
                    plotOutput("Law",
                               width = "65%"),
                    h5("About the data and indicators"),
                    p("Lorem ipsum dolor sit amet, consectetur adipiscing elit,
                          sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
                          quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
                          Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
                          Excepteur sint occaecat cupidatat non proident,
                          sunt in culpa qui officia deserunt mollit anim id est laborum.")
                )

        )
      )
    )
  )
