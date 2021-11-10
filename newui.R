library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(bs4Dash)
library(fresh)

ui <-
  dashboardPage(

    freshTheme = create_theme(bs4dash_layout(sidebar_width = "400px")),

    dashboardHeader(

      title = dashboardBrand(
        title = "Global Institutional Benchmarking Dashboard ",
      ),
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE

    ),

    dashboardSidebar(

      status = "info",
      skin = "light",
      elevation = 5,

      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Country benchmarking", tabName = "country", icon = icon("sort-amount-up")),
        menuItem("Aggregation of preferences", tabName = "heatmap", icon = icon("comments")),
        menuItem("Map & trends", tabName = "viz", icon = icon("chart-bar")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Methodology", tabName = "methodology", icon = icon("book"))
      )
    ),

    dashboardBody()
  )

server <- function(input, output) { }

shinyApp(ui, server)

