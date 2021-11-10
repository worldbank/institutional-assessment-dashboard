library(shiny)
library(shinydashboard)

ui <-
  dashboardPage(

    skin = "black"

    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)
