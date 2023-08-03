library(shiny)

source("modules/func_mod_controls.R")

controlsUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("comp_cat"),
          label = "Select grouping variable",
          choices = c("Region", "Income group", "Lending category", "Other (EMU or HIPC)", "Custom"),
          multiple = TRUE
        )
      ),
      shiny::column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("comp_cat_choices"),
          label = "Select group",
          choices = NULL,
          multiple = TRUE
        )
      )
    )
  )
}

controlsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ## Update labels and of comp_cat_choices


      shiny::observe({
        shiny::req(input$comp_cat)

        comp_cat <- input$comp_cat
        
        print(comp_cat)

        if (any(comp_cat %in% "Custom")) {
          shinyjs::hide("comp_cat_choices")
        } else {
          shinyjs::show("comp_cat_choices")
          shinyWidgets::updatePickerInput(
            session = session,
            inputId = "comp_cat_choices",
            # label = func_comp_cat_choices(comp_cat)[[1]],
            choices = sort(func_comp_cat_choices(comp_cat)[[2]])
          )
        }
      })
    }
  )
}

ui <- fluidPage(
  controlsUI("controls")
)

server <- function(input, output, session) {
  controlsServer("controls")
}

shinyApp(ui, server)