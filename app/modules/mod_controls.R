library(shiny)

source("modules/func_mod_controls.R")
countries <- unique(CLASS_new$Economy)

controlsUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(style = "height: 15px;"),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("comp_cat"),
          label = "Select grouping variable",
          choices = c("Region", "Income group", "Lending category", "Other (EMU or HIPC)"),
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
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        prettyCheckbox(
          inputId = ns("create_custom_grps"),
          label = "Create custom groups",
          value = FALSE,
          icon = icon("check"),
          status = "success"
        )
      ),
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId = ns("custom_grps_count"),
          label = "Indicate number of groups",
          value = 1,
          min = 1,
          max = 3
        )
      ),
      shiny::column(
        width = 6,
        shiny::uiOutput(ns("custom_grps"))
      )
    ),
    shiny::fluidRow(
      shiny::actionButton(ns("submit_btn"), "Submit"),

      ## temporary table that shows whether the grouping methodology is working well
      # reactable::reactableOutput(ns("custom_groups_table"))
      shiny::uiOutput(ns("custom_groups_table"))
    )
  )
}

controlsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      ## Hide custom group fields until one chooses to create them ---------------
      shinyjs::hide("custom_grps_count")
      shinyjs::hide("custom_grps")
      shinyjs::hide("custom_groups_table")

      ## Update labels and of comp_cat_choices ------------------------
      shiny::observeEvent(input$comp_cat, {
        comp_cat <- input$comp_cat
        shinyWidgets::updatePickerInput(
          session = session,
          inputId = "comp_cat_choices",
          # label = func_comp_cat_choices(comp_cat)[[1]],
          choices = sort(func_comp_cat_choices(comp_cat)[[2]])
        )
      })

      
      

      ## When one chooses to create custom groups, show the number of groups field
      shiny::observeEvent(input$create_custom_grps, {
        if (input$create_custom_grps == T) {
          shinyjs::show("custom_grps_count")
        } else {
          shinyjs::hide("custom_grps_count")
        }
      })


      ## Show the custom groups ----------------------------

      shiny::observeEvent(input$create_custom_grps, {
        if (input$create_custom_grps == T & input$custom_grps_count > 0) {
          shinyjs::show("custom_grps")
        } else {
          shinyjs::hide("custom_grps")
        }
      })

      ## Create the custom groups ----------------------------

      shiny::observeEvent(input$create_custom_grps, {
        output$custom_grps <- renderUI({
          
          ## number of fields to create
          n_fields <- input$custom_grps_count


          ## create an object that will hold each custom group field (name and country drop-downs)
          ui_fields <- c()


          ## for each custom group ....
          lapply(1:n_fields, function(i) {
            ui_fields[[i]] <- shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::textInput(
                  inputId = ns(paste("custom_grps_names", i, sep = "_")),
                  label = paste("Insert the name of group ", i)
                )
              ),
              shiny::column(
                width = 6,
                pickerInput(
                  inputId = ns(paste("custom_grps_countries", i, sep = "_")),
                  label = paste("Select countries that fall into group ", i),
                  choices = c("", countries),
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE
                  )
                )
              )
            )
          })
        })
      })

      ## table
      
      shiny::observeEvent(input$submit_btn, {
        
        if (input$create_custom_grps == T & input$custom_grps_count > 0) {
          shinyjs::show("custom_groups_table")
        } else {
          shinyjs::hide("custom_groups_table")
        }


        predetermined_groups_table_reactive <- shiny::eventReactive(input$submit_btn, {
          
          # shiny::req(input$comp_cat)
          # shiny::req(input$comp_cat_choices)
          
          comp_cat <- input$comp_cat
          comp_cat_choices <- input$comp_cat_choices
          
          if(!is.null(comp_cat) & !is.null(comp_cat_choices)){
            
          checker = apply(CLASS_new, 2, function(x) any(comp_cat_choices %in% x))
          var = names(checker[checker == TRUE])
          var_sym = sym(var)
          
          country_selection <- CLASS_new %>% 
                                 select(Economy, {{var_sym}}) %>% 
                                 filter({{var_sym}} %in% comp_cat_choices) %>% 
                                 mutate(Category = var) %>% 
                                 rename(Grp = {{var_sym}}, Countries = Economy) %>% 
                                 select(Category, Grp, Countries)
          }else{
           country_selection <- NULL 
          }
          
          

        })
                
        custom_groups_table_reactive <- shiny::eventReactive(input$submit_btn, {
          
          # shiny::req(input$custom_grps_names_1)
          # shiny::req(input$custom_grps_countries_1)
          
          n_fields <- input$custom_grps_count

          custom_grps_list <- list()

          for (i in 1:n_fields) {
            grp_name <- as.character(input[[paste("custom_grps_names", i, sep = "_")]])
            country_selection <- as.vector(input[[paste("custom_grps_countries", i, sep = "_")]])
            custom_grps_list[[i]] <- func_custom_grp(category = "Custom", name = grp_name, countriez = country_selection)
          }

          if(length(custom_grps_list) > 0){
          custom_grps_df <- bind_rows(custom_grps_list)
          }else{
            custom_grps_df <- NULL
          }
          return(custom_grps_df)

        })
        
        output$custom_groups_table <- shiny::renderUI({
        
        if(!is.null(custom_groups_table_reactive()) > 0){
          
          dat <- bind_rows(predetermined_groups_table_reactive(), 
                          custom_groups_table_reactive()
                            )
          
          reactable::reactable(data = dat)
        }
          
        })
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