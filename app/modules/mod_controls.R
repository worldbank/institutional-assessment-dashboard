library(shiny)

source("modules/func_mod_controls.R")
countries <- unique(CLASS_new$Economy)

controlsUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shiny::fluidRow(style = "height: 15px;"),
    shiny::fluidRow(
      
      column(
        width = 3,
        pickerInput(
          "country",
          label = "Select a base country",
          choices = c("", countries),
          selected = NULL,
          multiple = FALSE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE
            
          )
        )
      ),
      
      shiny::column(
        width = 3,
        shinyWidgets::pickerInput(
          inputId = ns("comp_cat"),
          label = "Select grouping variable",
          choices = c("Region", "Income group", "Lending category", "Other (EMU or HIPC)", "FCS"),
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
        align = "left",
        shinyWidgets::prettyCheckbox(
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
          max = 6
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


      ## Object that holds reactives
      rv <- shiny::reactiveValues(
        custom_group_fields_reactive = NULL,
        custom_grps_count = NULL
      )
      
      
      
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

      ## Show the custom groups ----------------------------

      shiny::observeEvent(input$create_custom_grps, {
        
        if (input$create_custom_grps == T) {
          
          shinyjs::show("custom_grps_count")
          shinyjs::show("custom_grps")
        } else {
          shinyjs::hide("custom_grps_count")
          shinyjs::hide("custom_grps")
        }
      })
      


      
      ## Create the custom groups ----------------------------

      custom_group_fields_reactive <- shiny::reactive({
        
        # shiny::req(input$custom_grps_count)
        
        ## number of fields to create
        n_fields <- input$custom_grps_count
        
        
        ## create an object that will hold each custom group field (name and country drop-downs)
        ui_fields <- c()
        
        ## for each custom group ....
        lapply(1:n_fields, function(i) {
          
          custom_names <- ""
          custom_countries <- NULL
          
          if(n_fields >= 1){

            custom_names <- isolate(input[[paste("custom_grps_names", i, sep = "_")]])
            custom_countries <- isolate(input[[paste("custom_grps_countries", i, sep = "_")]])


          ui_fields[[i]] <- shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::textInput(
                inputId = ns(paste("custom_grps_names", i, sep = "_")),
                label = paste("Insert the name of group ", i),
                value = custom_names
              )
            ),
            shiny::column(
              width = 6,
              shinyWidgets::pickerInput(
                inputId = ns(paste("custom_grps_countries", i, sep = "_")),
                label = paste("Select countries that fall into group ", i),
                choices = c("", countries),
                selected = custom_countries,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE
                )
              )
            )
          )
          }
      })
      })

      observeEvent(input$create_custom_grps, {
        
        if (input$create_custom_grps) {
          
          for (i in 1:input$custom_grps_count) {
            shinyjs::hide(id = paste("custom_grps_names", i, sep = "_"))
            shinyjs::reset(id = paste("custom_grps_names", i, sep = "_"))
            shinyjs::hide(id = paste("custom_grps_countries", i, sep = "_"))
            shinyjs::reset(id = paste("custom_grps_countries", i, sep = "_"))
          }
          
        } else {
          shinyjs::reset("custom_grps_count")
          
          for (i in 1:input$custom_grps_count) {
            shinyjs::reset(id = paste("custom_grps_names", i, sep = "_"))
            shinyjs::reset(id = paste("custom_grps_countries", i, sep = "_"))
          }
        }
      })
      
      shiny::observeEvent(input$create_custom_grps, {
        
        output$custom_grps <- renderUI({
          
          custom_group_fields_reactive()
          
        })
      })
      


      ## table

        predetermined_groups_table_reactive <- shiny::eventReactive(input$submit_btn, {
          comp_cat <- input$comp_cat
          comp_cat_choices <- input$comp_cat_choices

          predetermined_groups_table_func(comp_cat, comp_cat_choices)
        })

        custom_groups_table_reactive <- shiny::eventReactive(input$submit_btn, {
          
          
          if(input$create_custom_grps &
              !is.null(input$custom_grps_names_1) & !is.null(input$custom_grps_countries_1)){
          
          n_fields <- input$custom_grps_count

          custom_grps_list <- list()

          for (i in 1:n_fields) {
            grp_name <- as.character(input[[paste("custom_grps_names", i, sep = "_")]])
            country_selection <- as.vector(input[[paste("custom_grps_countries", i, sep = "_")]])
            custom_grps_list[[i]] <- data.frame(Category = "Custom", Grp = grp_name, Countries = country_selection)
          }

          if (length(custom_grps_list) > 0) {
            custom_grps_df <- dplyr::bind_rows(custom_grps_list)
          } else {
            custom_grps_df <- NULL
          }
          }else{
            custom_grps_df <- NULL  
          }
          return(custom_grps_df)
        })

        shiny::observeEvent(input$submit_btn, {
          
          
          output$custom_groups_table <- shiny::renderUI({
          
            print(predetermined_groups_table_reactive())
            print(custom_groups_table_reactive())
            
        if (!is.null(predetermined_groups_table_reactive())|!is.null(custom_groups_table_reactive())) {
          dat <- dplyr::bind_rows(
            predetermined_groups_table_reactive(),
            custom_groups_table_reactive()
          )
          
          reactable::reactable(data = dat)
        } else {
          NULL
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



# Source: https://en.wikipedia.org/wiki/Jim_O%27Neill,_Baron_O%27Neill_of_Gatley#Next_Eleven
# BRICS: a grouping acronym referring to the developing countries of Brazil, Russia, India, China and South Africa 
# which are identified as rising economic powers. These are fast-growing economies that would collectively dominate 
# the global economy by 2050

# N11 countries or the Next 11 countries refers to a group of eleven countries—specifically 
# Bangladesh, Egypt, Indonesia, Iran, Mexico, Nigeria, Pakistan, the Philippines, Turkey, 
# South Korea, and Vietnam—which have emerging markets that could potentially become some of 
# the world's largest economies
#
# Source: https://www.eac.int/overview-of-eac
# EAC: Burundi, Congo, Kenya, Rwanda, South Sudan, Uganda, Tanzania.
#
# Source: https://ischoolconnect.com/blog/worlds-top-powerful-countries-some-exciting-facts-about-them/
# Top 5 Superpowers: USA, China, Russia, Germany, United Kingdom
#
# Source: https://www.un.org/ohrlls/content/list-ldcs
# LDCs: 