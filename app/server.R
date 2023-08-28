# Server ################################################################################

server <- function(input, output, session) {
  # Handle inputs ======================================================================

  ## Base country ------------------------------------------------------------
  base_country <-
    eventReactive(
      input$select,
      input$country,
      ignoreNULL = FALSE
    )

  ## Start of benchmark tab control inputs----------------------

  ## Create comparison group inputs where users can insert custom group names and countries that
  ## they'd want to place in those groups

  ## Reactive object that will hold all these information
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
      c_groups <- input$groups[!input$groups %in% unlist(group_list)]

      if (n_fields >= 1) {
        custom_names <- isolate(input[[paste("custom_grps_names", i, sep = "_")]])
        custom_countries <- isolate(input[[paste("custom_grps_countries", i, sep = "_")]])

        value_textInput <- custom_names
        selected_pickerinput <- custom_countries


        ui_fields[[i]] <- shiny::fluidRow(
          width = 6,
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = paste("custom_grps_names", i, sep = "_"),
              label = paste("Insert the name of group ", i),
              value = value_textInput
            )
          ),
          shiny::column(
            width = 6,
            shinyWidgets::pickerInput(
              inputId = paste("custom_grps_countries", i, sep = "_"),
              label = paste("Select countries that fall into group ", i),
              choices = c("", countries[!countries %in% input$country]),
              selected = selected_pickerinput,
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

  ## Display the ui
  output$custom_grps <- renderUI({
    custom_group_fields_reactive()
  })

  ## Generate a dataframe containing the custom groups
  custom_grps_df <- shiny::eventReactive(input$save_custom_grps, {
    
    n_fields <- input$custom_grps_count

    if (n_fields > 0) {
      
      custom_grps_list <- list()

      for (i in 1:n_fields) {
        grp_name <- as.character(input[[paste("custom_grps_names", i, sep = "_")]])
        country_selection <- as.vector(input[[paste("custom_grps_countries", i, sep = "_")]])
        
        ## if both fields are filled, create a dataframe 
        if(!is.null(grp_name) & !is.null(country_selection)){
          
          custom_grps_list[[i]] <- data.frame(Category = "Custom", Grp = grp_name, Countries = country_selection)
        }else{
          
          ## else return a NULL object
          custom_grps_list[[i]] <- NULL
        }
        
      }

      ## append all the dataframes into one. 
      custom_grps_df <- dplyr::bind_rows(custom_grps_list)
    } else {
      custom_grps_df <- NULL
    }
    
    ## if we don't have any custom group data (fields are blank), return a NULL object
    if(nrow(custom_grps_df) == 0){
      custom_grps_df <- NULL
    }
    
    ## temporary code that checks the countries that are captured in the custom group dataset
    #print(custom_grps_df)
    
    return(custom_grps_df)
  })
  
  
  ## once the save button is clicked
  shiny::observeEvent(input$save_custom_grps, {
    
    ## if the custom group dataframe is NULL
    if(is.null(custom_grps_df())){
     
      ## unselect the create_custom_grps field
      shinyWidgets::updatePrettyCheckbox(
        session = session,
        inputId = "create_custom_grps",
        value = FALSE
      )
      
    }
    
    ## and convert the custom_grps_df reactive to NULL. 
    if(input$create_custom_grps == FALSE){
    custom_grps_df() <- NULL
    }

  })
  

  ## Turning on the "Show custom groups" switch shows the custom groups ui
  shiny::observeEvent(input$show_custom_grps, {
    if (input$show_custom_grps == TRUE) {
      shinyjs::show(id = "custom_grps_count")
      shinyjs::show(id = "custom_grps")
      shinyjs::show(id = "save_custom_grps")
    } else {
      shinyjs::hide(id = "custom_grps_count")
      shinyjs::hide(id = "custom_grps")
      shinyjs::hide(id = "save_custom_grps")
    }
  })

  ### Once the save button is clicked

  shiny::observeEvent(input$save_custom_grps, {


    ### check if any of the custom group names is part of group list.
    ### If so, ask the user to change the name

    if (any(custom_grps_df()$Grp %in% unlist(group_list))) {
      dup_grp_names <- unique(custom_grps_df()$Grp[custom_grps_df()$Grp %in% unlist(group_list)])

      shiny::showModal(
        modalDialog(
          shiny::tagList(
            shiny::tags$p(
              paste0("The following list includes group name(s) that already exist(s) within the 
                    original group list. Please modify the group name(s) to continue.")
            ),
            shiny::tags$p(
              paste(as.character(dup_grp_names), collapse = " , ")
            )
          )
        )
      )
    } else {
      ### turn off the "Show custom groups" switch
      shinyWidgets::updateMaterialSwitch(
        session = session,
        inputId = "show_custom_grps",
        value = FALSE
      )

      ## and edit the "Select comparison groups" and "Show group median" fields to include these custom groups
      Custom <- list(unique(custom_grps_df()$Grp))

      if(length(unique(custom_grps_df()$Grp)) == 1){
        names(Custom) <- unique(custom_grps_df()$Grp)
      }else{
        names(Custom) <- "Custom"
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "groups",
        choices = as.list(append(group_list, Custom)),
        selected = unique(c(input$groups, unique(custom_grps_df()$Grp)))
      )

      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "benchmark_median",
        choices = append("Comparison countries", append(group_list, Custom)),
        selected = unique(c(input$benchmark_median, custom_grps_df()$Grp))[1:3],
        options = list(
          `live-search` = TRUE,
          "max-options" = 3
        )
      )
    }
  })

  ## Unselecting the "Create custom groups" field should reset all custom group fields,
  ## but retain all other inputs

  shiny::observeEvent(input$create_custom_grps, {
    
    if (input$create_custom_grps == FALSE) {
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "groups",
        choices = group_list,
        selected = input$groups[!input$groups %in% unique(custom_grps_df()$Grp)]
      )

      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "benchmark_median",
        choices = append("Comparison countries", group_list),
        selected = input$benchmark_median[!input$benchmark_median %in% unique(custom_grps_df()$Grp)],
        options = list(
          `live-search` = TRUE,
          "max-options" = 3
        )
      )

      updateCheckboxGroupButtons(
        session,
        "countries",
        label = NULL,
        choices = countries,
        checkIcon = list(
          yes = icon("ok",
            lib = "glyphicon",
            style = "color: #e94152"
          )
        ),
        selected = input$countries[!input$countries %in% unique(custom_grps_df()$Countries)]
      )
    }
  })



  ## Comparison countries

  observeEvent(
    input$groups,
    {
      selected_groups <- input$groups
      selected_country <- input$country

      # Can use character(0) to remove all choices
      if (is.null(selected_groups)) {
        selected <- NULL
      } else {
        selected <-
          country_list %>%
          filter(group %in% selected_groups) %>%
          select(country_name) %>%
          unique()

        if (!is.null(selected_country)) {
          selected <-
            selected %>%
            filter(country_name != selected_country)
        }

        selected <-
          selected %>%
          pluck(1)
      }



      updateCheckboxGroupButtons(
        session,
        "countries",
        label = NULL,
        choices = countries,
        checkIcon = list(
          yes = icon("ok",
            lib = "glyphicon",
            style = "color: #e94152"
          )
        ),
        selected = selected
      )
    },
    ignoreNULL = FALSE
  )

  # When custom groups are included and the group field is updated, append the countries to the initial list of countries
  # displayed
  observeEvent(
    input$groups,
    {
      if (!is.null(custom_grps_df())) {
        
        ## custom group countries
        custom_grp_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups]
        
        ## countries in the group-list groups
        preselected_grp_countries <- country_list %>%
          filter(group %in% input$groups) %>%
          pull(country_name)
        
        ## countries that are selected from the "Select individual comparison countries" directly
        # other_countries <- input$countries[!input$countries %in% c(custom_grp_countries, preselected_grp_countries)]

        # if (length(preselected_grp_countries) > 0) {
        #   selected_c <- unique(c(custom_grp_countries, preselected_grp_countries, other_countries))
        # } else {
        #   selected_c <- unique(c(custom_grp_countries, other_countries))
        # }

        if (length(preselected_grp_countries) > 0) {
          selected_c <- unique(c(custom_grp_countries, preselected_grp_countries))
        } else {
          selected_c <- unique(custom_grp_countries)
        }
        
        updateCheckboxGroupButtons(
          session,
          "countries",
          label = NULL,
          choices = countries,
          checkIcon = list(
            yes = icon("ok",
              lib = "glyphicon",
              style = "color: #e94152"
            )
          ),
          selected = selected_c
        )
      }
    }
  )


  ## Repeat the same above if edits are made in the custom groups where groups aren't updated e.g 
  ## removing or adding countries in the already specified custom groups
  observeEvent(
    input$save_custom_grps,
    {

      if (!is.null(custom_grps_df())) {
        
        ## custom group countries
        custom_grp_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups]
        
        ## countries in the group-list groups
        preselected_grp_countries <- country_list %>%
          filter(group %in% input$groups) %>%
          pull(country_name)

        ## countries that are selected from the "Select individual comparison countries" card directly
        # other_countries <- input$countries[!input$countries %in% c(custom_grp_countries, preselected_grp_countries)]
        
        # if (length(preselected_grp_countries) > 0) {
        #   selected_c <- unique(c(custom_grp_countries, preselected_grp_countries, other_countries))
        # } else {
        #   selected_c <- unique(c(custom_grp_countries, other_countries))
        # }

        if (length(preselected_grp_countries) > 0) {
          selected_c <- unique(c(custom_grp_countries, preselected_grp_countries))
        } else {
          selected_c <- unique(custom_grp_countries)
        }
        
        updateCheckboxGroupButtons(
          session,
          "countries",
          label = NULL,
          choices = countries,
          checkIcon = list(
            yes = icon("ok",
              lib = "glyphicon",
              style = "color: #e94152"
            )
          ),
          selected = selected_c
        )
      }
    }
  )


  ## Validate options

  output$select_button <-
    renderUI({
      if (length(input$countries) >= 10 & input$country != "") {
        actionButton(
          "select",
          "Apply selection",
          icon = icon("check"),
          class = "btn-success",
          width = "100%"
        )
      } else {
        actionButton(
          "select",
          "Select a base country and at least 10 comparison countries to apply selection",
          icon = icon("triangle-exclamation"),
          class = "btn-warning",
          width = "100%",
          shinyjs::disable("report"),
          shinyjs::disable("pptreport"),
        )
      }
    })

  observeEvent(
    input$countries,
    {
      toggleState(
        id = "select",
        condition = length(input$countries) >= 10,
        shinyjs::disable("report"),
      )
      toggleState(
        id = "select",
        condition = length(input$countries) >= 10,
        shinyjs::disable("pptpptreport"),
      )
    },
    ignoreNULL = FALSE
  )
  
  ## End of benchmark tab control inputs----------------------

  ## Reactive objects ==============================================

  ### Benchmark data -----------------------------------------------
  vars <-
    eventReactive(
      input$select,
      {
        if (input$family == "Overview") {
          vars_all
        } else {
          variable_names %>%
            filter(family_name == input$family) %>%
            pull(variable) %>%
            unique()
        }
      }
    )

  ### Comparison group note (group or countries) -------------------------
  ### To be edited to include custom groups
  note_compare <-
    eventReactive(
      input$select,
      {
        browser()
        ## countries that fall under input$groups
        ## 
        group_list_countries <- country_list %>%
          filter(group %in% input$groups) %>%
          pull(country_name)

        
        ## custom groups countries that fall under input$groups and are selected in the 
        ## "Select individual comparison countries" card. We don't care about those that are unselected.
        custom_df_countries <- NULL
        
        if (input$create_custom_grps == TRUE) {
          custom_df_countries <- custom_grps_df()$Countries[custom_grps_df()$Grp %in% input$groups &
            custom_grps_df()$Countries %in% input$countries]
        }
        
        ## if we ever want to see which countries are in custom_grps_df() but unselected (not in input$countries), run
        ## the following code.
        ## Do not activate this line here coz the app will not work
        # dropped_custom_countries =  custom_grps_df()$Countries[!custom_grps_df()$Countries %in% input$countries]


        ## if no groups are selected
        if (is.null(input$groups)) {
          
          ## return the countries
          return(input$countries)
          
        } else if (
          ## else if all the countries are part of the group-list and custom group countries ... 
          
          all(
            unique(input$countries) %in%
              unique(
                c(group_list_countries, custom_df_countries)
              )
          )
        ) {
          ## return the groups instead
          return(input$groups)
        } else {
          
          ## else return countries (this would occur if we directly selected some countries in the 
          ## "Select individual comparison countries" card that are not part of the group-list groups 
          ## or custom groups)
          return(input$countries)
        }
      }
    )


  
  
  ### Indicators with low variance -------------------------------------------
  low_variance_indicators <-
    eventReactive(
      input$select,
      {
        global_data %>%
          low_variance(
            base_country(),
            country_list,
            input$countries,
            vars(),
            variable_names
          )
      }
    )

  low_variance_indicators_dyn <-
    eventReactive(
      input$select,
      {
        global_data_dyn %>%
          low_variance_dyn(
            base_country(),
            country_list,
            input$countries,
            vars(),
            variable_names
          )
      }
    )
  
  data <-
    eventReactive(
      input$select,
      {
        global_data %>%
          def_quantiles(
            base_country(),
            country_list,
            input$countries,
            vars_all,
            variable_names,
            input$threshold
          )
      }
    )

  data_dyn <-
    eventReactive(
      input$select,
      {
        global_data_dyn %>%
          def_quantiles_dyn(
            base_country(),
            country_list,
            input$countries,
            vars_all,
            variable_names,
            input$threshold
          )
      }
    )
  
  data_family <-
    eventReactive(
      input$select,
      
      {

        
        family_data(
          global_data,
          base_country(),
          variable_names
        ) %>%
          def_quantiles(
            base_country(),
            country_list,
            input$countries,
            vars_family,
            family_names,
            input$threshold
          )
      }
    )
  
  data_family_dyn <-
    eventReactive(
      input$select,
      
      {
        
        family_data_dyn(
          global_data_dyn,
          base_country(),
          variable_names
        ) %>%
          def_quantiles_dyn(
            base_country(),
            country_list,
            input$countries,
            vars_family,
            family_names,
            input$threshold
          )
      }
    )
  

  # Missing variables from base country
  na_indicators <-
    eventReactive(
      input$select,
      {
        global_data %>%
          ungroup() %>%
          filter(country_name == input$country) %>%
          select(where(is.na)) %>%
          names()
      }
    )

  ### Update inputs based on benchmark selection ------------------------------
  observeEvent(
    input$select,
    {
      if (!is.null(input$groups)) {
        updatePickerInput(
          session,
          "groups_data",
          choices = c("All", "Comparison groups only", "None")
        )

        updatePickerInput(
          session,
          "groups_bar",
          selected = input$groups
        )

        updatePickerInput(
          session,
          "high_group",
          selected = input$groups
        )

        updatePickerInput(
          session,
          "group_trends",
          selected = input$groups
        )
      }

      updatePickerInput(
        session,
        "countries_data",
        choices = c("All", "Base country only", "Base + comparison countries")
      )

      # Create report
      toggleState(
        id = "report",
        condition = input$select,
        shinyjs::disable("report")
      )
      toggleState(
        id = "pptreport",
        condition = input$select,
        shinyjs::disable("pptreport")
      )

      # Cross-crountry comparison selection
      updatePickerInput(
        session,
        "country_bar",
        selected = input$country
      )

      updateCheckboxGroupButtons(
        session,
        "countries_bar",
        selected = input$countries
      )

      # Bivariate correlation selection
      updatePickerInput(
        session,
        "country_scatter",
        selected = input$country
      )

      updateCheckboxGroupButtons(
        session,
        "countries_scatter",
        selected = input$countries
      )

      # Time trends
      updatePickerInput(
        session,
        "country_trends",
        selected = input$country
      )

      updateCheckboxGroupButtons(
        session,
        "countries_trends",
        selected = input$countries
      )
    },
    ignoreNULL = TRUE
  )


  ## Make sure only valid groups are chosen ----------------------------------

  observeEvent(
    input$country,
    {
      valid_vars <-
        ctf_long %>%
        filter(
          country_name == input$country,
          !is.na(value)
        ) %>%
        select(family_name) %>%
        unique() %>%
        unlist() %>%
        unname()

      updatePickerInput(
        session,
        "family",
        choices = c(
          "Overview",
          intersect(names(variable_list), valid_vars)
        )
      )
    },
    ignoreNULL = FALSE
  )

  ## Median data ------------------------------------------------------------

  data_family_median <-
    eventReactive(
      input$select,
      {
        family_data(
          global_data,
          base_country(),
          variable_names
        )
      }
    )

  ## Benchmark plot ============================================================

  ## custom_df dataset will be used here if the groups in it are part of the benchmark median groups and its countries 
  ## are selected
  

  custom_df <- shiny::eventReactive(input$select, {
    if (input$create_custom_grps == TRUE) {
      custom_df <- custom_grps_df()[custom_grps_df()$Grp %in% input$benchmark_median &
          custom_grps_df()$Countries %in% input$countries, ]
    } else {
      custom_df <- NULL
    }
    
  })

  output$plot <-
    renderPlotly({
      
      if (length(input$countries) >= 10) {
        
        input$select


        # browser()
        
        ## Important!
        ## Shel added custom_df as an argument in the static_plot function to accommodate the custom groups


        isolate(
          
          
          if (input$family == "Overview") {
            missing_variables <-
              global_data %>%
              missing_var(
                base_country(),
                country_list,
                input$countries,
                vars_all,
                variable_names
              )

            low_variance_variables <-
              low_variance_indicators() %>%
              data.frame() %>%
              rename("variable" = ".") %>%
              left_join(variable_names %>% select(variable, var_name), by = "variable") %>%
              .$var_name

            missing_variables <- c(missing_variables, low_variance_variables)

            data_family() %>%
              static_plot(
                base_country(),
                input$family,
                input$rank,
                dots = input$benchmark_dots,
                group_median = input$benchmark_median,
                custom_df = custom_df(),
                threshold = input$threshold
              ) %>%
              interactive_plot(
                base_country(),
                note_compare(),
                input$family,
                plotly_remove_buttons,
                missing_variables
              )
          } else {
            missing_variables <-
              global_data %>%
              missing_var(
                base_country(),
                country_list,
                input$countries,
                vars(),
                variable_names
              )

            low_variance_variables <-
              low_variance_indicators() %>%
              data.frame() %>%
              rename("variable" = ".") %>%
              left_join(variable_names %>% select(variable, var_name), by = "variable") %>%
              .$var_name

            missing_variables <- c(missing_variables, low_variance_variables)

            data() %>%
              filter(variable %in% vars()) %>%
              static_plot(
                base_country(),
                input$family,
                input$rank,
                dots = input$benchmark_dots,
                group_median = input$benchmark_median,
                custom_df = custom_df(),
                threshold = input$threshold
              ) %>%
              interactive_plot(
                base_country(),
                note_compare(),
                input$family,
                plotly_remove_buttons,
                missing_variables
              )
          }
        )
      }
    }) %>%
  bindCache(input$country,  input$group, input$family, input$benchmark_median,
    input$rank, input$benchmark_dots, input$create_custom_grps,
    input$show_dynamic_plot, input$threshold) %>%
  bindEvent(input$select)

  ## End of benchmark tab ----------------------

  ## Dynamic benchmark plot  ============================================================
  
  output$dynamic_benchmark_plot <-
    renderPlotly({
      if (length(input$countries) >= 10) {
        
        input$select

        isolate(
          if (input$family == "Overview") {
            missing_variables <-
              global_data_dyn %>%
              missing_var_dyn(
                base_country(),
                country_list,
                input$countries,
                vars_all,
                variable_names
              )
            
            low_variance_variables <-
              low_variance_indicators_dyn() %>%
              data.frame() %>%
              rename("variable" = ".") %>%
              left_join(variable_names %>% select(variable, var_name), by = "variable") %>%
              .$var_name
            
            missing_variables <- c(missing_variables, low_variance_variables)
            
           
            data_family_dyn() %>%
              static_plot_dyn(
                base_country(),
                input$family,
                input$rank,
                dots = input$benchmark_dots,
                group_median = input$benchmark_median,
                custom_df = custom_df(),
                threshold = input$threshold
              )%>%
              interactive_plot(
                base_country(),
                note_compare(),
                input$family,
                plotly_remove_buttons,
                missing_variables
              )
          } else {
            
            missing_variables <-
              global_data_dyn %>%
              missing_var_dyn(
                base_country(),
                country_list,
                input$countries,
                vars(),
                variable_names
              )
            
            low_variance_variables <-
              low_variance_indicators_dyn() %>%
              data.frame() %>%
              rename("variable" = ".") %>%
              left_join(variable_names %>% select(variable, var_name), by = "variable") %>%
              .$var_name
            
            missing_variables <- c(missing_variables, low_variance_variables)
            
            
            data_dyn() %>%
              filter(variable %in% vars()) %>%
              static_plot_dyn(
                base_country(),
                input$family,
                input$rank,
                dots = input$benchmark_dots,
                group_median = input$benchmark_median,
                custom_df = custom_df(),
                threshold = input$threshold
              ) %>%
              interactive_plot(
                base_country(),
                note_compare(),
                input$family,
                plotly_remove_buttons,
                missing_variables
              )
            
          }
        )
      }
    }) %>%
  bindCache(input$country,  input$group, input$family, input$benchmark_median,
    input$rank, input$benchmark_dots, input$create_custom_grps,
    input$show_dynamic_plot, input$threshold) %>%
  bindEvent(input$select)
  

  ## Change variable selection in all tabs --------------------------

  observeEvent(
    input$vars_bar,
    {
      updatePickerInput(
        session,
        "y_scatter",
        selected = input$vars_bar
      )

      updatePickerInput(
        session,
        "vars_map",
        selected = input$vars_bar
      )

      updatePickerInput(
        session,
        "vars_trends",
        selected = input$vars_bar
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    input$y_scatter,
    {
      updatePickerInput(
        session,
        "vars_bar",
        selected = input$y_scatter
      )

      updatePickerInput(
        session,
        "vars_map",
        selected = input$y_scatter
      )

      updatePickerInput(
        session,
        "vars_trends",
        selected = input$y_scatter
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    input$vars_map,
    {
      updatePickerInput(
        session,
        "vars_bar",
        selected = input$vars_map
      )

      updatePickerInput(
        session,
        "y_scatter",
        selected = input$vars_map
      )

      updatePickerInput(
        session,
        "vars_trends",
        selected = input$vars_map
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(
    input$vars_trends,
    {
      updatePickerInput(
        session,
        "vars_bar",
        selected = input$vars_trends
      )

      updatePickerInput(
        session,
        "y_scatter",
        selected = input$vars_trends
      )

      updatePickerInput(
        session,
        "vars_map",
        selected = input$vars_trends
      )
    },
    ignoreNULL = FALSE
  )


  # Bar plot ==================================================================

  observeEvent(
    input$vars_bar,
    {
      var <-
        db_variables %>%
        filter(var_name == input$vars_bar) %>%
        pull(variable)

      valid <-
        global_data %>%
        filter(
          !is.na(get(var))
        ) %>%
        select(country_name) %>%
        unique() %>%
        unlist() %>%
        unname()

      bar_countries <-
        intersect(valid, countries)

      updatePickerInput(
        session,
        "country_bar",
        choices = c(
          "",
          bar_countries
        )
      )

      updateCheckboxGroupButtons(
        session,
        "countries_bar",
        choices = bar_countries,
        checkIcon = list(
          yes = icon(
            "ok",
            lib = "glyphicon"
          )
        )
      )
    },
    ignoreNULL = TRUE
  )


  output$bar_plot <-
    renderPlotly({
      static_bar(
        global_data,
        input$country_bar,
        input$countries_bar,
        input$groups_bar,
        input$vars_bar,
        variable_names
      ) %>%
        interactive_bar(
          input$vars_bar,
          db_variables,
          plotly_remove_buttons
        )
    })

  # Scatter plot ============================================================

  high_group <- reactive({
    country_list %>%
      filter(group %in% input$high_group) %>%
      select(group, country_code)
  })

  output$scatter_plot <-
    renderPlotly({
      static_scatter(
        global_data,
        input$country_scatter,
        input$countries_scatter,
        high_group(),
        input$y_scatter,
        input$x_scatter,
        variable_names,
        country_list
      ) %>%
        interactive_scatter(
          input$y_scatter,
          input$x_scatter,
          db_variables,
          high_group(),
          plotly_remove_buttons
        )
    })

  # Map =======================================================================

  output$map <-
    renderPlotly({
      if (input$vars_map != "") {
        var_selected <-
          variable_names %>%
          filter(var_name == input$vars_map) %>%
          pull(variable)

        static_map(
          input$value_map,
          var_selected,
          input$vars_map,
          input$countries_map,
          base_country(),
          input$countries
        ) %>%
          interactive_map(
            var_selected,
            db_variables,
            plotly_remove_buttons,
            input$value_map
          )
      }
    })

  # Trends plot ===============================================================

  observeEvent(
    input$vars_trends,
    {
      if (input$vars_trends != "") {
        var <-
          db_variables %>%
          filter(var_name == input$vars_trends) %>%
          pull(variable)

        valid <-
          global_data %>%
          filter(
            !is.na(get(var))
          ) %>%
          select(country_name) %>%
          unique() %>%
          unlist() %>%
          unname()

        valid_countries <-
          intersect(valid, countries)

        updatePickerInput(
          session,
          "country_trends",
          choices = c(
            "",
            valid_countries
          )
        )

        updateCheckboxGroupButtons(
          session,
          "countries_trends",
          choices = valid_countries,
          checkIcon = list(
            yes = icon(
              "ok",
              lib = "glyphicon"
            )
          )
        )
      }
    },
    ignoreNULL = TRUE
  )


  output$time_series <-
    renderPlotly({
      if (input$vars_trends != "") {
        var <-
          db_variables %>%
          filter(var_name == input$vars_trends) %>%
          pull(variable)

        trends_plot(
          raw_data,
          var,
          input$vars_trends,
          input$country_trends,
          input$countries_trends,
          country_list,
          input$group_trends,
          db_variables
        )
      }
    })

  # Data table ================================================================

  browse_data <-
    reactive({
      data <-
        if (input$data_source == "Closeness to frontier") {
          global_data
        } else {
          raw_data
        }

      groups <-
        if (input$groups_data == "All") {
          all_groups
        } else if (input$groups_data == "Comparison groups only" & input$groups != "") {
          input$groups
        } else {
          ""
        }

      selected_countries <-
        if (input$countries_data == "All") {
          countries
        } else if (input$countries_data == "Base country only") {
          input$country
        } else if (input$countries_data == "Base + comparison countries") {
          c(input$country, input$countries)
        }

      vars <-
        variable_names %>%
        filter(
          family_name %in% input$vars,
          var_level == "indicator"
        ) %>%
        select(variable) %>%
        unlist()

      if (input$data_source != "Closeness to frontier") {
        vars_table <- c("Country", "Year", vars)
      } else {
        vars_table <- c("Country", "country_code", "country_group", vars)
      }

      vars_table <- unname(vars_table)

      data <-
        data %>%
        rename(Country = country_name) %>%
        filter(
          Country %in% c(selected_countries, groups)
        ) %>%
        ungroup() %>%
        mutate(
          across(
            where(is.numeric),
            round, 3
          )
        ) %>%
        select(all_of(vars_table))

      data <-
        data %>%
        setnames(
          .,
          as.character(variable_names$variable),
          as.character(variable_names$var_name),
          skip_absent = TRUE
        )

      if (input$data_value == "Rank") {
        data1 <-
          data %>%
          filter(country_group == 0) %>%
          mutate(
            across(
              4:ncol(.),
              ~ rank(desc(.), ties.method = "min")
            )
          )

        # data2<-data %>%
        #   filter(country_group == 1) %>%
        #   mutate(
        #     across(
        #       4:ncol(.),
        #       ~ dense_rank(desc(.))
        #     )
        #   )

        data <- data1
      }

      return(data)
    })

  output$benchmark_datatable <-
    DT::renderDataTable(
      server = FALSE,
      datatable(
        browse_data(),
        rownames = FALSE,
        extensions = c("FixedColumns"),
        filter = "none",
        options = list(
          scrollX = TRUE,
          scrollY = "550px",
          pageLength = 25,
          autoWidth = TRUE,
          dom = "lftipr",
          fixedColumns = list(leftColumns = 1, rightColumns = 0)
        )
      )
    )

  # Downloadable rds of selected dataset
  output$download_global_rds <-
    downloadHandler(
      filename = function() {
        paste0(input$data_source, "-", input$data_value, "-gbid-data.rds")
      },
      content = function(file) {
        write_rds(
          browse_data() %>%
            setnames(
              .,
              as.character(variable_names$var_name),
              as.character(variable_names$variable),
              skip_absent = TRUE
            ),
          file
        )
      }
    )

  # Downloadable csv of selected dataset
  output$download_global_csv <-
    downloadHandler(
      filename = function() {
        paste0(input$data_source, "-", input$data_value, "-gbid-data.csv")
      },
      content = function(file) {
        write_csv(
          browse_data(),
          file,
          na = ""
        )
      }
    )

  # Downloadable dta of selected dataset
  output$download_global_dta <-
    downloadHandler(
      filename = function() {
        paste0(input$data_source, "-", input$data_value, "-gbid-data.dta")
      },
      content = function(file) {
        write_dta(
          browse_data() %>%
            setnames(
              .,
              as.character(variable_names$var_name),
              as.character(variable_names$variable),
              skip_absent = TRUE
            ),
          file
        )
      }
    )


  # Report ================================================================================

  output$report <- downloadHandler(
    filename =
      reactive(
        paste0(
          "CLIAR-benchmarking-",
          base_country(),
          ".docx"
        )
      ),
    content = function(file) {
      show_modal_spinner(
        color = "#17a2b8",
        text = "Compiling report",
      )

      on.exit(remove_modal_spinner())

      tmp_dir <- tempdir()

      tempReport <- file.path(tmp_dir, "report.Rmd")

      file.copy("www/", tmp_dir, recursive = TRUE)
      file.copy("report.Rmd", tempReport, overwrite = TRUE)


      params <-
        list(
          base_country = base_country(),
          comparison_countries = input$countries,
          data = data(),
          family_data = data_family(),
          rank = input$rank,
          definitions = definitions,
          variable_names = variable_names,
          dots = input$benchmark_dots,
          group_median = input$benchmark_median,
          threshold = input$threshold
        )

      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv()),
        knit_root_dir = getwd()
      )
    }
  )

  # PPT Report ================================================================================

  output$pptreport <- downloadHandler(
    filename =
      reactive(
        paste0(
          "CLIAR-PPT-",
          base_country(),
          ".pptx"
        )
      ),
    content = function(file) {
      show_modal_spinner(
        color = "#17a2b8",
        text = "Compiling report",
      )

      on.exit(remove_modal_spinner())

      tmp_dir <- tempdir()

      tempReport <- file.path(tmp_dir, "CLAR_template.pptx")

      # file.copy("www/", tmp_dir, recursive = TRUE)
      # file.copy("CLAR_template.pptx", tempReport, overwrite = TRUE)

      ppt <- read_pptx("www/CLIAR_template.pptx")


      if (input$create_custom_grps == TRUE) {
        custom_df <- custom_grps_df()[custom_grps_df()$Grp %in% input$benchmark_median &
          custom_grps_df()$Countries %in% input$countries, ]
      } else {
        custom_df <- NULL
      }

      plot1 <- data_family() %>%
        static_plot(
          base_country(),
          "Country overview",
          rank = input$rank,
          group_median = input$benchmark_median,
          dots = input$benchmark_dots,
          custom_df = custom_df,
          title = FALSE,
          threshold = input$threshold
        )
      
      plot2 <- data_family_dyn() %>%
        static_plot_dyn(
          base_country(),
          input$family,
          input$rank,
          dots = input$benchmark_dots,
          group_median = input$benchmark_median,
          custom_df = custom_df,
          threshold = input$threshold
        )
      

      plot1 <- dml(ggobj = plot1)
      plot2 <- dml(ggobj = plot2)
      
      
      ppt <- ppt %>%
        on_slide(index = 4) %>%
        ph_with(value = plot1, location = ph_location(
          left = 1.5, top = 1.2,
          width = 10.04, height = 4.67, bg = "transparent"
        )) %>% 
        #https://rdrr.io/cran/officer/src/R/pptx_slide_manip.R
        add_slide(master = "Custom Design") %>%
        on_slide(index = 5) %>%
        ph_with(value = plot2, location = ph_location(
          left = 1.5, top = 1.2,
          width = 10.04, height = 4.67, bg = "transparent"
        ))

      print(ppt, file)
    }
  )




  # Definitions ===========================================================================

  output$definition <-
    renderTable({
      variables <-
        db_variables %>%
        filter(var_level == "indicator")

      if (input$family != "Overview") {
        variables <-
          variables %>%
          filter(family_name == input$family)
      }

      variables %>%
        select(
          Indicator = var_name,
          Family = family_name,
          Description = description,
          Source = source
        )
    })

  output$definition_bar <-
    renderTable({
      variables <-
        db_variables %>%
        filter(
          var_name == input$vars_bar
        ) %>%
        select(
          Indicator = var_name,
          Family = family_name,
          Description = description,
          Source = source
        )
    })


  # Download csv with definitions
  output$download_indicators <-
    downloadHandler(
      filename = "CLIAR Indicators.csv",
      content = function(file) {
        write_csv(
          db_variables %>%
            select(
              indicator = var_name,
              family = family_name,
              description,
              source
            ),
          file,
          na = ""
        )
      }
    )

  # Full methodology --------------------------------------------------------
  output$download_metho <-
    downloadHandler(
      filename = "CLIAR-Methodological-Note_20220403.pdf",
      content = function(file) {
        file.copy("www/CLIAR-Methodological-Note_20220403.pdf", file)
      }
    )
}
