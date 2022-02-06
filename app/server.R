
# Load packages ###########################################################################

  library(shiny)
  library(tidyverse)
  library(haven)
  library(DT)
  library(data.table)
  library(plotly)
  library(sf)
  library(hrbrthemes)
  library(stringr)
  library(grDevices)
  library(shinyjs)


# Inputs ################################################################################

  plotly_remove_buttons <-
    c("zoomIn2d",
      "zoomOut2d",
      "pan2d",
      "autoScale2d",
      "lasso2d",
      "select2d",
      "toggleSpikelines",
      "hoverClosest3d",
      "hoverClosestCartesian",
      "hoverCompareCartesian")

  ## Auxiliary functions -----------------------------------------------------------------

  #source(file.path("auxiliary",
  #                 "vars-by-family.R"))

  source(file.path("auxiliary",
                   "vars-control.R"))

  db_variables <-
    db_variables %>%
    filter(variable %in% vars_all | var_level == "family") %>%
    mutate(
      description = str_replace_all(description, "[[:punct:]]", " ")
    )

  # Function that defines quantiles based on country, comparison and variables
  source(file.path("auxiliary",
                   "fun_quantiles.R"))

  source(file.path("auxiliary",
                   "fun_family_data.R"))

  # Create benchmark graphs
  source(file.path("auxiliary",
                   "plots.R"))

  ## Data sets ---------------------------------------------------------------------------

  # Indicator definitions
  #definitions <-
  #  read_rds(file.path("data",
  #                     "indicator_definitions.rds"))

  #all_indicators <-
  #  read_rds(file.path("data",
  #                     "list_of_indicators.rds"))

  # Closeness to frontier data
  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds")) %>%
    mutate(
      country_name = country_name %>%
      str_replace_all("Macedonia", "North Macedonia") %>%
      str_replace_all("Swaziland", "Eswatini")
    )

  wb_country_geom_fact <-
    read_rds(file.path("data",
                       "wb_country_geom_fact.rds"))

  st_crs(wb_country_geom_fact) <- "WGS84"

  # Raw data
  raw_data <-
    read_rds(file.path("data",
                       "raw_data.rds")) %>%
    filter(year >= 1990,
           rowSums(!is.na(.)) > 3) %>%
    rename(Year = year)

  # Metadata
  variable_names <-
    db_variables %>%
    select(variable,var_level,var_name,family_var,family_name)

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  color_groups <- colorRampPalette(c("#053E5D", "#60C2F7"))


# Server ################################################################################

  server <- function(input, output, session) {


   # Handle inputs ======================================================================

    ## Base country ------------------------------------------------------------
    base_country <-
      eventReactive(
        input$select,
        input$country
      )

    ## Comparison countries ----------------------------------------------------
    observeEvent(
      input$groups,

      {
        selected_groups  <- input$groups
        selected_country <- input$country

        # Can use character(0) to remove all choices
        if (is.null(selected_groups)) {
          selected <- NULL
        } else {
          selected <-
            country_list %>%
            filter(group %in% selected_groups) %>%
            select(country_name) %>%
            unique

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
          choices = global_data$country_name %>% unique %>% sort,
          checkIcon = list(
            yes = icon("ok",
                       lib = "glyphicon")
          ),
          selected = selected
        )
      },

      ignoreNULL = FALSE
    )

    ## Validate options -------------------------------------------------------
    observeEvent(
      input$countries,

       {
         toggleState(
           id = "select",
           condition = length(input$countries) >= 10
         )
       },

       ignoreNULL = FALSE
    )


    observeEvent(
      input$select,

      {
        toggleState(
          id = "report",
          condition = input$select
        )
      },

      ignoreNULL = FALSE
    )

    # Reactive objects ==============================================

    ## Benchmark data -----------------------------------------------
    data <-
      eventReactive(
        input$select,

        {
          data <-
            global_data %>%
            def_quantiles(
              base_country(),
              country_list,
              input$countries,
              vars_all,
              variable_names
            )
        }
      )

    ## Browse data -------------------------------------------------------------
    browse_data <-
      eventReactive(
        input$data,

        {
          selected_data <- input$data

          if (selected_data == "Closeness to frontier") {
            return(global_data)
          }

          if (selected_data == "Compiled indicators") {
            return(raw_data)
          }
        }
      )

   # Benchmark plot ============================================================

    output$plot <-
      renderPlotly({

        input$select

        isolate(

          if (input$family == "Overview") {

            data <-
              family_data(
                global_data,
                base_country(),
                variable_names
              ) %>%
              def_quantiles(
                base_country(),
                country_list,
                input$countries,
                family_names,
                variable_names
              )

            data %>%
              static_plot(base_country(),
                          input$family) %>%
              interactive_plot(base_country(),
                               input$groups,
                               input$family,
                               plotly_remove_buttons)
          } else {

            vars <-
              # case_when()
              if (input$family == "Labor market institutions") {vars_lab} else
                if (input$family == "Financial market institutions") {vars_fin} else
                  if (input$family == "Legal institutions") {vars_leg} else
                    if (input$family == "Political institutions") {vars_pol} else
                      if (input$family == "Social institutions") {vars_social} else
                        if (input$family == "Business environment and trade institutions") {vars_mkt} else
                          if (input$family == "Public sector performance institutions") {vars_publ} else
                            if (input$family == "SOE Corporate Governance") {vars_service_del} else
                              if (input$family == "Anti-Corruption, Transparency and Accountability institutions") {vars_transp}

            data() %>%
              filter(variable %in% vars) %>%
              static_plot(base_country(),
                          input$family) %>%
              interactive_plot(base_country(),
                               input$groups,
                               input$family,
                               plotly_remove_buttons)
          }
        )
      })





   # Map =======================================================================

    output$map <-
      renderPlotly({

        if (input$vars_map != "") {

          var_selected <-
            variable_names %>%
            filter(var_name == input$vars_map) %>%
            .$variable

          static_map(wb_country_geom_fact,
                     var_selected,
                     input$vars_map) %>%
          interactive_map(input$vars_map,
                          plotly_remove_buttons)
        }

      })

   # Trends plot ===============================================================

    var_trends <-
      eventReactive(
        input$indicator_trends,

        {
          var_selected <-
            variable_names %>%
            filter(var_name == input$indicator_trends) %>%
            .$variable
        }
      )

    observeEvent(
      input$indicator_trends,

      {
        valid_countries <-
          raw_data %>%
          filter(!is.na(get(var_trends()))) %>%
          select(country_name) %>%
          unique %>%
          arrange(country_name) %>%
          unlist %>%
          unname

        last_country <-
          input$country_trends

        updatePickerInput(
          session,
          "country_trends",
          choices = valid_countries,
          selected = last_country
        )

        last_countries <-
          input$countries_trends

        updatePickerInput(
          session,
          "countries_trends",
          choices = valid_countries,
          selected = last_countries
        )
      },

      ignoreNULL = FALSE
    )


    data_trends <-
      reactive({

        data <-
          raw_data %>%
          filter(country_name %in% c(input$country_trends)) %>%
          mutate(alpha = .8,
                 shape = 19)

        if (!is.null(input$countries_trends)) {

          data <-
            raw_data %>%
            filter(country_name %in% c(input$countries_trends)) %>%
            mutate(alpha = .5,
                   shape = 18) %>%
            bind_rows(data)
        }

        if (!is.null(input$group_trends)) {

          indicator <-
            raw_data %>%
            select(country_name, Year, all_of(var_trends()))

          data <-
            country_list %>%
            filter(group %in% input$group_trends) %>%
            select(group, country_name) %>%
            mutate(country_name = as.character(country_name)) %>%
            left_join(indicator) %>%
            group_by(Year, group) %>%
            summarise_all(~ mean(., na.rm = TRUE)) %>%
            mutate(country_name = as.character(group),
                   alpha = .5,
                   shape = 19) %>%
            bind_rows(data)
        }

        data %>%
          rename(Country = country_name) %>%
          select(Country, Year, all_of(var_trends()), alpha) %>%
          mutate_at(vars(all_of(var_trends())),
                    ~ round(., 3))
      })

    output$time_series <-
      renderPlotly({

        if (input$indicator_trends != "") {
          static_plot <-
            ggplot(data_trends(),
                   aes_string(x = "Year",
                              y = var_trends(),
                              color = "Country",
                              group = "Country",
                              alpha = "alpha")) +
            geom_point(aes(text = paste0("Country: ", Country, "<br>",
                                        "Year: ", Year, "<br>",
                                        "Value: ", get(var_trends()))),
                       size = 3) +
            geom_line() +
            theme_ipsum() +
            labs(
              x = "Year",
              y = "Indicator value",
              title = paste0("<b>",input$indicator_trends,"</b>")
            ) +
            scale_color_manual(
              name = NULL,
              values = c("#FB8500",
                         gray.colors(length(input$countries_trends)),
                         color_groups(length(input$group_trends))),
              breaks = c(input$country_trends,
                         input$countries_trends,
                         input$group_trends)
            ) +
            scale_alpha_identity() +
            theme(
              axis.text.x = element_text(angle = 90)
            )

          ggplotly(static_plot, tooltip = "text") %>%
            layout(
              legend = list(
                title = list(text = '<b>Country:</b>'),
                y = 0.5
              ),
              margin = list(l = 50, r = 50, t = 75, b = 135)
            ) %>%
            config(
              modeBarButtonsToRemove = plotly_remove_buttons,
              toImageButtonOptions = list(filename = paste0("trends_",
                                                           tolower(input$country_trends),"_",
                                                           tolower(input$indicator_trends)))
            )
        }

      })

    # Aggregation of preferences ===============================================
    observeEvent(input$select_pref,{

      variable_names <-
        variable_names %>%
        select(family_var,
               family_name) %>%
        rename(var_name = family_name,
               variable = family_var) %>%
        unique

      data <-
        family_data(
          global_data,
          input$country_pref,
          input$groups
        ) %>%
        def_quantiles(
          input$country_pref,
          country_list,
          input$groups,
          variable_names$variable,
          variable_names
        )  %>%
        filter(country_name == input$country_pref) %>%
        ungroup() %>%
        select(var_name, status_dtf) %>%
        unique %>%
        filter(status_dtf %in% c("Weak","Emerging")) %>%
        mutate(
          development_change_1 = 0,
          development_change_2 = 1,
          development_change_3 = 2,
          development_change_4 = 3
        ) %>%
        as.data.frame()

      output$comparison_countries <-
        renderText({
          paste0(" <b>Comparison countries: </b>", paste(input$countries, collapse = ", "))
        })

      output$table_legend <-
        renderText({
          paste0("<b>Legend:</b> For the institutional challenges, the coloring reflects the institutional areas in need of development (orange) and those that are emerging/transitioning (yellow). For the SCD development challenges, the cells are shaded to reflect the following relation ship: top tier/substantial (<b>3</b>); mid-tier or moderate (<b>2</b>); low-tier or marginal (<b>1</b>).")
        })

      output$matrix <-
        renderDT({

          data %>%
            datatable(
              rownames = FALSE,
              colnames = c("Institutional dimension in which the country lags most","Classification",
                           "Development Change #1","Development Change #2","Development Change #3","Development Change #4"),
              editable = list(target = "cell", disable = list(columns = c(0,1))),
              extensions = c('Buttons','Responsive'),
              selection = 'none',
              options = list(
                columnDefs = list(list(targets = 1, visible = FALSE)),
                paging = FALSE,
                searching = FALSE,
                dom = "lBtpr",
                buttons = c('csv', 'excel', 'pdf')
              )
            ) %>%
            formatStyle("var_name","status_dtf",
                        color = "black",
                        backgroundColor = styleEqual(c("Weak","Emerging"), c('#D2222D', '#FFBF00')),
                        fontSize = '90%', fontWeight = 'bold') %>%
            formatStyle(
              c(3:6),
              fontSize = '110%', fontWeight = 'bold',
              backgroundColor = styleEqual(c(0,1,2,3), c('#bababa','#d9f2d9','#7ad17a',"#349834"))
            )

        })

      proxy <- dataTableProxy('matrix')

      observeEvent(input$matrix_cell_edit, {

        data <<-
          editData(data,
                   input$matrix_cell_edit,
                   proxy = proxy,
                   resetPaging = FALSE,
                   rownames = FALSE
          )

      })

    })

   # Data table ================================================================

    output$benchmark_datatable <-
      renderDataTable(server = FALSE, {

        vars <-
          variable_names %>%
          filter(family_name %in% input$vars,
                 var_level == "indicator") %>%
          .$variable %>%
          unlist

        if (input$data == "Compiled indicators") {
          vars_table <- c("Country", "Year", vars)
        } else {
          vars_table <- c("Country", vars)
        }

        data <-
          browse_data() %>%
          rename(Country = country_name) %>%
          ungroup() %>%
          select(all_of(vars_table)) %>%
          mutate(across(where(is.numeric),
                        round, 3))

        if(input$show_rank){

          data <-
            data %>%
            mutate_at(vars(all_of(vars)),
                      ~ dense_rank(desc(.)
              )
            )

        }

        datatable(
          data %>%
            setnames(.,
                     as.character(variable_names$variable),
                     as.character(variable_names$var_name),
                     skip_absent = TRUE),
          rownames = FALSE,
          filter = 'none',
          options = list(scrollX = TRUE,
                         pageLength = 10,
                         autoWidth = TRUE,
                         dom = "lftipr"))

      })

        # Downloadable rds of selected dataset

        output$download_global_rds <-
          downloadHandler(
            filename = "data.rds",

            content = function(file) {
              write_rds(browse_data(),
                        file)
            }
          )

        # Downloadable csv of selected dataset

        output$download_global_csv <-
          downloadHandler(
            filename = "data.csv",

            content = function(file) {
              write_csv(browse_data(),
                        file,
                        na = "")
            }
          )

        # Downloadable dta of selected dataset

        output$download_global_dta <-
          downloadHandler(
            filename = "data.dta",

            content = function(file) {
              write_dta(browse_data(),
                        file)
            }
          )


   # Report ================================================================================

    output$report <- downloadHandler(

      filename = paste0(
        str_to_lower(input$country),
        "-instutitional-assessment-report.docx"
      ),

      content = function(file) {

        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        params <-
          list(
            base_country = base_country(),
            comparison_countries = input$countries,
            data = data()
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

   # Definitions ===========================================================================

    output$definition <-
      renderTable(

        if(input$family == "Overview"){
          db_variables %>%
            select(Indicator=var_name,Family=family_name,Description=description,Source=source)
        } else {

        db_variables %>%
          filter(family_name == input$family) %>%
          select(Indicator=var_name,Family=family_name,Description=description,Source=source)

        }
      )
      #renderTable(definitions[[input$family]])

    # Download csv with definitions
    output$download_indicators <-
      downloadHandler(
        filename = "Institutional assessment indicators.csv",

        content = function(file) {
          write_csv(db_variables %>% select(indicador=var_name,family=family_name,description,source),
                    file,
                    na = "")
        }
      )

  }

