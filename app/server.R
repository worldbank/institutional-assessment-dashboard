
# Load packages --------------------------------------------------------------------------------

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(hrbrthemes)
  library(DT)
  library(plotly)


# Load data sets --------------------------------------------------------------------------------

  #country_data <-
  #  read_csv(file.path("data",
  #                     "mock_country.csv"))

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  data_table <-
    global_data %>%
    ungroup() %>%
    select(-c("lac", "lac6", "structural", "oecd","country_code")) %>%
    mutate(across(where(is.numeric), round, 3))

  source(file.path("auxiliary",
                   "vars-by-family.R"))

  # Function that defines quantiles based on country, comparison and variables
  source(file.path("auxiliary",
                   "fun_quantiles.R"))

# Server ---------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({

      selected_groups  <- input$groups
      selected_country <- input$country

      # Can use character(0) to remove all choices
      if (is.null(selected_groups)) {
        selected <- NULL
      } else {
        selected <-
          country_list %>%
          filter(group_code %in% selected_groups) %>%
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

      # Can also set the label and select items
      updateCheckboxGroupInput(session,
                               "countries",
                               label = NULL,
                               choices = global_data$country_name %>% unique,
                               selected = selected
      )

      req(input$select)

      output$Overview <- renderPlotly({
        plot <-
          ggplot(data = dtf_family_level %>%
                   def_quantiles(
                     selected_country,
                     selected,
                     family_names
                   )
          ) +
          geom_segment(
            aes(x = reorder(variable,-dtf),
                xend = reorder(variable,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(variable,-dtf),
                y = dtf,
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(values = c("Advanced"="#009E73",
                                        "Emerging"="#E69F00",
                                        "Weak"="#D55E00")) +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot)

      })

      output$Labor <- renderPlotly({
        plot <-
          ggplot(data = global_data %>%
                   def_quantiles(
                     selected_country,
                     selected,
                     vars_lab
                   )
          ) +
          geom_segment(
            aes(x = reorder(variable,-dtf),
                xend = reorder(variable,-dtf),
                y = 0,
                yend = dtf,
                color = classification),
            size = 1) +
          geom_point(
            aes(x = reorder(variable,-dtf),
                y = dtf,
                color = classification),
            size = 3)  +
          coord_flip() +
          scale_color_manual(values = c("Advanced"="#009E73",
                                        "Emerging"="#E69F00",
                                        "Weak"="#D55E00")) +
          theme_ipsum() +
          theme(
            panel.grid.minor.y = element_blank(),
            panel.grid.major.y = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          ylab("Distance to frontier") +
          xlab("")

        ggplotly(plot)

      })



    })

    output$dataset <-
      renderDataTable(server = FALSE, {

        vars <-
          input$vars %>%
          map(get) %>%
          unlist

        datatable(data_table %>%
                    select(country_name,
                           all_of(vars)),
                  rownames = FALSE,
                  extensions = 'Buttons',
                  filter = 'top',
                  options = list(scrollX = TRUE,
                                 pageLength = 15,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 dom = "lBtipr",
                                 buttons = c('copy', 'csv', 'excel')))
      })




  }


