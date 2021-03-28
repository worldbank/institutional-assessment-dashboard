
# Load packages --------------------------------------------------------------------------------

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(hrbrthemes)
  library(DT)
  library(plotly)


# Load data sets --------------------------------------------------------------------------------

  country_data <-
    read_csv(file.path("data",
                       "mock_country.csv"))

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

  global_data <-
    read_rds(file.path("data",
                       "country_dtf.rds"))
  country_list <-
    read_csv(here("data",
                  "data_raw",
                  "wb_country_list.csv")) %>%
    rename(country_name = country)

  data_table <-
    global_data %>%
    select(-c("lac", "lac6", "structural", "oecd")) %>%
    mutate(across(where(is.numeric), round, 3))

  source(file.path("auxiliary",
                   "vars-by-family.R"))

# Server ---------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

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

    output$Overview <- renderPlotly({
      plot <-
        ggplot(data = country_data %>%
                 filter(tab == "Overview")) +
          geom_segment(
            aes(x = reorder(indicator,
                            -value,
                            sum),
                xend = reorder(indicator,
                               -value,
                               sum),
                y = 0,
                yend = value,
                color = category),
            size = 1) +
          geom_point(
            aes(x = reorder(indicator,
                            -value,
                            sum),
                y = value,
                color = category),
            size = 3)  +
          coord_flip() +
          scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00")) +
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
        ggplot(data = country_data %>%
                 filter(tab == "Labor")) +
          geom_segment(
            aes(x = reorder(indicator,
                            -value,
                            sum),
                xend = reorder(indicator,
                               -value,
                               sum),
                y = 0,
                yend = value,
                color = category),
            size = 1) +
          geom_point(
            aes(x = reorder(indicator,
                            -value,
                            sum),
                y = value,
                color = category),
            size = 3)  +
          coord_flip() +
          scale_color_manual(values = c("#009E73", "#E69F00", "#D55E00")) +
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



  }


