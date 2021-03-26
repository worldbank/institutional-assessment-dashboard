
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

  global_data <-
    read_csv(file.path("data",
                       "dtf_vars_global.rds"))

# Server ---------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    output$dataset = renderDataTable({
      global_data
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


