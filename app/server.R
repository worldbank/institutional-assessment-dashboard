
# Load packages --------------------------------------------------------------------------------

library(shinydashboard)
library(shiny)
library(shinyjs)
library(tidyverse)
library(hrbrthemes)
library(DT)


# Load data sets --------------------------------------------------------------------------------

  country_data <-
    read_csv("data/mock_country.csv")

# Server ---------------------------------------------------------------------------------------------

  server <- function(input, output, session) {

    output$dataset = renderDataTable({
      country_data
    })

    output$Overview <- renderPlot({
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

    })

    output$Labor <- renderPlot({
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

    })

    output$Law <- renderPlot({
      ggplot(data = country_data %>%
               filter(tab == "Legal")) +
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

    })

    output$Finance <- renderPlot({
      ggplot(data = country_data %>%
               filter(tab == "Finance")) +
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

    })

  }


