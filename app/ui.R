# Load packages ###########################################################################

  library(shinydashboard)
  library(shiny)
  library(shinyjs)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(leaflet)

# Inputs ################################################################################

# Data sets ---------------------------------------------------------------------------

  country_groups <-
    read_rds(file.path("data",
                       "wb_country_groups.rds"))

  country_list <-
    read_rds(file.path("data",
                       "wb_country_list.rds"))

  variable_names <-
    read_rds(file.path("data",
                       "variable_names.rds"))

  # Auxiliary functions -----------------------------------------------------------------

  source(file.path("auxiliary",
                   "vars-by-family.R"))

# UI ###################################################################################

  ui <- navbarPage(

    "Global institutional assessment",

    theme = bs_theme(bootswatch = "minty"),

    # About page ---------------------------------------------
    tabPanel(
      "Home",
      useShinyjs(),

      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
      ),

      tags$div(
        class = "container",

        h3("About"),

        p("The World Bank recognizes institutional strengthening as key ingredient for progress of its members countries along income categories. While there are numerous diagnostic and assessment tools for specific functional areas such as financial management and tax administration, there is no analytical tool for country-level institutional assessment."),
        p("The Institutional Assessment (IA) benchmarking aims at partially filling this gap by providing a standard methodology to summarize information from a large set of institutional indicators. This dashboard offers a user-friendly interface that can easily and speedily be used for the country-level IA benchmarking."),
        p("The dashboard provides a visualization of a country’s profile based on a set of international institutional indicators, highlighting a given country’s institutional strengths and weaknesses. It is recommended to use these empirical results as guides for further quantitative or qualitative in-depth analysis in the specific areas of interest."),

        h3("How to use this dashboard"),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit")

      )
    ),

    # Benchmarking tab ===================================================================================================
    tabPanel("Country benchmarking"
    ),

    # Map tab ===============================================================================================================

    tabPanel("World map"
    ),

    # Data tab ===============================================================================================================

    tabPanel("Browse the data"
    ),

    # Methodology tab ===============================================================================================================

    tabPanel("Methodology"
    )

  ) # closing dashboard page
