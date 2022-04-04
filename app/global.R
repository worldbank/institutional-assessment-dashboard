# Packages ----------------------------------------------------------------

library(tidyverse)
library(DT)
library(plotly)
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(bs4Dash)
library(fresh)
library(formattable)

# Data -------------------------------------------------------------

global_data <-
  read_rds(
    file.path(
      "data",
      "closeness_to_frontier.rds"
    )
  ) 