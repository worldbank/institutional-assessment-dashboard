library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(bs4Dash)
library(fresh)
library(tidyverse)

country_groups <-
  read_rds(file.path("data",
                     "wb_country_groups.rds"))

country_list <-
  read_rds(file.path("data",
                     "wb_country_list.rds"))

definitions <-
  read_rds(file.path("data",
                     "indicator_definitions.rds"))

# UI ###########################################################################

ui <-
  dashboardPage(

    freshTheme = create_theme(bs4dash_layout(sidebar_width = "400px")),

    dashboardHeader(

      title = dashboardBrand(
        title = "Global Institutional Benchmarking Dashboard ",
      ),
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE

    ),

    dashboardSidebar(

      status = "info",
      skin = "light",
      elevation = 5,

      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Country benchmarking", tabName = "country", icon = icon("sort-amount-up")),
        menuItem("Aggregation of preferences", tabName = "heatmap", icon = icon("comments")),
        menuItem("Map & trends", tabName = "viz", icon = icon("chart-bar")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Methodology", tabName = "methodology", icon = icon("book"))
      )
    ),

    dashboardBody(
      tabItems(

        tabItem(
          tabName = "home",

          userBox(width = 12,
                  status = "navy",

                  title = userDescription(
                    title = h2("Global Institutional Benchmarking Dashboard"),
                    type = 1,
                    image = "world-bank-logo.png"
                  ),

                  br(),
                  p("The World Bank recognizes institutional strengthening as key ingredient for progress of its members countries along income categories. While there are numerous diagnostic and assessment tools for specific functional areas such as public financial management and tax administration, there is no analytical tool for country-level institutional assessment."),
                  p("The Global Institutional Benchmarking Dashboard (GIBD) contributes to fill this gap by providing a standard methodology to summarize information from a large set of country-level institutional indicators."),
                  p("The dashboard provides a user-friendly interface with multiple visualizations of a country’s institutional profile based on a set of international indicators, highlighting a given country’s institutional strengths and weaknesses relative to a set of country comparators. The findings of the GIBD can provide a structured and up-to-date empirical guidance for further in-depth analysis in the specific areas of interest, given the nature of the World Bank engagement in a country and/or complementarity with other ongoing country-level diagnostics (SCDs, CEMs, CPFs and the like)."),
                  p("The GIBD part of a larger analytical effort to assess and review the quality of country’s institutions. For full details about the broader analytical effort, see the Approach paper: Marco Larizza, Serena Sara Daniela Cocciolo, Eric Braian Arias, Peter Siegenthaler and Jim Brumby (forthcoming),  ",
                    tags$em("Country Level Institutional Assessment and Review (CLIAR): a 3-steps analytical framework."),
                    "Users of this resource should cite this approach paper. Further, any publications using data drawn from the GIBD should include a citation of the dashboard as well as the original source(s) of the data used. Citation information for each component dataset is included in the methodology page."),

                  h3("How to use this dashboard"),
                  p("This dashboard aims to enable its users to interact with the country-level IA benchmarking through the following tabs:"),
                  tags$ul(
                    tags$li("The ",
                            tags$b("country benchmarking"),
                            "tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator."

                    ),
                    tags$li("The ",
                            tags$b("world map"),
                            "tab shows the closeness to frontier of a given indicator for all countries with available data."

                    ),
                    tags$li("The ",
                            tags$b("trends"),
                            "tab shows the evolution year by year of multiple indicators."

                    ),
                    tags$li("The ",
                            tags$b("aggregation of preferences"),
                            "tab shows the prioritization matrix where the coloring reflects the institutional areas in need of development or emerging."

                    ),
                    tags$li("The ",
                            tags$b("browse the data"),
                            "tab provides an interactive table containing the closeness to frontier data for all countries.
                  It also allows users to download the data in different formats."

                    )
                    ,
                    tags$li("The ",
                            tags$b("methodology"),
                            "tab includes metadata on the indicators, country groups and methods used in the analysis."

                    )
                  )
          )
        ),

## Country benchmark tab -------------------------------------------------------

        tabItem(
          tabName = "country",

          bs4Card(

            fluidRow(

              column(
                width = 2,
                pickerInput(
                  "country",
                  label = "Select a base country",
                  choices = c("", country_list$country_name %>% unique %>% sort),
                  selected = "Uruguay",
                  multiple = FALSE
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "groups",
                  label = "Select comparison groups",
                  choices = country_groups$group_name,
                  selected = c("OECD members", "Latin America & Caribbean"),
                  multiple = TRUE
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "family",
                  label = "Select institutional family",
                  choices = c("Overview",
                              names(definitions)
                  ),
                  selected = "Overview"
                )
              ),

              column(
                width = 1,
                actionButton(
                  "select",
                  "Apply selection",
                  icon = icon("check"),
                  class = "btn-success",
                  width = "100%"
                )
              )
            ),

            fluidRow(

              box(
                title = "Select individual comparison countries",
                status = "secondary",
                width = 9,
                collapsed = TRUE,
                collapsible = TRUE,
                checkboxGroupButtons(
                  inputId = "countries",
                  individual = TRUE,
                  label = NULL,
                  choices = country_list$country_name %>% unique %>% sort,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-check-square",
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-square-o",
                                style = "color: steelblue")
                  )
                )
              ),

              column(
                width = 3,
                downloadButton(
                  "report",
                  "Download editable report",
                  style = "width:100%; background-color: #204d74; color: white"
                )

              )
            ),



            # inputId = NULL,
            title = "Select information to be displayed",
            # footer = NULL,
            status = "success",
            # elevation = NULL,
            solidHeader = TRUE,
            # headerBorder = TRUE,
            gradientColor = "lightblue",
            width = 12
            # height = NULL,
            # collapsible = TRUE,
            # collapsed = FALSE,
            # closable = FALSE,
            # maximizable = FALSE,
            # cardLabel = NULL,
            # dropdownMenu = NULL,
            # overflow = FALSE,
            # sidebar = NULL
          )
        ),

## Data tab --------------------------------------------------------------------

        tabItem(
          tabName = "data",

          bs4Card(

            fluidRow(

              column(
                width = 3,
                shinyWidgets::radioGroupButtons(
                  "data",
                  label = "Select a dataset",
                  choices = c("Closeness to frontier",
                              "Compiled indicators"),
                  justified = TRUE
                )
              ),

              column(
                width = 4,
                pickerInput(
                  "family",
                  label = "Select institutional families to include",
                  choices = names(definitions),
                  selected = names(definitions),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                )
              ),

              column(
                width = 2,
                materialSwitch(
                  inputId = "show_rank",
                  label = "Show rank variables",
                  value = FALSE,
                  status = "success"
                )
              ),

              column(
                width = 1,
                downloadButton(
                  "download_global_rds",
                  ".rds",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              ),

              column(
                width = 1,
                downloadButton(
                  "download_global_csv",
                  ".csv",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              ),

              column(
                width = 1,
                downloadButton(
                  "download_global_dta",
                  ".dta",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              )
            ),

            status = "success",
            width = 12,
            collapsible = FALSE

          )
        ),

## Methodology tab -------------------------------------------------------------

        tabItem(
          tabName = "methodology",

          box(
            width = 11,
            status = "navy",
            title = "Institutional families",

            p("The dashboard uses established well-institutional indicators, clustered into nine main institutional families:",
              tags$ul(
                tags$li("Anti-corruption, transparency and accountability institutions"),
                tags$li("Business environment and trade institutions"),
                tags$li("Financial institutions"),
                tags$li("SOE Corporate Governance"),
                tags$li("Labor market institutions"),
                tags$li("Legal institutions"),
                tags$li("Political institutions"),
                tags$li("Public sector institutions"),
                tags$li("Social institutions")
              )
            ),
            p("There is no agreed theoretical framework that could guide the categorization process. The proposed families are based on an effort to capture key functions that different institutions perform. In so doing, the categorization process faces a trade-off between aggregation and narrowness, where the categories ought to be broad enough to capture enough indicators and policy spaces, but narrow enough to guide a deep qualitative analysis as well as a fruitful and engaged conversation with the country."),
            p('All country-level indicators can be downloaded in the “Browse data” tab.')
          ),

          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Closeness to frontier",

            p('The dashboard uses the “Closeness to Frontier” methodology, which is adapted from the Doing Business’s',
              a("Distance to Frontier methodology",
                href = "https://www.doingbusiness.org/content/dam/doingBusiness/media/Annual-Reports/English/DB17-Chapters/DB17-DTF-and-DBRankings.pdf"),
              '. The CTF methodology allows to assess country’s performance across institutional indicators by comparing it with the “global frontier”, where the global frontier is the world’s best performer. For each indicator, a country’s performance is rescaled on a 0-1 scale using the linear transformation (worst–y)/(worst–frontier), where 1 represents the best performer and 0 the worst performer. The higher the score, the closer a country is to the best performer and the lower the score, the closer a country is to the worst performer, and more distant to the frontier. The best and worst performers are identified using available data from the global sample (i.e., considering all countries for which data is available) since 2013. Thus, a country may set the frontier for an indicator even though it is no longer at the frontier in the most recent year for which the indicator is available.'),
            p('For each institutional family, the CTF scores obtained for each indicator are aggregated through simple averaging into one CTF score at family level. This captures the overall performance for an institutional family relatively to the “global frontier”, while the performance across the indicators will help identify the most challenging areas for institutional strengthening.')

          ),

          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Percentile analysis and comparator countries",

            p('The CTF scores compare the country’s performance with the best and worst performers at global level. However, how does the country compare relatively to a set of chosen comparators?'),
            p('The dashboard uses percentile distribution and traffic light coloring to capture the areas where the largest institutional gaps exist, ',
              HTML('<b>relative to the set of country comparators</b>'),
              '. Relative institutional weaknesses and strengths are defined based on the percentile in which each country indicator belongs. This methodology requires teams to make an informed decision on the set of comparator countries used for the benchmarking, since institutional weaknesses and strengths are identified relatively to those comparator countries.'),
            p('The “Closeness to Frontier” (length of the bar) and the percentile analysis (color of the bar) capture two related but different performance dimensions. The CTF compares the country’s performance with the best and worst performers. The percentile analysis benchmarks the country’s performance with all the set of other comparator countries. For example, it could be that for one indicator or institutional cluster the CTF score is relatively high and close to 1 (indicating in fact ‘closeness to the frontier’) but, at the same time, this dimension is marked as an institutional weakness (red coloring) because the country’s performance is still worse than the majority of comparator countries.'),
            p('The percentile analysis effectively drops those indicators whose distribution precludes this percentile classification (i.e., low variance).')
          ),

          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title ="Country group definitions",

            p(
              "Country group definitions are extracted from the",
              a(
                "World Bank Country and Lending Groups.",
                href = "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"
              ),
              "It classifies all 189 World Bank member countries, and all other economies with populations of more than 30,000.",
              "Economies are divided among income groups according to 2019 gross national income (GNI) per capita,",
              "calculated using the World Bank Atlas method."
            ),
            p(
              "The groups are:",
              tags$ul(
                tags$li(HTML("<b>Low income:</b> $1,035 or less")),
                tags$li(HTML("<b>Lower middle income:</b> $1,036 - 4,045")),
                tags$li(HTML("<b>Upper middle income:</b> $4,046 - 12,535")),
                tags$li(HTML("<b>High income:</b> $12,536 or more"))
              )
            ),
            p(
              HTML("The term <i>country</i>, used interchangeably with <i>economy,</i>"),
              "does not imply political independence but refers to any territory for which authorities report separate social or economic statistics.",
              "Income classifications set on 1 July 2020 remain in effect until 1 July 2021.",
              "Argentina, which was temporarily unclassified in July 2016 pending release of revised national accounts statistics,",
              "was classified as upper middle income for FY17 as of 29 September 2016 based on alternative conversion factors."
            )
          ),


          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "List of indicators",

            p("The indicators used to benchmark the institutional families are extracted from multiple public data sources.
                            For a full list of the indicators used, their sources, and their definitions, download the metadata below."),
            downloadButton("download_indicators",
                           "Download indicator definitions",
                           style = "background-color: #204d74; color: white")
          )
        )
      )
    )
  )

server <- function(input, output) { }

shinyApp(ui, server)

