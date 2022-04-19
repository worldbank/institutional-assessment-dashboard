# UI ###########################################################################

ui <-
  dashboardPage(

    freshTheme = create_theme(bs4dash_layout(sidebar_width = "400px")),

    ## Header ------------------------------------------------------------------

    dashboardHeader(

      title = dashboardBrand(
        title = "Global Benchmarking Institutions Dashboard ",
      ),
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE

    ),

    ## Navigation menu ---------------------------------------------------------
    dashboardSidebar(

      status = "info",
      skin = "light",
      elevation = 5,

      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Country benchmarking", tabName = "benchmark", icon = icon("sort-amount-up")),
        menuItem("Cross-country comparison", tabName = "country", icon = icon("chart-bar")),
        menuItem("Bivariate correlation", tabName = "scatter", icon = icon("search-dollar")),
        menuItem("World map", tabName = "world_map", icon = icon("globe-americas")),
        menuItem("Time trends", tabName = "trends", icon = icon("chart-line")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Methodology", tabName = "methodology", icon = icon("book")),
        menuItem("FAQ", tabName = "faq", icon = icon("question"))
      )
    ),

    dashboardBody(
      tabItems(

        ## Landing page --------------------------------------------------------

        tabItem(
          tabName = "home",

          bs4Card(
            width = 12,
            status = "navy",
            solidHeader = TRUE,
            title = 
              span(
                img(src = "cliar.png", width = "80%")
              ),

            br(),
            p("The World Bank recognizes institutional strengthening as key ingredient for progress of its members countries along income categories. While there are numerous diagnostic and assessment tools for specific functional areas such as public financial management and tax administration, there is no analytical tool for country-level institutional assessment."),
            p("The Global Benchmarking Institutions Dashboard (G-BID) contributes to fill this gap by providing a standard methodology to summarize information from a large set of country-level institutional indicators."),
            p("The dashboard provides a user-friendly interface with multiple visualizations of a country’s institutional profile based on a set of international indicators, highlighting a given country’s institutional strengths and weaknesses relative to a set of country comparators. The findings of the G-BID can provide a structured and up-to-date empirical guidance for further in-depth analysis in the specific areas of interest, given the nature of the World Bank engagement in a country and/or complementarity with other ongoing country-level diagnostics (SCDs, CEMs, CPFs and the like)."),
            p("The G-BID is part of a larger analytical effort to assess and review the quality of country’s institutions. For full details about the broader analytical effort, see the Approach paper: Marco Larizza, Serena Sara Daniela Cocciolo, Eric Braian Arias, Peter Siegenthaler and Jim Brumby (forthcoming),  ",
              tags$em("Country Level Institutional Assessment and Review (CLIAR)"),
              "Users of this resource should cite this approach paper. Further, any publications using data drawn from the G-BID should include a citation of the dashboard as well as the original source(s) of the data used. Citation information for each component dataset is included in the methodology page."),

            h3("How to use this dashboard"),
            p("This dashboard aims to enable its users to interact with the country-level benchmarking through the following tabs:"),
            tags$ul(
              tags$li(
                "The ",
                tags$b("country benchmarking"),
                "tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator and institutional cluster. 
                It works best with a relatively large group of comparator countries."
              ),
              tags$li(
                "The ",
                tags$b("cross-country comparison "),
                "tab shows how one country compares to another group of countries for each relevant indicator. 
                It works even with a few comparator countries."
              ),
              tags$li(
                "The",
                tags$b("bivariate correlation"),
                "tab shows correlations between the closeness to frontier scores for pairs of indicators"
              ),
              tags$li(
                "The ",
                tags$b("world map"),
                "tab shows the closeness to frontier of a given indicator for all countries with available data."
              ),
              tags$li(
                "The ",
                tags$b("time trends"),
                "tab shows the evolution year by year of multiple indicators."
              ),
              tags$li(
                "The ",
                tags$b("data"),
                "tab provides an interactive table containing the closeness to frontier data for all countries.
                It also allows users to download the data in different formats."
              ),
              tags$li(
                "The ",
                tags$b("methodology"),
                "tab includes metadata on the indicators, country groups and methods used in the analysis, and FAQs."
              )
            )
          )
        ),

        ## Country benchmark tab -------------------------------------------------------

        tabItem(
          tabName = "benchmark",
          useShinyjs(),

          bs4Card(
            title = "Select information to display",
            status = "success",
            solidHeader = TRUE,
            width = 12,

            fluidRow(

              column(
                width = 3,
                pickerInput(
                  "country",
                  label = "Select a base country",
                  choices = countries,
                  selected = "Uruguay",
                  multiple = FALSE,
                  options = list(
                    size = 20,
                    `actions-box` = TRUE
                  )
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "groups",
                  label = "Select comparison groups",
                  choices = group_list,
                  selected = c("OECD members"),
                  multiple = TRUE,
                  options = list(
                    size = 21,
                    `actions-box` = TRUE
                  )
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "family",
                  label = "Select institutional family",
                  choices = c(
                    "Overview",
                    names(definitions)
                  ),
                  selected = "Overview"
                )
              ),
              
              column(
                width = 3,
                pickerInput(
                  inputId = "benchmark_median",
                  label = "Show group median",
                  choices = append(
                    "Comparison countries",
                    group_list
                  ),
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `live-search` = TRUE,
                    maxOptions = 3
                  )
                )
              )

            ),

            fluidRow(

              column(
                width = 3,
                prettyCheckbox(
                  inputId = "benchmark_dots",
                  label = "Show comparison countries",
                  value = FALSE,
                  icon = icon("check"),
                  status = "success"
                )
              ),
              
              column(
                width = 3,
                uiOutput(
                  "select_button"
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
            )

          ),

          bs4Card(
            title = "Select individual comparison countries",
            width = 12,
            status = "success",
            collapsed = TRUE,

            checkboxGroupButtons(
              inputId = "countries",
              individual = TRUE,
              label = NULL,
              choices = countries,
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon")
              )
            )
          ),

          bs4Card(
            title = NULL,
            collapsible = FALSE,
            width = 12,

            conditionalPanel(
              "input.select !== 0",
              fluidRow(

                column(
                  width = 12,
                  plotlyOutput(
                    "plot",
                    height = paste0(plot_height * .8, "px")
                  )
                )

              )

            )

          ),

          bs4Card(
            title = "Indicator definitions",
            collapsible = TRUE,
            collapsed = FALSE,
            status = "secondary",
            solidHeader = TRUE,
            width = 12,

            tableOutput('definition')
          )
        ),

        ## Country comparison ----------------------------------------------------

        tabItem(
          tabName = "country",

          bs4Card(
            title = "Select information to display",
            status = "success",
            solidHeader = TRUE,
            width = 11,

            fluidRow(

              column(
                width = 4,
                pickerInput(
                  "vars_bar",
                  label = "Select indicator",
                  choices = variable_list,
                  selected = "Capital controls",
                  options = list(
                    size = 20,
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    maxOptions = 3
                  ),
                  width = "100%"
                )
              ),

              column(
                width = 2,
                pickerInput(
                  "country_bar",
                  label = "Select a base country",
                  choices = countries,
                  selected = "Uruguay",
                  multiple = FALSE
                )
              ),

              column(
                width = 3,
                pickerInput(
                  inputId = "countries_bar",
                  label = "Select comparison countries",
                  choices = countries,
                  selected = c("Brazil", "Argentina", "Paraguay", "Austria"),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE)
                )
              ),

              column(
                width = 3,
                pickerInput(
                  inputId = "groups_bar",
                  label = "Select comparison groups",
                  choices = group_list,
                  selected = NULL,
                  multiple = TRUE
                )
              )
            )
          ),

          bs4Card(
            width = 11,
            solidHeader = FALSE,
            gradientColor = "primary",
            collapsible = FALSE,
            plotlyOutput(
              "bar_plot",
              height = paste0(plot_height, "px")
            )
          )
        ),

        ## Bivariate correlation ----------------------------------------------------

        tabItem(
          tabName = "scatter",

          bs4Card(
            title = "Select indicators to visualize",
            status = "success",
            solidHeader = TRUE,
            width = 11,

            fluidRow(

              column(
                width = 3,
                pickerInput(
                  "country_scatter",
                  label = "Select a base country",
                  choices = countries,
                  selected = "Uruguay",
                  multiple = FALSE,
                  options = list(
                    size = 20,
                    `actions-box` = TRUE
                  )
                )
              ),
              
              column(
                width = 3,
                pickerInput(
                  "y_scatter",
                  label = "Select indicator for Y axis",
                  choices = append(
                    "Log GDP per capita, PPP",
                    variable_list
                  ),
                  selected = "Central bank independence",
                  options = list(
                    `live-search` = TRUE,
                    size = 20,
                    title = "Click to select family or indicator"
                  ),
                  width = "100%"
                )
              ),
              
              column(
                width = 3,
                pickerInput(
                  "x_scatter",
                  label = "Select indicator for X axis",
                  choices = append(
                    "Log GDP per capita, PPP",
                    variable_list
                  ),
                  selected = "Log GDP per capita, PPP",
                  options = list(
                    `live-search` = TRUE,
                    size = 20,
                    title = "Click to select family or indicator"
                  ),
                  width = "100%"
                )
              ),
              
              column(
                width = 3,
                pickerInput(
                  "high_group",
                  label = "Highlight a group",
                  choices = append(
                    "No highlight",
                    group_list
                  ),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    `live-search` = TRUE,
                    `actions-box` = TRUE,
                    size = 18
                  ),
                )
              )


            )
          ),

          bs4Card(
            width = 11,
            solidHeader = FALSE,
            gradientColor = "primary",
            collapsible = FALSE,
            plotlyOutput(
              "scatter_plot",
              height = paste0(plot_height, "px")
            )
          )
        ),

        ## Trends  tab ------------------------------------------------------------


        tabItem(
          tabName = "trends",

          box(
            width = 11,
            solidHeader = TRUE,
            title = "Select indicator to visualize",
            status = "success",
            collapsible = TRUE,

            fluidRow(
              column(
                width = 3,
                pickerInput(
                  "indicator_trends",
                  label = "Select indicator to visualize",
                  choices = variable_list,
                  selected = "Capital controls",
                  options = list(
                    `live-search` = TRUE,
                    size = 21,
                    title = "Click to select family or indicator"
                  ),
                  width = "100%"
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "country_trends",
                  label = "Select a base country",
                  choices = countries,
                  selected = "Uruguay",
                  multiple = FALSE,
                  options = list(
                    size = 23
                  )
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "group_trends",
                  label = "Select comparison groups",
                  choices = group_list,
                  selected = c("OECD members", "Latin America & Caribbean"),
                  multiple = TRUE,
                  options = list(
                    maxOptions = 5,
                    `live-search` = TRUE,
                    size = 21
                  )
                )
              ),

              column(
                width = 3,
                pickerInput(
                  "countries_trends",
                  label = "Select comparison countries",
                  choices = countries,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    size = 22,
                    title = "Click to select family or indicator"
                  )
                )
              )
            )
          ),

          bs4Card(
            width = 11,
            solidHeader = FALSE,
            gradientColor = "primary",
            collapsible = FALSE,

            conditionalPanel(
              "input.indicator_trends !== ''",
              plotlyOutput(
                "time_series",
                height = paste0(plot_height, "px")
              )
            )
          )
        ),

        ## Map  tab ------------------------------------------------------------


        tabItem(
          tabName = "world_map",

          box(
            width = 11,
            solidHeader = TRUE,
            title = "Select information to display",
            status = "success",
            collapsible = TRUE,

            fluidRow(
              column(
                width = 5,
                pickerInput(
                  "vars_map",
                  label = "Select indicator",
                  choices = variable_list,
                  selected = "Capital controls",
                  options = list(
                    `live-search` = TRUE,
                    size = 20,
                    title = "Click to select family or indicator"
                  ),
                  width = "100%"
                )
              )
              # ,
              # 
              # column(
              #   width = 3,
              #   pickerInput(
              #     "data",
              #     label = "Select data",
              #     choices = c("Closeness to frontier",
              #                 "Raw indicator (average of last 7 years)"),
              #     selected = "Closeness to frontier"
              #   )
              # )
            )
          ),

          bs4Card(
            width = 11,
            solidHeader = FALSE,
            gradientColor = "primary",
            collapsible = FALSE,

            conditionalPanel(
              "input.vars_map !== ''",
              plotlyOutput(
                "map",
                height = paste0(plot_height, "px")
              )
            )
          )
        ),

        ## Data tab --------------------------------------------------------------------

        tabItem(
          tabName = "data",

          fluidRow(
            bs4Card(

              fluidRow(

                column(
                  width = 5,
                  radioGroupButtons(
                    "data",
                    label = "Select a dataset",
                    choices = c("Closeness to frontier",
                                "Compiled indicators"),
                    selected = "Closeness to frontier",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok",
                                 lib = "glyphicon")
                    )
                  )
                ),

                column(
                  width = 4,
                  pickerInput(
                    "vars",
                    label = "Select institutional families to include",
                    choices = names(definitions),
                    selected = names(definitions),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  )
                ),

                column(
                  width = 3,
                  materialSwitch(
                    inputId = "show_rank",
                    label = "Show rank instead of indicator values",
                    value = FALSE,
                    status = "success"
                  )
                )

              ),

              title = "Select information to display",
              status = "success",
              width = 9,
              collapsed = TRUE


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

          dataTableOutput("benchmark_datatable")

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
            title = "List of indicators",

            p("The indicators used to benchmark the institutional families are extracted from multiple public data sources.
                            For a full list of the indicators used, their sources, and their definitions, download the metadata below."),
            downloadButton("download_indicators",
                           "Download indicator definitions",
                           style = "background-color: #204d74; color: white")
          )
        ),
        
## FAQ tab =====================================================================
        
        tabItem(
          tabName = "faq",

          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Does the G-BID collect new data on governance and institutions?",
            p(
              "NO. 
              The dashboard extracts data from original sources and collects international indicators that are publicly available and have been widely tested and used as reliable proxies to measure country-level governance and institutions."
            )
          ),
          
      
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Can I add my own indicators to the dashboard and run the analysis including these indicators? ",
            p(
              "NO. 
              You cannot add indicators to the dashboard. 
              However, you can download the full database and augment it with additional indicators to customize the analysis. 
              You can also get in touch with the G-BID coordinator (scocciolo@worldbank.org) indicating which data you would like to be added in the database, and for which cluster. 
              Each request will be reviewed by a team of technical experts and if the indicator meets the selection criteria indicated in the methodological note (quality and coverage) it will be added to the G-BID."
            )
          ),
              
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Is the “Closeness to Frontier” methodology the same one used in the “Doing Business Report”?",
            p(
              "The “Closeness to Frontier” is used in order to standardize indicators and make it possible to compare and aggregate them. 
              The resulting scores range between 0 and 1 and we labeled them “Closeness to Frontier” because higher values mean closer to the frontier, which is set at 1. 
              It is similar to the transformation that was used in the “Doing Business Reports”."
            )
          ),
              
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "What does the traffic coloring mean? Is there a methodological foundation?",
            p(
              "The results from the institutional benchmarking are relative for 
              a given country of interest vis a vis a chosen set of comparator countries. 
              Using the distribution of the CTF scores in the set of comparator countries, 
              we identify the score range for the bottom 25% of comparators, 
              the score range for the 25%-50% group and the score range for the top 50% of comparators.
              Given the CTF score of the country of interest, 
              we identify whether the country of interest for the analysis belong to the bottom, 
              middle or top group. 
              These percentile groups are used because they are simple, intuitive and standards. 
              The sensitivity analysis shows that most of the results are not sensitive to this choice,
              and for example would be robust if the grouping 0-33%, 33%-66%, 66%+ would be used."
            )
          ),
          
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Why the length of the bar is different? Why a red bar is longer than another red bar, if they are both red?",
            p(
              "Using the distribution of the CTF scores in the set of comparator countries, 
              we identify the score range for the bottom 25% of comparators, 
              the score range for the 25%-50% group and the score range for the top 50% of comparators. 
              The red bar represents the score range for the bottom 25% of comparators. 
              While the CTF scores always range between 0 and 1, 
              the length of the red bar varies across indicators depending on the distribution of the CTF scores in the comparator group. 
              As an illustration, for a given set of comparator countries, 
              for a given indicator the CTF scores in the bottom 25% of comparators may range between 0 and 0.2, 
              while for another indicator it may range between 0 and 0.5."
            )
          ),

          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "How do you deal with missing data for certain indicators and for certain countries?",
            p(
              "We deal with missing data in various ways. 
              First, the benchmarking analysis uses the average of indicators over recent years. 
              Conceptually, governance and institutional indicators are expected to show limited yearly variations. 
              This helps in reducing data gaps. 
              Second, we only include in the institutional benchmarking the indicators that 
              are non-missing for the country of interest. 
              Third, we only include in the institutional benchmarking the indicators 
              that are non-missing for at least 70% of the countries in the comparator group. 
              The average CTF scores at institutional cluster level are calculated 
              as averages of the CTF scores of the indicators in that clusters, 
              but only for the indicators that meet these criteria above. 
              This ensures that, 
              for each pair of country of interest and group of comparator countries, 
              the average CTF scores are calculated from the same indicators."
            )
          ),
          
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Why do I have to choose at least 10 comparator countries for the benchmarking analysis?",
            p(
              "The percentile analysis identifies whether the performance of the 
              country of interest in a given indicator or institutional cluster 
              belongs to the bottom 25%, the 25%-50% group or the top 50% of 
              the comparator countries. 
              This percentile analysis can be meaningfully performed only if 
              there is sufficient number of comparator countries.
              Given the definition of the traffic coloring,
              it is recommended to use a group of comparator countries which is aspirational."
            )
          ),
              
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "How do you choose the comparator countries/groups?",
            p(
              "It depends on the purpose of the analysis and the country context. 
              For example, if this analysis is used in the SCDs, it is recommendable to use the regional, 
              aspirational and structural peers identified for the SCD."
            )
          ),
          
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Can I download the raw data for my own research/analytical purposes?",
            p(
              'YES. The full compiled database, once updated, is available in the "Data" tab for download.
              Both the "Closeness to Frontier" scores and the full database with yearly indicators are available for download,
              and therefore users can easily verify the latest year available for each indicator.'
            )
          ),
          
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "How often is the G-BID updated? How do I know that the G-BID uses the latest available data?",
            p(
              'It is currently planned that the G-BID will be updated once or twice a year, 
              depending on demands and usage. 
              The G-BID is programmed so that the data extraction from the data sources (primarily gov360) 
              is automated through APIs, 
              therefore with minimal maintenance costs for the indicators already 
              included in the dashboard and with stable APIs. The full compiled database, 
              once updated, is available in the "Data" tab for download. 
              Both the "Closeness to Frontier"" scores and the full database with 
              yearly indicators are available for download, 
              and therefore users can easily verify the latest year available for each indicator.'
            )
          ),
          
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "How were the indicators included in G-BID selected?",
            p(
              'The indicators included in the G-BID were selected following two criteria:
              geographical coverage and quality. 
              This list was defined based on initial internal reviews, 
              and will be further refined based on inputs recently received by 
              sector experts and from the experiences of country teams in applying this tool. 
              For example, the team is currently considering expanding the existing 
              database in order to include indicators from additional data sources, 
              such as PEFA, Tax DIAMOND and Regional Barometers. 
              The list of indicators used in the G-BID will be periodically 
              reviewed in order to include new governance and institutions indicators 
              that may be become available in the future. 
              The G-BID is a "live tool".'
            )
          ),
          
          box(
            width = 11,
            status = "navy",
            collapsed = TRUE,
            title = "Is the G-BID available to external users (i.e non-bank staff) ?",
            p(
              "As of now, the G-BID is not available for external users.
              The team will consider making the dashboard publicly available it has been tested and validated through a
              few pilots, and depending on demands and usage."
            )
          )
          
        ) # Close FAQ tab
      )
    )
  )
