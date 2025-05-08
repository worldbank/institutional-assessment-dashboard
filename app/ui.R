# UI ###########################################################################

# -----------------------------------------------------------------------------
# CLIAR Benchmarking Dashboard - User Interface Script (ui.R)
# -----------------------------------------------------------------------------
# Overview:
# This script defines the User Interface (UI) for the CLIAR Benchmarking Dashboard, 
# a comprehensive tool developed to facilitate the analysis and visualization 
# of country-level institutional indicators. The dashboard serves as a resource 
# for policymakers, researchers, and stakeholders interested in benchmarking 
# institutional performance globally.

# Purpose:
# The CLIAR Benchmarking Dashboard provides a standardized methodology to:
# - Summarize information from diverse country-level institutional indicators.
# - Facilitate country-level institutional comparisons.
# - Offer dynamic data visualization and interactive analysis tools.

# Functionality:
# The dashboard consists of multiple tabs designed to provide users with various 
# tools for interacting with CLIAR data, including:
# 1. **Country Benchmarking**: Compare a country's closeness to frontier scores 
#    against a group of comparator countries.
# 2. **Cross-Country Comparison**: Perform direct indicator-level comparisons 
#    across countries.
# 3. **Bivariate Correlation**: Visualize correlations between indicators.
# 4. **World Map**: Explore global patterns in closeness to frontier scores.
# 5. **Time Trends**: Analyze year-on-year changes in indicators.
# 6. **Data**: Access and download the dataset.
# 7. **Methodology & User Guide**: Understand the methods, metadata, and FAQs.
# 8. **Terms of Use and Disclaimers**: Review the legal and citation information.
# 9. **FAQ**: Address commonly asked questions about CLIAR.
# 10. **Contact Us**: Connect with the CLIAR team for inquiries.
# 11. **Source Code**: Access the open-source GitHub repository.

# Key Features:
# - Dynamic visualizations to simplify institutional indicator analysis.
# - Downloadable data in multiple formats.
# - Comprehensive metadata and user guide for clarity on methodologies.
# - Integrated disclaimers and citation guidelines for transparency.

# Disclaimer:
# The findings, interpretations, and conclusions expressed in CLIAR are those of 
# World Bank staff and do not necessarily represent the views of the World Bank, 
# its affiliated organizations, or its member governments.

# Usage:
# To launch the dashboard, ensure all dependencies are installed and execute 
# the server script along with this UI script in an R environment or deploy it 
# on a Shiny Server.



js <- "
var mytips = ['0-25, 25-50, >50', '0-33, 33-66, >66'];
$('#threshold').on('shown.bs.select', function() {
  var $lis = $($(this).data('selectpicker').selectpicker.current.elements);
  $lis.each(function(i) {
    $(this).attr('title', mytips[i]);
  });
});"




ui <-
  dashboardPage(
    
    
    freshTheme = create_theme(bs4dash_layout(sidebar_width = "350px")),
    
    ## Header ------------------------------------------------------------------
    
    dashboardHeader(
      
      title = dashboardBrand(
        title = "CLIAR Benchmarking Dashboard"
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
        menuItem("Methodology & User Guide", tabName = "methodology_ug", icon = icon("book")),
        menuItem("Publications", tabName = "pubs", icon = icon("list")),
        menuItem("Terms of use and Disclaimers", tabName = "terms", icon = icon("handshake")),
        menuItem("FAQ", tabName = "faq", icon = icon("question")),
        menuItem("Contact Us", 
                 icon = icon("comments", lib = "font-awesome"),
                 href = "mailto:CLIAR@worldbank.org"),
        menuItem("Source code", 
                 icon = icon("github", lib = "font-awesome"),
                 href = "https://github.com/worldbank/institutional-assessment-dashboard/")
      )
    ),
    
    dashboardBody(
      
      cicerone::use_cicerone(),
      
      ## universal css styles
      tags$head(includeCSS("www/styles.css")),
      
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
            p("Welcome to the Country Level Institutional Assessment and Review (CLIAR) Benchmarking Dashboard!"), 
            
            p("The CLIAR Benchmarking Dashboard provides a standard quantitative methodology to summarize information from a large set of country-level institutional indicators."),
            
            p("For full details about the methodology behind the CLIAR Benchmarking, please find the Methodological paper in the Methodology tab. Users of this resource should cite this paper. Publications using the CLIAR data should include a citation of the CLIAR Dashboard as well as the original source(s) of the data used. Citation information for each component dataset is also included in the Methodology page."),
            h3("How to use this dashboard"),
            p("This dashboard enables its users to interact with the CLIAR benchmarking through the following tabs:"),
            tags$ul(
              tags$li(
                "The ",
                tags$b("Country Benchmarking"),
                "tab shows how one country compares to another group of countries in terms of closeness to frontier for each relevant indicator and institutional cluster.
                It works best with a relatively large group of comparator countries."
              ),
              tags$li(
                "The ",
                tags$b("Cross-Country Comparison "),
                "tab shows how one country compares to another group of countries for each relevant indicator.
                It works even with a few comparator countries."
              ),
              tags$li(
                "The",
                tags$b("Bivariate Correlation"),
                "tab shows correlations between the closeness to frontier scores for pairs of indicators"
              ),
              tags$li(
                "The ",
                tags$b("World Map"),
                "tab shows the closeness to frontier of a given indicator for all countries with available data."
              ),
              tags$li(
                "The ",
                tags$b("Time Trends"),
                "tab shows the evolution year by year of multiple indicators."
              ),
              tags$li(
                "The ",
                tags$b("Data"),
                "tab provides an interactive table containing the closeness to frontier data for all countries.
                It also allows users to download the data in different formats."
              ),
              tags$li(
                "The ",
                tags$b("Methodology & User Guide"),
                "tab includes metadata on the indicators, country groups and methods used in the analysis, and FAQs."
              ),
              tags$li(
                "The ",
                tags$b("Terms of Use and Disclaimers"),
                "tab provides more information about the terms of use and disclaimers, as well as citation information."
              ),
              tags$li(
                "The ",
                tags$b("FAQ"),
                "tab shows and answers the most frequently asked questions about CLIAR."
              ),
              tags$li(
                "The ",
                tags$b("Contact Us"),
                "tab allows users to directly contact us to CLIAR@worldbank.org"
              ),
              tags$li(
                "The ",
                tags$b("Source Code"),
                "tab takes users to our GitHub repository where they can access our source code."
              )
            ),
            p("Disclaimer :The findings, interpretations, and conclusions expressed in CLIAR are a product of staff of the World Bank, but do not necessarily reflect the views of the World Bank and its affiliated organizations, or those of the Executive Directors of the World Bank or the governments they represent. Moreover, country borders or names used and available in this dashboard do not necessarily reflect the World Bank Group's official position, and do not imply the expression of any opinion on the part of the World Bank, concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries. The term country, used interchangeably with economy, does not imply political independence but refers to any territory for which authorities report separate social or economic statistics.")
            
          )
        ),
        
        ## Country benchmark tab -------------------------------------------------------
        tabItem(
          tabName = "benchmark",
          useShinyjs(),
          
          fluidRow(
            column(
              width = 6,
              style = "padding-left: 24px",
              pickerInput(
                "country",
                label = helper(
                  shiny_tag = tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
                  type = "inline",
                  icon = "circle-question",
                  title = "Base country",
                  content = c(
                    "Choose the base country of interest. (For some analysis, you can select more than one.)"
                  ),
                  buttonLabel = "Close",
                  fade = T,
                  size = "s"
                ),
                choices = countries,
                choicesOpt = list(
                  content = flags_with_countries,
                  style = rep(length(flags_with_countries))
                ),
                selected = NULL,
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE 
                )
              )
            ),
            ## Guide action button
            column(
              width = 3,
              tags$span("Guided tour", style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
              fluidRow(
                shinyWidgets::actionBttn(
                  inputId = "start_guide_bench",
                  label = "Start", 
                  icon = shiny::icon("gear"),
                  style = "jelly",
                  color = "primary",
                  size = "sm"
                )
              )
            ),
            column(
              id = "input_buttons",
              width = 3,
              fluidRow(
                column(
                  width = 8,
                  helper(
                    shiny_tag = tags$span("Selection of Countries", style = "font-size: 1rem; color: #051f3f; font-weight:bold"),
                    type = "inline",
                    icon = "circle-question",
                    title = "Saving and loading Selection of Countries",
                    content = c(
                      "You can save your selected inputs to return to at a future time: click “Save Selection of Countries” button to download a .rds file to your computer with that information. When you return to the dashboard, you can click “Load Selection of Countries” button and then “Browse” to select this same .rds file. Loading this .rds file will re-populate all of the selections that you previously made."
                    ),
                    buttonLabel = "Close",
                    fade = T,
                    size = "s"
                  )
                ),
                column(4)
              ),
              fluidRow(
                ## Load inputs button
                buttons_func(
                  id = "load_inputs",
                  lab = "Load"
                ),
                ## Save inputs button
                shinyjs::disabled(
                  downloadButton(
                    "save_inputs", 
                    "Save"
                  )
                )
              )
            )
          ),
          
          fluidRow(style = "height: 5px;"),
            
            ### Comparison card 
            bs4Card(
              title = "Comparator countries",              
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              
              shiny::fluidRow(
                column(
                  width = 6,
                  pickerInput(
                    "groups",
                    label = helper(
                      shiny_tag = "Select comparison groups",
                      type = "inline",
                      icon = "circle-question",
                      title = "Pre-defined groups",
                      content = c(
                        "There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed."
                      ),
                      buttonLabel = "Close",
                      fade = T,
                      size = "s"
                    ),
                    choices = group_list,
                    selected = NULL,
                    multiple = TRUE,
                    options = list(
                      `actions-box` = TRUE,
                      `live-search` = TRUE
                    )
                  )
                ),
                column(
                  id = "show_countries_column",
                  width = 3,
                  style = "display: flex; align-items: center; justify-content: center;",
                  shinyWidgets::materialSwitch(
                    inputId = "show_countries",
                    label = helper(
                      shiny_tag = tags$b("Show list of countries"),
                      type = "inline",
                      icon = "circle-question",
                      title = "List of countries",
                      content = c(
                        "Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove."
                      ),
                      buttonLabel = "Close",
                      fade = T,
                      size = "s"
                    ),
                    value = FALSE,
                    status = "success"
                  )
                ),
                column(
                  id = "custom_grps_column",
                  width = 3,
                  style = "display: flex; align-items: center; justify-content: left;",
                  shinyWidgets::materialSwitch(
                    inputId = "create_custom_grps",
                    label = helper(
                      shiny_tag = tags$b("Create custom groups"),
                      type = "inline",
                      icon = "circle-question",
                      title = "Custom groups",
                      content = paste0(
                        "Alternative, you may create up to three custom groups of countries. This feature will additionally display in the Benchmarking graphs the median estimates of each custom group.",
                        "<br><br><b>Note:</b> Currently custom groups are not allowed when displaying ranks instead of values, when ranking from best to worst, or when doing the dynamic benchmark."
                      ),
                      buttonLabel = "Close",
                      fade = T,
                      size = "s"
                    ),
                    value = FALSE,
                    status = "success"
                  )
                )
              ),
              
              shiny::conditionalPanel(
                "input.create_custom_grps == true",
                
                fluidRow(
                  column(
                    width = 12,
                    shinyWidgets::materialSwitch(
                       inputId = "show_custom_grps",
                       label = tags$b("Show custom groups"),
                       status = "success",
                       value = TRUE
                     )
                  )
                ),
                #fluidRow(
                #  column(
                #    width = 12,
                #    tags$p(HTML("<strong>NOTE:</strong> Currently custom groups are not allowed when displaying ranks instead of values, when ranking from best to worst, or when doing the dynamic benchmark."))
                #  )
                #),
                fluidRow(
                  column(
                    width = 3,
                    numericInput(
                      inputId = "custom_grps_count",
                      label = "Number of groups",
                      value = 1,
                      min = 1,
                      max = 3,
                      step = 1
                    )
                  ),
                  column(
                    width = 3,
                    style = "display: flex; align-items: center; justify-content: center;",
                    shinyWidgets::actionBttn(
                      inputId = "save_custom_grps",
                      label = "Save custom groups", 
                      icon = shiny::icon("save"),
                      style = "jelly",
                      color = "primary",
                      size = "sm"
                    )
                  ),
                  column(
                    width = 12,
                    conditionalPanel(
                      "input.custom_grps_count >= 1",
                      uiOutput("custom_grps")
                    )
                  )
                )
              ),
              #### Countries list 
              shiny::conditionalPanel(
                "input.show_countries == true",
                
                fluidRow(style = "height: 15px;"),
                
                fluidRow(
                  column(
                    width = 12,
                    checkboxGroupButtons(
                      inputId = "countries",
                      individual = TRUE,
                      label = NULL,
                      choices = countries,
                      selected = NULL, 
                      checkIcon = list(
                        yes = icon(
                          "ok",
                          lib = "glyphicon"
                        )
                      )
                    )
                  )
                )
              )
            ),
          
            ### Bench card 
            bs4Card(
              title = "Benchmarking options",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              collapsed = FALSE,
              fluidRow(
                column(
                  width = 6,
                  pickerInput(
                    inputId = "threshold", 
                    label = helper(
                      shiny_tag = tags$b("Benchmarking Thresholds"),
                      type = "inline",
                      icon = "circle-question",
                      title = "Benchmarking Thresholds",
                      content = c(
                        "The default benchmarking thresholds for weak, emerging and strong institutions are 25th and 50th percentiles. You can also select the “Terciles” option, which uses 33rd and 66th percentiles as thresholds instead."
                      ),
                      buttonLabel = "Close",
                      fade = T,
                      size = "s"
                    ),
                    choices = c("Default","Terciles")
                  )
                ),
                column(width = 1),
                column(
                  width = 5,
                  div(
                    id = "benchmark_dots_div",
                    prettyCheckbox(
                      inputId = "benchmark_dots",
                      label = helper(
                        shiny_tag = tags$b("Show comparison countries"),
                        type = "inline",
                        icon = "circle-question",
                        title = "Show comparison countries",
                        content = c(
                          "Select this option to show the comparison countries as white circles on the plots. You may hover over each circle to see the country name. Note that individual countries are represented by circles in the first example below. This shows the distribution of values for the comparison group."
                        ),
                        buttonLabel = "Close",
                        fade = T,
                        size = "s"
                      ),
                      value = FALSE,
                      icon = icon("check"),
                      status = "success"
                    )
                  ),
                  div(
                    id = "rank_div",
                    prettyCheckbox(
                      inputId = "rank",
                      label = helper(
                        shiny_tag = tags$b("Show rank instead of value"),
                        type = "inline",
                        icon = "circle-question",
                        title = "Show rank instead of value",
                        content = c(
                          "Select this option to change the x-axis of the static benchmarking plot to display rankings instead of the CTF value."
                        ),
                        buttonLabel = "Close",
                        fade = T,
                        size = "s"
                      ),
                      value = FALSE,
                      icon = icon("check"),
                      status = "success"
                    )
                  ),
                  div(
                    id = "preset_order_div",
                    prettyCheckbox(
                      inputId = "preset_order",
                      label = helper(
                        shiny_tag = tags$b("Rank indicators from best to worst"),
                        type = "inline",
                        icon = "circle-question",
                        title = "Rank indicators from best to worst",
                        content = c(
                          "Select this option to change the ordering of the variables on the vertical axis of the figure. Ranking from best to worst will place the indicator for which the base country has the highest value first and the indicator with the lowest value last."
                        ),
                        buttonLabel = "Close",
                        fade = T,
                        size = "s"
                      ),
                      value = FALSE,
                      icon = icon("check"),
                      status = "success"
                    )
                  )
                )
              )
            ),
          
            ### Outputs card
            bs4Card(
              title = "Outputs",
              status = "success",
              solidHeader = TRUE,
              width = 12,
              collapsible = TRUE,
              fluidRow(
                column(
                  width = 6,
                  pickerInput(
                    "family",
                    label = helper(
                      shiny_tag = tags$b("Select institutional cluster"),
                      type = "inline",
                      icon = "circle-question",
                      title = "Institutional cluster",
                      content = c(
                        "Choose the institutional cluster you would like to display. The overview displays the aggregate results at the institutional-cluster level. When selecting a specific institutional-cluster, the individual indicators/components will be displayed."
                      ),
                      buttonLabel = "Close",
                      fade = T,
                      size = "s"
                    ),
                    choices = c("Overview", names(variable_list)),
                    selected = NULL
                  )
                ),
                # Please do not delete the below button, though not being displayed. It has downstream impact on other process.
                column(
                  width = 1,
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
                      "max-options" = 3
                    )
                  )
                ),
                column(
                  width = 4,
                  style = "display: flex; align-items: center; justify-content: center;",
                  fluidRow(
                    column(
                      width = 10,
                      uiOutput(
                        "select_button"
                      )
                    ),
                    column(
                      width = 1,
                      helper(
                        shiny_tag = NULL,
                        type = "inline",
                        icon = "circle-question",
                        title = "Apply",
                        content = c(
                          "Click on this box to (re-)run the analysis and (re-)load the resulting graphs. Note that this has to be done for every new selection or option, including a different institutional cluster. This option is enabled when the base country and at least 10 comparison countries are selected."
                        ),
                        buttonLabel = "Close",
                        fade = T,
                        size = "s"
                      )
                    ),
                    column(1)
                  )
                )
              ),
              fluidRow(
                column(
                  width = 2,
                  helper(
                    shiny_tag = tags$b("Downloads"),
                    type = "inline",
                    icon = "circle-question",
                    title = "Pre-populated reports and data",
                    content = c(
                      "Download pre-populated Word or Power Point documents with the results. Note that you may select the 
                      “Advanced Report” box to receive more detailed information - including all dynamic graphs. Select the help
                      button next to the checkbox to learn more.
                      
                      Click the download “Data” button to download a CSV file that contains the data needed to recreate the benchmarking graphs."
                    ),
                    buttonLabel = "Close",
                    fade = T,
                    size = "s"
                  ),
                )
              ),
              fluidRow(
                column(
                  width = 9,
                  fluidRow(
                    id = "download_reports",
                    column(
                      width = 3,
                      shinyjs::disabled(
                        downloadButton(
                          "report",
                          "Editable report",
                          style = "width:100%; background-color: #204d74; color: white"
                        )
                      ),
             
                    ),
                    column(
                      width = 3,
                      shinyjs::disabled(
                        downloadButton(
                          "advreport",
                          "Advanced Report",
                          style = "width:100%; background-color: #204d74; color: white"
                        )
                      )
                    ),
                    column(
                      width = 3,
                      shinyjs::disabled(
                        downloadButton(
                          "pptreport",
                          "PPT report",
                          style = "width:100%; background-color: #204d74; color: white"
                        )
                      )
                    ),
                    column(
                      id = "download_data_opt",
                      width = 3,
                      shinyjs::disabled(
                        downloadButton(
                          "download_data_1",
                          "Data",
                          style = "width:100%; background-color: #204d74; color: white"
                   
                        )
                      )
                    )
                  )
                ),
                
                column(
                  width = 3,
                  shinyjs::disabled(
                    downloadButton(
                      "download_Coverage",
                      "Coverage report",
                      style = "width:100%; background-color: #204d74; color: white"
                    )
                  )
                )
                #Commenting Out MISSINGNESS REPORT until it is fixed (Formatting is bad)
                # column(
                #   width = 4,
                #   shinyjs::disabled(
                #     downloadButton(
                #       "download_missing",
                #       "Missingness report",
                #       style = "width:100%; background-color: #204d74; color: white"
                #     )
                #   )
                # )
              )
              # shiny::column(3,
              #               shinyWidgets::materialSwitch(
              #                 inputId = "show_dynamic_plot",
              #                 label = "Show dynamic benchmark plot",
              #                 status = "success",
              #                 value = FALSE
              #               )
              # )
          ),
          
          ### Static Benchmarks ----
          bs4Card(
            title = "Static Benchmarks",
            collapsible = TRUE,
            width = 12,
            
            conditionalPanel(
              "input.select !== 0",
              fluidRow(
                
                column(
                  width = 12,
                  plotlyOutput(
                    "plot",
                    height = paste0(plot_height * 3, "px")
                  ) %>% shinycssloaders::withSpinner(color = "#051f3f", type = 8)
                )
              ),
              fluidRow(
                shinyWidgets:: materialSwitch(
                  inputId = "show_plot_notes",
                  label = "Show notes",
                  status = "success",
                  value = FALSE
                )

              ),
              
              conditionalPanel(
                "input.show_plot_notes !== false",
                
                fluidRow(
                  
                  column(
                    width = 12,
                    htmlOutput(
                      "plot_notes"
                    )
                  )
                  
                )
              )
            )
          ),
          
          ### Dynamic benchmark tab  -------------------------------------------------------
          bs4Card(
            width = 12,
            solidHeader = FALSE,
            gradientColor = "primary",
            title = "Dynamic Benchmarks",
            collapsible = TRUE,
            tags$style("
                #dynamic_benchmark_plot {
                  height: 100%;
                  overflow-y: scroll;
                }
              "),

            conditionalPanel(
              "input.select !== 0 && output.plot!=null",
              fluidRow(
                
                column(
                  width = 12,
                  plotlyOutput(
                    "dynamic_benchmark_plot",
                    height =  paste0(plot_height * 4, "px"),
                    
                  ) %>% shinycssloaders::withSpinner(color = "#051f3f", type = 8)
                )
                
              )
              
            )
            
          ),
          
          bs4Card(
            title = "Indicator definitions",
            collapsible = TRUE,
            collapsed = TRUE,
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
            width = 12,
            
            fluidRow(
        
              column(
                width = 3,
                pickerInput(
                  "country_bar",
                  label = "Select a base country",
                  choices = c("", countries),
                  selected = NULL,
                  multiple = FALSE
                )
              ),
              
              column(
                width = 3,
                pickerInput(
                  inputId = "groups_bar",
                  label = "Select comparison groups",
                  choices = group_list,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    # size = 21,
                    `live-search` = TRUE,
                    `actions-box` = TRUE
                  )
                )
              ),   
              column(
                width = 3,
                pickerInput(
                  "vars_bar",
                  label = "Select indicator",
                  choices = variable_list_benchmarked,
                  selected = NULL,
                  options = list(
                    # size = 20,
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    "max-options" = 3
                  ),
                  width = "100%"
                )
              ),
              column(
                width = 3,
                radioGroupButtons(
                  "value_bar",
                  label = "Select data source",
                  choices = c(
                    "Closeness to frontier" = "ctf",
                    "Original indicator" = "raw"
                  ),
                  justified = TRUE,
                  selected = "ctf",
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"))
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
              inputId = "countries_bar",
              individual = TRUE,
              label = NULL,
              choices = countries,
              checkIcon = list(
                yes = icon(
                  "ok",
                  lib = "glyphicon"
                )
              )
            )
          ),
          # Color Select BS4 Card
          bs4Card(
            title = "Select Bar Graph Colors",
            status = "success",
            collapsed=TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                colourInput(
                  "color_base_bar",
                  "Choose a base country color:",
                  value = "#f29411"  
                )
              ),
              column(
                width = 4,
                colourInput(
                  "color_comp_bar",
                  "Choose a comparison country color:",
                  value = "#080770")
            ),
              column(
                width = 4,
                colourInput(
                  "color_groups_bar",
                  "Choose a comparison group color:",
                  value = "#808080")
              )
          )),
  

         #======================================  
          conditionalPanel(
            'input.country_bar !== "" && input.vars_bar != null',
            
            bs4Card(
              width = 12,
              solidHeader = FALSE,
              gradientColor = "primary",
              collapsible = FALSE,
              
              plotlyOutput(
                "bar_plot",
                height = paste0(plot_height * 1.6, "px")
              )
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
            width = 12,
            
            fluidRow(
              
              column(
                width = 3,
                pickerInput(
                  "country_scatter",
                  label = "Select a base country",
                  choices = c("", countries),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    # size = 20,
                    `actions-box` = TRUE,
                    `live-search` = TRUE
                  )
                )
              ),
              
              column(
                width = 3,
                pickerInput(
                  "y_scatter",
                  label = "Select indicator for Y axis",
                  choices = y_scatter_choices,
                  selected = NULL,
                  options = list(
                    `live-search` = TRUE,
                    # size = 20,
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
                  choices = NULL,
                  selected = NULL,
                  options = list(
                    `live-search` = TRUE,
                    # size = 20,
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
                    `actions-box` = TRUE#,
                    # size = 18
                  ),
                )
              )
              
              
            ),
            fluidRow(
              column(
                width = 3,
                shinyWidgets::materialSwitch(
                  inputId = "linear_fit",
                  label = "Show linear fit line", 
                  value = FALSE,
                  status = "success"
                )
              ),
              column(
                width = 6),
              column(
                width = 2.4,
                shinyjs::hidden(downloadButton(
                  "download_bivariate_data",
                  "Download Chart Data",
                  style = "width:100%; background-color: #204d74; color: white"
                ))
              ),
            )
          ),
          
          bs4Card(
            title = "Select individual comparison countries",
            width = 12,
            status = "success",
            collapsed = TRUE,
            
            checkboxGroupButtons(
              inputId = "countries_scatter",
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
            title = "Select Bar Graph Colors",
            status = "success",
            collapsed=TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                colourInput(
                  "color_base_scatter",
                  "Choose a base country color:",
                  value = "#f29411"  
                )
              ),
              column(
                width = 4,
                colourInput(
                  "color_comp_scatter",
                  "Choose a comparison country color:",
                  value = "#080770")
              )
            )),
          
          conditionalPanel(
            'input.y_scatter !== ""',
            
            bs4Card(
              width = 12,
              solidHeader = FALSE,
              gradientColor = "primary",
              collapsible = FALSE,
              
              plotlyOutput(
                "scatter_plot",
                height = paste0(plot_height * 1.6, "px")
              )
            )
          )
        ),
        
        
        ## Trends  tab ------------------------------------------------------------
        
        
        tabItem(
          tabName = "trends",
          
          box(
            width = 12,
            solidHeader = TRUE,
            title = "Select indicator to visualize",
            status = "success",
            collapsible = TRUE,
            
            fluidRow(
              
              column(
                width = 3,
                pickerInput(
                  "country_trends",
                  label = "Select a base country",
                  choices = c("", countries),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(
                    # size = 23
                    `live-search` = TRUE
                  )
                )
              ),
              
              column(
                width = 4,
                pickerInput(
                  "group_trends",
                  label = "Select comparison groups",
                  choices = group_list,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    "max-options" = 5,
                    `live-search` = TRUE#,
                    # size = 21
                  )
                )
              ),
              column(
                width = 5,
                pickerInput(
                  "vars_trends",
                  label = "Select indicator to visualize",
                  choices = filtered_variable_list,
                  selected = NULL,
                  options = list(
                    `live-search` = TRUE,
                    # size = 21,
                    title = "Click to select family or indicator"
                  ),
                  width = "100%"
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
              inputId = "countries_trends",
              individual = TRUE,
              label = NULL,
              choices = countries,
              checkIcon = list(
                yes = icon("ok",
                           lib = "glyphicon")
              )
            )
          ),
          # Color Select BS4 Card
          bs4Card(
            title = "Select Time Trend Colors",
            status = "success",
            collapsed=TRUE,
            width = 12,
            
            fluidRow(
              column(
                width = 4,
                colourInput(
                  "color_base_trends",
                  "Choose a base country color:",
                  value = "#f29411"  
                )
              ),
              column(
                width = 4,
                colourInput(
                  "color_comp_trends",
                  "Choose a comparison country color:",
                  value = "#080770"  
                )
              ),
              column(
                width = 4,
                colourInput(
                  "color_groups_trends",
                  "Choose a comparison group color:",
                  value = "#808080"))
            )
          ),
          
          #==============================
          
          conditionalPanel(
            'input.vars_trends !== null && input.country_trends != ""',
            
            bs4Card(
              width = 12,
              solidHeader = FALSE,
              gradientColor = "primary",
              collapsible = FALSE,
              
              plotlyOutput(
                "time_series",
                height = paste0(plot_height * 1.6, "px")
              )
            )
          )
        ),
        
        ##  World Map  tab ------------------------------------------------------------
        
        
        tabItem(
          tabName = "world_map",
          
          box(
            width = 12,
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
                  selected = NULL,
                  options = list(
                    `live-search` = TRUE,
                    # size = 20,
                    title = "Click to select family or indicator"
                  ),
                  width = "100%"
                )
              ),
              
              column(
                width = 3,
                radioGroupButtons(
                  "countries_map",
                  label = "Select countries to display",
                  choices = c(
                    "All" = FALSE,
                    "Base + comparison countries" = TRUE
                  ),
                  justified = TRUE,
                  selected = FALSE,
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"))
                )
              ),
              
              column(
                width = 4,
                radioGroupButtons(
                  "value_map",
                  label = "Select data source",
                  choices = c(
                    "Closeness to frontier" = "ctf",
                    "Original indicator" = "raw"
                  ),
                  justified = TRUE,
                  selected = "ctf",
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"))
                )
              )
            )
          ),
          
          conditionalPanel(
            "input.vars_map !== ''",
            
            bs4Card(
              width = 12,
              solidHeader = FALSE,
              gradientColor = "primary",
              collapsible = FALSE,
              
              plotlyOutput(
                "map",
                height = paste0(plot_height, "px")
              ) %>% shinycssloaders::withSpinner(color = "#051f3f", type = 8)
            )
          )
        ),
        
        ## Data tab --------------------------------------------------------------------
        
        tabItem(
          tabName = "data",
          
          bs4Card(
            title = "Data download",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = F,
            
            fluidRow(
              column(
                width = 2.4,
                  downloadButton(
                    "down_clust_ctf_stat",
                    "CTF Static (Cluster-level aggregates only)",
                    style = "width:100%; background-color: #204d74; color: white"
                  )
                
              ),
              column(
                width = 2.4,
                  downloadButton(
                    "down_all_ctf_stat",
                    "CTF Static (All indicators)",
                    style = "width:100%; background-color: #204d74; color: white"
                  )
              ),
              column(
                width = 2.4,
                  downloadButton(
                    "down_clust_ctf_dyn",
                    "CTF Dynamic (Cluster-level aggregates only)",
                    style = "width:100%; background-color: #204d74; color: white"
                  )
              ),
              column(
                width = 2.4,
                  downloadButton(
                    "down_all_ctf_dyn",
                    "CTF Dynamic (All indicators)",
                    style = "width:100%; background-color: #204d74; color: white"
                  )
              ),
              column(
                width = 2.4,
                  downloadButton(
                    "down_original",
                    "Original indicators",
                    style = "width:100%; background-color: #204d74; color: white"
                  )
              ),
              column(
                width = 2.4,
                downloadButton(
                  "down_db_var",
                  "Data Dictionary",
                  style = "width:100%; background-color: #204d74; color: white"
                )
              )
            )
          ),
      
          bs4Card(
            title = "Pre-Download Base & Comparison Country Selection",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsed = TRUE,
            fluidRow(
              
              column(
                width = 6,
                style = "padding-left: 24px",
                pickerInput(
                  "country_dwnld",
                  label = helper(
                    shiny_tag = tags$span("Base country:", style = "font-size: 28px; color: #051f3f;"),
                    type = "inline",
                    icon = "circle-question",
                    title = "Base country",
                    content = c(
                      "Choose the base country of interest. (For some analysis, you can select more than one.) This menu can also be accessed in the Country Benchmarking tab"
                    ),
                    buttonLabel = "Close",
                    fade = T,
                    size = "s"
                  ),
                  choices = countries,
                  choicesOpt = list(
                    content = flags_with_countries,
                    style = rep(length(flags_with_countries))
                  ),
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE 
                  )
                )
              )),
          fluidRow(style = "height: 5px;"),
          
          ### Comparison card 
            
            shiny::fluidRow(
              column(
                width = 6,
                pickerInput(
                  "groups_dwnld",
                  label = helper(
                    shiny_tag = "Select comparison groups",
                    type = "inline",
                    icon = "circle-question",
                    title = "Pre-defined groups",
                    content = c(
                      "There are multiple ways to select the comparator countries. Here you can select one (or more) pre-defined group(s) (either as a comparator group itself or as a shortcut for selecting individual countries). When selecting more than one, it is the union (i.e., sum) of the groups that will be analyzed.This menu can also be accessed in the Country Benchmarking tab"
                    ),
                    buttonLabel = "Close",
                    fade = T,
                    size = "s"
                  ),
                  choices = group_list,
                  selected = NULL,
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE
                  )
                )
              ),
              column(
                id = "show_countries_column_dwnld",
                width = 3,
                style = "display: flex; align-items: center; justify-content: center;",
                shinyWidgets::materialSwitch(
                  inputId = "show_countries_dwnld",
                  label = helper(
                    shiny_tag = tags$b("Show list of countries"),
                    type = "inline",
                    icon = "circle-question",
                    title = "List of countries",
                    content = c(
                      "Here you can add and remove individual comparator countries. If you have already selected one or more the pre-defined groups, those countries will appear as selected, and you can manually add or remove."
                    ),
                    buttonLabel = "Close",
                    fade = T,
                    size = "s"
                  ),
                  value = FALSE,
                  status = "success"
                )
              ),
              
              shiny::conditionalPanel(
                "input.show_countries_dwnld == true",
                
                fluidRow(style = "height: 15px;"),
                
                fluidRow(
                  column(
                    width = 12,
                    checkboxGroupButtons(
                      inputId = "countries_dwnld",
                      individual = TRUE,
                      label = NULL,
                      choices = countries,
                      selected = "countries", 
                      checkIcon = list(
                        yes = icon(
                          "ok",
                          lib = "glyphicon"
                        )
                      )
                    )
                  )
                )
              )
            ) #fluid row
          )#bs4
          ,
          
  
          bs4Card(
            title = "Interactive Data Access & Custom Download",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            collapsible = F,
            fluidRow(
              column(
                width = 8,
                dataTableOutput("benchmark_datatable") |>
                  shinycssloaders::withSpinner(color = "#051f3f", type = 8)
              ),
              column(
                width = 4,
                bs4Card(
                  
                  title = "Select information to display",
                  status = "success",
                  width = 12,
                  collapsible = FALSE,
                  
                  pickerInput(
                    "countries_data",
                    label = "Select countries to include",
                    choices = c("All", "Base country only", "Base + comparison countries"),
                    selected = "All",
                    options = list(
                      "All" = list(disabled = FALSE),
                      "Base country only" = list(disabled = TRUE),
                      "Base + comparison countries" = list(disabled = TRUE)
                    )
                  ),
                  
                  # pickerInput(
                  #   "groups_data",
                  #   label = "Select groups to include",
                  #   choices = c("All", "None"),
                  #   selected = "All"
                  # ),
                  
                  pickerInput(
                    "vars",
                    label = "Select institutional families to include",
                    choices = names(definitions),
                    selected = names(definitions),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  
                  radioGroupButtons(
                    "data_source",
                    label = "Select a data source",
                    choices = c("Closeness to frontier (Static)",
                                "Closeness to frontier (Dynamic)",
                                "Original indicators"),
                    selected = "Closeness to frontier (Static)",
                    direction = "vertical",
                    justified = TRUE,
                    checkIcon = list(
                      yes = icon("ok",
                                 lib = "glyphicon")
                    )
                  ),
                  #Input selector for download column names
                  (
                  shinyWidgets::materialSwitch(
                    inputId = "descriptions_dwnld",
                    label = helper(
                      shiny_tag = tags$b("Descriptive Columns"),
                      type = "inline",
                      icon = "circle-question",
                      title = "Descriptive Names",
                      content = c(
                    "Here you can select whether you want abrreviated or full names for each of the columns in the downloaded data."
                      ),
                      buttonLabel = "Close",
                      fade = T,
                      size = "s"
                    ),
                    value = FALSE,
                    status = "success"
                  )),
                  
                  shinyjs::hidden(
                    radioGroupButtons(
                      "data_value",
                      label = "Select information to show",
                      choices = c("Value"),
                      selected = "Value",
                      direction = "vertical",
                      justified = TRUE,
                      checkIcon = list(
                        yes = icon("ok",
                                   lib = "glyphicon")
                      )
                    )
                  ),
                  
                  p(tags$b("Download data")),
                  
                  downloadButton(
                    "download_global_rds",
                    ".rds",
                    style = "width:100%; background-color: #204d74; color: white"
                  ),
                  
                  downloadButton(
                    "download_global_csv",
                    ".csv",
                    style = "width:100%; background-color: #204d74; color: white"
                  ),
                  
                  downloadButton(
                    "download_global_dta",
                    ".dta",
                    style = "width:100%; background-color: #204d74; color: white"
                  )
                )
              )
            )
          )

        ),
        
        ## Methodology tab -------------------------------------------------------------
        
        tabItem(
          tabName = "methodology_ug",
          
          box(
            width = 12,
            status = "navy",
            title = "User Guide",
            
            p('Here is a Downloadable User Guide Meant to Demonstrate the Capabilities of the CLIAR Dashboard'),
            downloadButton("download_user_guide",
                           "Download CLIAR User Guide",
                           style = "background-color: #204d74; color: white")
          
            
          ),
          box(
            width = 12,
            status = "navy",
            collapsed=TRUE,
            title = "Institutional families",
            
            p("The CLIAR Benchmarking uses a set of curated and validated institutional indicators, clustered into 13 institutional clusters:", 
              
              tags$ul( 
                
                tags$li("Political institutions"), 
                
                tags$li("Social institutions"), 
                
                tags$li("Absence of Corruption"), 
                
                tags$li("Transparency and Accountability institutions"), 
                
                tags$li("Justice institutions"), 
                
                tags$li("Public Finance Institutions"), 
                
                tags$li("Public Human Resource Management institutions"), 
                
                tags$li("Digital and Data institutions"), 
                
                tags$li("Business environment institutions"), 
                
                tags$li("SOE Corporate Governance"), 
                
                tags$li("Labor and Social Protection institutions"), 
                
                tags$li("Service Delivery institutions"), 
                
                tags$li("Climate Change and Environment institutions") 
                
              ) 
              
            ), 
            p("The proposed clusters are based on an effort to capture key functions that different institutions perform. In so doing, the categorization process faces a trade-off between aggregation and narrowness, where the categories ought to be broad enough to capture enough indicators and policy spaces, but narrow enough to guide a deep qualitative analysis as well as a fruitful and engaged conversation with the country. In addition, the categorization also faces the limitations of data availability."),
            p('All country-level indicators can be downloaded in the “Data” tab.')
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Closeness to frontier",
            
            p('The dashboard uses a “Closeness to Frontier"(CTF) methodology. The CTF methodology allows to assess country’s performance across institutional indicators by comparing it with the “global frontier”, where the global frontier is the world’s best performer. For each indicator, a country’s performance is rescaled on a 0-1 scale using the linear transformation (worst–y)/(worst–frontier), where 1 represents the best performer and 0 the worst performer. The higher the score, the closer a country is to the best performer and the lower the score, the closer a country is to the worst performer, and more distant to the frontier. The best and worst performers are identified using available data from the global sample (i.e., considering all countries for which data is available), and using the relevant time period according to the benchmarking approach –i.e., whether it estimates the static (default) CTF benchmarking scores or dynamic CTF scores. In the static case, the average of the 2019-2023 period is used.'),
            p('For each institutional family, the CTF scores obtained for each indicator are aggregated through simple averaging into one CTF score at family level. This captures the overall performance for an institutional family relatively to the “global frontier”, while the performance across the indicators will help identify the most challenging areas for institutional strengthening.')
            
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Percentile analysis and comparator countries",
            
            p('The CTF scores compare the country’s performance with the best and worst performers at global level. However, how does the country compare relatively to a set of chosen comparators?'),
            p('The dashboard uses percentile distribution and traffic light coloring to capture the areas where the largest institutional gaps exist, ',
              HTML('<b>relative to the set of country comparators</b>'),
              '. Relative institutional weaknesses and strengths are defined based on the percentile in which each country indicator belongs. This methodology requires teams to make an informed decision on the set of comparator countries used for the benchmarking, since institutional weaknesses and strengths are identified relatively to those comparator countries.'),
            p('The “Closeness to Frontier” (length of the bar) and the percentile analysis (color of the bar) capture two related but different performance dimensions. The CTF compares the country’s performance with the best and worst performers. The percentile analysis benchmarks the country’s performance with all the set of other comparator countries. For example, it could be that for one indicator or institutional cluster the CTF score is relatively high and close to 1 (indicating in fact ‘closeness to the frontier’) but, at the same time, this dimension is marked as an institutional weakness (red coloring) because the country’s performance is still worse than the majority of comparator countries.'),
            p('The percentile analysis requires indicators to be available for the base country, while it also effectively drops those indicators whose distribution precludes this percentile classification (i.e., low variance).')
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title ="Country group definitions",
            
            p(
              "Country group definitions are extracted from the",
              a(
                "World Bank Country and Lending Groups.",
                href = "https://datahelpdesk.worldbank.org/knowledgebase/articles/906519-world-bank-country-and-lending-groups"
              ),
              "which classifies all 218 World Bank member countries and economies.", 
              "Income classifications for FY24 is based on 2022 gross national income (GNI) per capita,", 
              "calculated using the World Bank Atlas method."
            ),
            p(
              "The groups are:",
              tags$ul(
                tags$li(HTML("<b>Low income:</b> $1,135 or less")), 
                
                tags$li(HTML("<b>Lower middle income:</b> $1,136 - 4,465")), 
                
                tags$li(HTML("<b>Upper middle income:</b> $4,466 - 13,845")), 
                
                tags$li(HTML("<b>High income:</b> more than $13,845")) 
              )
            ),
            p(
              HTML("The term <i>country</i>, used interchangeably with <i>economy,</i>"),
              "does not imply political independence but refers to any territory for which authorities report separate social or economic statistics.",
              "Income classifications set on 1 July 2023 remain in effect until 1 July 2024.",
            ),
            p(
              "OECD members are: ", paste0(paste(country_list %>% filter(group_code=="OED") %>% .$country_name, collapse = ", "),".")
            )
          ),
          
          
          box(
            width = 12,
            status = "navy",
            title = "List of indicators",
            
            p("The indicators used to benchmark the institutional families are extracted from multiple public data sources.
                            For a full list of the indicators used, their sources, and their definitions, download the metadata below."),
            downloadButton("download_indicators",
                           "Download indicator definitions",
                           style = "background-color: #204d74; color: white")
          ),
          
          box(
            width = 12,
            status = "navy",
            title = "Where can I find additional information on the methodology?",
            downloadButton("download_metho",
                           "Download complete methodology",
                           style = "background-color: #204d74; color: white")
          )
          
        ),
        
        ## Publications  ================================================================
        tabItem(
          tabName = "pubs",
          publicationsUI("publications")
        ),
        
        ## Terms of use ================================================================
        
        tabItem(
          tabName = "terms",
          
          box(
            width = 12,
            status = "navy",
            collapsible = FALSE,
            title = "Terms of use and Disclaimer",
            solidHeader = TRUE,
            tags$ul(
              tags$li(
                'We ask that all users of the data to cite the data as follows:',
                HTML('"<em>Source: World Bank CLIAR Dashboard.</em>"')
              ),
              tags$li(
                "We kindly request that if users modify the methodology and the source code for their reports and analyses clearly state so and highlight the relevant departures from the CLIAR Benchmarking methodology."
              ),
              tags$li(
                "Disclaimer: The findings, interpretations, and conclusions expressed in CLIAR are a product of staff of the World Bank, but do not necessarily reflect the views of the World Bank and its affiliated organizations, or those of the Executive Directors of the World Bank or the governments they represent. Moreover, country borders or names used and available in this dashboard do not necessarily reflect the World Bank Group's official position, and do not imply the expression of any opinion on the part of the World Bank, concerning the legal status of any country or territory or concerning the delimitation of frontiers or boundaries. The term country, used interchangeably with economy, does not imply political independence but refers to any territory for which authorities report separate social or economic statistics."
              )
            )
          )
        ),
        
        ## FAQ tab =====================================================================
        
        tabItem(
          tabName = "faq",
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Does the CLIAR Benchmarking collect new data on governance and institutions?",
            p(
              "No.
              The CLIAR Benchmarking collects indicators that are publicly available and have been validated by our internal review process as proxies to measure country-level governance and institutions, with their corresponding caveats and limitations.
              In some exceptional cases, CLIAR does combine existing indicators to create new ones (e.g., aggregation of binary indicators); these are detailed in the CLIAR Methodological Note."
            )
          ),
          
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Can I add my own indicators to the dashboard and run the analysis including these indicators? ",
            p(
              "You cannot add indicators to the dashboard.
              However, you can download the full database and augment it with additional indicators to customize your analysis.
              You can also get in touch with the CLIAR team (CLIAR@worldbank.org) indicating which data you would like to be added in the database, and for which cluster.
              Each request will be reviewed by a team of technical experts and if the indicator meets the selection criteria indicated in the methodological note (quality and coverage) it will be added during the next update round."
            )
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "What does the traffic coloring mean? Is there a methodological foundation?",
            p(
              "The results from the institutional benchmarking are relative for
              a given country of interest vis a vis a chosen set of comparator countries.
              Using the distribution of the CTF scores in the set of comparator countries,
              we identify the score range for the bottom 25% of comparators,
              the score range for the 25%-50% group and the score range for the top 50% of comparators (or alternatively, using 33% and 66% as thresholds).
              Given the CTF score of the country of interest,
              we identify whether the country of interest for the analysis belong to the bottom,
              middle or top group. These percentile groups are used because they are simple and intuitive."
            )
          ),
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Why the length of the bar is different? Why a red bar is longer than another red bar, if they are both red?",
            p(
              "Using the distribution of the CTF scores in the set of comparator countries,
              we identify the score range for the bottom 25% of comparators,
              the score range for the 25%-50% group and the score range for the top 50% of comparators.
              The red bar represents the score range for the bottom 25% of comparators. (The same explanation applies if 33% and 66% thresholds are used.)
              While the CTF scores always range between 0 and 1,
              the length of the red bar varies across indicators depending on the distribution of the CTF scores in the comparator group.
              As an illustration, for a given set of comparator countries,
              for a given indicator the CTF scores in the bottom 25% of comparators may range between 0 and 0.2,
              while for another indicator it may range between 0 and 0.5."
            )
          ),
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "What is the difference between the static and dynamic benchmarking?",
            p(
              "The static benchmarking presents a cross-country snapshot based on averaging available indicators over the period 2018-2022; CTF calculations and distributional analysis are implemented over that cross-section. The dynamic benchmarking
              , computes CTF scores at the individual level on an annual basis, from 2013 to 2022, and distributional analysis is implemented on an annual basis, when data is available. Given data limitations, the dynamic benchmarking is more limited in the number of indicators and families analyzed --and some families are not included precisely because they do not offer data that could be aggregated and compared over time."
            )
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Why are certain indicators or institutional families not appearing in my benchmarking results?",
            p(

              "Indicators that are missing for the base country or exhibit low variance are dropped from the analysis. In some cases, such as for the SOE Governance family, this can result in dropping an entire institutional cluster."
          )),

          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Can I change in the dashboard the time period over which the benchmarking is applied?",
            p(
              "The Dashboard does not offer that functionality, but such customized analyzed could be performed by downloading the data from the dashboard."
            )
          ), box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Why are certain indicators and clusters not included in the dynamic benchmarking?",
            p(
              "Compared to static benchmarking, dynamic benchmarking is more selective (or “demanding”) with respect to indicators, considering their panel characteristics. Hence, indicators that do not offer multiple measurements for the same country are excluded from the analysis – e.g., OECD PMR and PEFA, which consequently excludes the SOE Governance Institutions and Public Finance Institutions indicator clusters from dynamic benchmarking"
            )
          ),
          
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "How do you deal with missing data for certain indicators and for certain countries?",
            p(
              "We deal with missing data in various ways.
              First, the (static) benchmarking analysis uses the average of indicators over recent years.
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
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Why do I have to choose at least 10 comparator countries for the benchmarking analysis?",
            p(
              "The percentile analysis identifies whether the performance of the
              country of interest in a given indicator or institutional cluster
              belongs to the bottom 25%, the 25%-50% group or the top 50% of
              the comparator countries (or, alternatively, the groups based on 33% and 66% thresholds).
              This percentile analysis can be meaningfully performed only if
              there is sufficient number of comparator countries."
            )
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "How do you choose the comparator countries/groups?",
            p(
              "It depends on the purpose of the analysis and the country context.
              For example, many reports have used regional,
              aspirational, and structural peers as identified by World Bank Country Teams."
            )
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Can I download the raw data for my own research/analytical purposes?",
            p(
              'Yes, the full compiled database is available in the "Data" tab for download.
              Both the "Closeness to Frontier" scores and the full database with yearly indicators are available for download.'
            )
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "Why are certain cluster averages missing when I download the data even if there is non-missing data on the indicators of that cluster?",
            p(
              "A balanced sample of individual CTF scores is aggregated by cluster to create cluster-level CTF scores. For each institutional cluster, a “balanced” subset of countries with full coverage (i.e., non-missing data) across all indicators within each cluster is created. This ensures that each cluster-level aggregate score relies on the same set of indicators for every country, allowing for robust and methodically sound inferences. The CTF cluster-level score is computed via simple averaging of the indicators within each cluster. This cluster-level score captures the overall performance for a given institutional category relative to the “global frontier.” The drawback of this robust methodological aggregation decision is that the data requirement is higher. Several families in both the static and dynamic versions do not meet the data requirements for meaningful aggregation (i.e., the balanced sample is too small or empty), and thus CTF cluster scores are not computed."
            )
          ),
          
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "How often is the CLIAR data updated? How do I know that the CLIAR data uses the latest available data?",
            p(
              'It is currently planned that the CLIAR Database will be updated once per year.
              The CLIAR Benchmarking Dashboard is programmed so that the data extraction from the data sources (primarily EFI360)
              is automated through APIs,
              therefore with minimal maintenance costs for the indicators already
              included in the dashboard and with stable APIs. The full compiled database,
              once updated, is available in the "Data" tab for download.
              Both the CLIAR Benchmarking "Closeness to Frontier" scores and the full CLIAR master database with
              yearly indicators are available for download and therefore users can easily verify the latest year available for each indicator.'
            )
          ),
          
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "How were the indicators included in the CLIAR Benchmarking selected?",
            p(
              'The indicators included in the CLIAR Benchmarking were selected following a criteria of 
              (geographical and time) coverage and quality. 
              This list was defined based on initial internal reviews, 
              and will be further refined based on inputs recently received by 
              sector experts and from the experiences of country teams in applying this tool. 
              The list of indicators used will be periodically reviewed in order to include new indicators that may be become available in the future. 
              As such, the CLIAR database is a "live tool".' 
            )
          ),
          box(
            width = 12,
            status = "navy",
            collapsed = TRUE,
            title = "How does CLIAR manage changes in the methodology of the construction of individual indicators used in the CLIAR database?",
            p(
              'CLIAR aims to keep consistent indicators. Hence, if specific indicators go through changes in their methodology, CLIAR will keep only those that are consistent, prioritizing the most recent ones. Some examples include PEFA and PMR indicators. If such change means a given indicator no longer meets the benchmarking criteria, then it is dropped from the benchmarking analysis.' 
            )
          )
        ) # Close FAQ tab
      )
    )
  )
