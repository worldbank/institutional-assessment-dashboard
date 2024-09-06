#' publications module
#'
#' Contains code used to display content in the publications tab
#'
#' @param id a unique identifier for this module. To be used in the ui and server files
#'
#' @return
#' @export
#'
#' @examples
publicationsUI <- function(id) {
  
  ## Note: When using modules, all inputs are wrapped in ns()
  ## e.g 1: shiny::textInput(inputId = ns("name"), label = "Please insert your name)
  ## e.g 2: shiny::numericInput(inputId = ns("obs"), label = "Observations:", value = 10, min = 1, max = 100)

  ns <- NS(id)

  tagList(
    # ## ensures that we can use the Bs4Dash functions in our module, but also suppresses
    #Warnings from bs4dash
    tags$script(suppressWarnings(shinyWidgets::useBs4Dash())),
    # shinyWidgets::useBs4Dash(),

    ## styling implemented on the divs
    tags$style(
      "
      .pubs { 
          height: 100%;
          /* grid-column-gap: 1.5625vw; */
          grid-row-gap: 1.5625vw;
          grid-template-rows: auto;
          grid-template-columns: 25% 75%;
          grid-auto-columns: 1fr;
          display: grid;
          border: 4px solid #051f3f;
          /*width: 352px; */
         }
    
      .pubs:hover {
            -webkit-transform: scale(1.05,1.05);
            -webkit-transition-timing-function: ease-out;
            -webkit-transition-duration: 250ms;
            -moz-transform: scale(1.05,1.07);
            -moz-transition-timing-function: ease-out;
            -moz-transition-duration: 250ms;
            position: relative;
            z-index: 99;
      }
           
           
    .pubs_image {
             background-color: #d8dcdb;
             justify-content: center;
             align-items: center;
             display: flex;
            }
   
   .pubs_content {
            display: flex;
            /*height: 250px;*/
            flex-direction: column;
            justify-content: space-evenly;
            align-items: left;
            background-color: white;
            /*border-radius: 0px 0px 10px 10px;*/
            border: 0 solid rgba(0,0,0,0.125);
            background: white;
            /*margin: 30px 0 0 0;*/
            padding: 5px 15px;
            text-align: left;
            color: #051f3f;'
           }  
      "
    ),

    ## create space at the top
    shiny::fluidRow(style = "height:15px"),

    ## a user can select a country whose report they want to view. By default, all reports
    ## are displayed. The `countries` object has been defined in the global.R file

    column(
      width = 3,
      pickerInput(
        ns("country"),
        label = "Select a country",
        choices = c("All", countries),
        selected = "All",
        multiple = FALSE,
        options = list(
          # size = 20,
          `live-search` = TRUE,
          `actions-box` = TRUE
        )
      )
    ),
    shiny::fluidRow(style = "height:15px"),

    ## publications (see note in the server)
    uiOutput(ns("publications"))
  )
}

publicationsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## reactive object that holds the country selected
      sel_country <- reactive({
        input$country
      })

      ## Note: The publications will be grouped by title, each in it's own separate card. We wont know how many
      ## titles or publications per title we'll have eventually. We are going to create this dynamically
      ## based on the info in publicationsList.xlsx

      output$publications <- renderUI({
        
        ## the number of titles in our file determines the number of cards that we will have
        n_titles <- length(unique(pubList$title))

        ## this object will hold output for each card
        ui_titles <- c()

        ## for each card (title) ...

        lapply(1:n_titles, function(i) {
          ## these two objects will hold shiny::fluidRow() and shiny::column() content for each card
          ui_parts_rows <- c()
          ui_parts_cols <- c()

          ## this checks how many publications we have per title ...
          ui_tot_pubs_per_title <- length(pubList$title[pubList$title == unique(pubList$title)[i]])

          ## which determines the columns we'll have in each card. Each publication occupies one column
          ui_cols <- ui_tot_pubs_per_title

          ## we'll have one row of publications per card (title) for now. As the number of publications
          ## grows, we'll re-consider
          ui_rows <- 1

          ## this generates a dataset of the publications per title
          pubList_per_title <- pubList %>% dplyr::filter(title %in% unique(pubList$title)[i])

          ## generate the card. Remember i here represents number of titles. Each will be placed in its own card
          ui_titles[[i]] <-
            
            ## card that will hold the publications
            bs4Dash::bs4Card(
              title = unique(pubList$title)[i],
              solidHeader = TRUE,
              width = 12,
              status = "navy",
              collapsible = TRUE,
              inputId = paste0("pub_title", i),

              ## publications
              lapply(1:ui_rows, function(i) {
                
                ## we will place all publications in one row per title
                ui_parts_rows[[i]] <-
                  shiny::fluidRow(
                    style = "display: flex;
                             margin-top: 15px;
                             margin-bottom: 30px;
                            justify-content: space-between;
                            ",

                    ## if All countries are selected by default, we show all publications
                    if (sel_country() == "All") {
                      
                      ## each publication occupies it's own column (of a width of 3)
                      lapply(1:ui_cols, function(j) {
                        ui_parts_cols[[j]] <-
                          shiny::column(
                            width = 3,
                            pub_function(
                              image = pubList_per_title$image[j],
                              link = pubList_per_title$weblink[j],
                              title = pubList_per_title$title[j],
                              country = pubList_per_title$country[j],
                              year = pubList_per_title$year[j],
                              authors = pubList_per_title$authors[j]
                            )
                          )
                      })

                      ## else if a country is selected ....
                    } else {
                      shiny::column(
                        width = 3,

                        ## and if its report exists (based on title)
                        if (sel_country() %in% pubList_per_title$country) {
                          
                          ## display the publication. pub_function() is found in "auxilliary/fun_publications.R"
                          pub_function(
                            image = pubList_per_title$image[pubList_per_title$country == sel_country()],
                            link = pubList_per_title$weblink[pubList_per_title$country == sel_country()],
                            title = pubList_per_title$title[pubList_per_title$country == sel_country()],
                            country = pubList_per_title$country[pubList_per_title$country == sel_country()],
                            year = pubList_per_title$year[pubList_per_title$country == sel_country()],
                            authors = pubList_per_title$authors[pubList_per_title$country == sel_country()]
                          )

                          ## else if its report doesn't exist, display a message alerting the user of the same.
                        } else {
                          shiny::HTML(paste0("No report found for ", sel_country()))
                        }
                      )
                    }
                  )
              })
            )
        })
      })
    }
  )
}


## Add this line to the ui
## publicationsUI("id_name_here")

## Add this line to the server
## publicationsServer("id_name_here")
##
## "id_name_here" should be the same in both ui and server