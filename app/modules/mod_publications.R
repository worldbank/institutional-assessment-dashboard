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
  ns <- NS(id)
  tagList(
    shinyWidgets::useBs4Dash(),

    tags$style(
      "
  .pubs:hover {
    -webkit-transform: scale(1.01,1.01);
    -webkit-transition-timing-function: ease-out;
    -webkit-transition-duration: 250ms;
    -moz-transform: scale(1.05,1.07);
    -moz-transition-timing-function: ease-out;
    -moz-transition-duration: 250ms;
    position: relative;
    z-index: 99;
} 
  "
    ),
    
    ## 
    shiny::fluidRow(style = "height:15px"),
    
    ## Country
    column(
      width = 3,
      pickerInput(
        ns("country"),
        label = "Select a country",
        choices = c("All", countries),
        selected = "All",
        multiple = FALSE,
        options = list(
          size = 20,
          `actions-box` = TRUE
        )
      )
    ),
    
    shiny::fluidRow(style = "height:15px"),
    
    ## Each row will contain 3 publications, each with a width of 4. A whole row has a width of 12
        uiOutput(ns('publications'))
    )
  
}

publicationsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
     
      sel_country <- reactive({
        input$country
      })
      
      
      ## pub_function() is documented in the fun_publications.R file
      ## contained in the auxiliary folder
    
      
      output$publications <- renderUI({

        ## Each card will be a publications title
        n_cards <- length(unique(pubList$title))
        ui_cards <- c()
        
        lapply(1:n_cards, function(i) {

          ## Each card will hold publications for the different countries, all in one
          ## row, that will be scrollable
          
          ui_parts_rows <- c()
          ui_parts_cols <- c()
          
          ui_tot_pubs_per_card <- length(pubList$title[pubList$title == unique(pubList$title)[i]])
          ui_cols <- ui_tot_pubs_per_card
          ui_rows <- 1
          
          pubList_per_card <- pubList %>% dplyr::filter(title %in% unique(pubList$title)[i])
          
          ui_cards[[i]] <- bs4Dash::bs4Card(
              title = unique(pubList$title)[i],
              solidHeader = TRUE,
              width = 12,
              status = "navy",
              collapsible = TRUE,
              inputId = paste0("pub_card", i),
                
              ## Country selector
              shiny::fluidRow(),
              
              ## Publications
                lapply(1:ui_rows, function(i) {
                  
                  ui_parts_rows[[i]] <-
                    
                    shiny::fluidRow(
                      style = "display: flex;
                       margin-top: 15px;
                       margin-bottom: 30px;
                       justify-content: space-between;
                     ",
                      
                      if(sel_country() == "All"){
                        
                      
                      lapply(1:ui_cols, function(j) {
                        
                        ui_parts_cols[[j]] <-
                          
                          shiny::column(
                            width = 3,
                              pub_function(image = pubList_per_card$image[j],
                                           link = pubList_per_card$weblink[j],
                                           title = pubList_per_card$title[j],
                                           country = pubList_per_card$country[j],
                                           year = pubList_per_card$year[j],
                                           authors = pubList_per_card$authors[j]
                              )
                          )
                            }
                          )
                      }else{
                        shiny::column(
                          width = 3,
                          if(sel_country() %in% pubList_per_card$country){
                            pub_function(image = pubList_per_card$image[pubList_per_card$country == sel_country()],
                                         link = pubList_per_card$weblink[pubList_per_card$country == sel_country()],
                                         title = pubList_per_card$title[pubList_per_card$country == sel_country()],
                                         country = pubList_per_card$country[pubList_per_card$country == sel_country()],
                                         year = pubList_per_card$year[pubList_per_card$country == sel_country()],
                                         authors = pubList_per_card$authors[pubList_per_card$country == sel_country()]
                            )
                          }else{
                            shiny::HTML(paste0("No report found for ", sel_country()))
                          }
                            
                         
                        )
                      }
                      
                    )
                        
                        
                      })
                    )
                  
                  
                  
                })
          
                
              })

        })

}



library(shiny)

ui <- fluidPage(
  publicationsUI("sth") 
)

server <- function(input, output, session) {
  publicationsServer("sth")
}

shinyApp(ui, server)
