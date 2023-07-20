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

    ## Each row will contain 3 publications, each with a width of 4. A whole row has a width of 12
    shiny::fluidRow(
      shiny::column(
        width = 4,
        ## pub_function() is documented in the fun_publications.R file
        ## contained in the auxiliary folder
        pub_function(
          image = "publications/CEM_Croatia.png",
          title = "Country Economic Memorandum (CEM)",
          country = "Croatia",
          year = "2022",
          authors = "Author 1: Jane Doe",
          downloadID = ns("download_pubs_1")
        )
      ),
      shiny::column(
        width = 4,
        pub_function(
          image = "publications/CCIA_Mozambique.png",
          title = "Climate Change Institutional Assessment (CCIA)",
          country = "Mozambique",
          year = "2022",
          authors = "Author 1: John Doe",
          downloadID = ns("download_pubs_2")
        )
      )
    )
  )
}

publicationsServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Publication downloads
      output$download_pubs_1 <- pub_function_download("Croatia CEM.pdf")
      output$download_pubs_2 <- pub_function_download("Mozambique CCIA  -  Mainstreaming Climate Change in Governance - P172569_1.pdf")
    }
  )
}

# ui <- fluidPage(
#   publicationsUI("publications")
# )
#
# server <- function(input, output, session) {
#   publicationsServer("publications")
# }
#
# shinyApp(ui, server)
