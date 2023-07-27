#' pub_function
#' 
#' Displays the publication cards
#'
#' @param image
#' @param title
#' @param country
#' @param year
#' @param authors
#' @param downloadID
#'
#' @return
#' @export
#'
#' @examples

pubList <- readxl::read_excel("www/publications/publicationsList.xlsx")

pub_function <- function(image,
                         title,
                         country,
                         year,
                         authors) {
  
  
  if(country %in% pubList$country[pubList$title == title]){

  shiny::div(
    style = 
     "height: 100%;
    /* grid-column-gap: 1.5625vw; */
    grid-row-gap: 1.5625vw;
    grid-template-rows: auto;
    grid-template-columns: 25% 75%;
    grid-auto-columns: 1fr;
    display: grid;
    border: 4px solid #051f3f;
    /*width: 352px; */
    ",
    shiny::div(
      style = 
        "background-color: #d8dcdb;
       justify-content: center;
        align-items: center;
        display: flex;
              ",
      shiny::img(
        src = image,
        style = "width: 80%; "
      )
    ),
    shiny::div(
      style = 
        "display: flex;
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
      ",
      shiny::span(paste0(title),
                  style = "font-size:14px; font-weight:bold; padding: 5px 0;"
      ),
      shiny::span(paste0(country),
                  style = "font-size:12px; font-weight:bold; padding: 5px 0;"
      ),
      shiny::span(paste0(year),
                  style = "font-size:10px; padding: 5px 0;"
      ),
      shiny::span(paste0(authors),
                  style = "font-size:10px; font-style:italic; padding: 5px 0;"
      )
    )
  )
}else{
  shiny::HTML(paste0("No report found for ", country))
}
}

# pub_function(image = "publications/CEM_Croatia.png",
#              title =  "Climate Change Institutional Assessment (CCIA)",
#              country = "Croatia",
#              year = "2022",
#             authors = "World Bank")
  

#' pub_function_download
#'
#' Contains code used to download publications
#' 
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
# pub_function_download <- function(filename) {
#   shiny::downloadHandler(
#     filename = filename,
#     content = function(file) {
#       file.copy(paste0("www/publications/", filename), file)
#     }
#   )
# }
