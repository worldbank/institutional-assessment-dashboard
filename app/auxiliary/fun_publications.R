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
pub_function <- function(image,
                         title,
                         country,
                         year,
                         authors,
                         downloadID) {
  shiny::div(
    style = 
    "height: 100%;
    /* grid-column-gap: 1.5625vw; */
    grid-row-gap: 1.5625vw;
    grid-template-rows: auto;
    grid-template-columns: 25% 75%;
    grid-auto-columns: 1fr;
    display: grid;
    border-top: 4px solid #051f3f;
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
      padding: 15px;
      text-align: center;
      color: #051f3f;'
      ",
      shiny::span(paste0(title),
        style = "font-size:20px; font-weight:bold; padding-top: 15px;"
      ),
      shiny::br(),
      shiny::span(paste0(country),
        style = "font-size:18px; font-weight:bold;"
      ),
      shiny::br(),
      shiny::span(paste0(year),
        style = "font-size:16px;"
      ),
      shiny::br(),
      shiny::span(paste0(authors),
        style = "font-size:16px; font-style:italic;"
      ),
      shiny::br(),
      shiny::downloadButton(
        outputId = downloadID,
        "Download",
        style = "color: white; background-color: #33a2b7;
                                          text-align: center;"
      )
    )
  )
}

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
pub_function_download <- function(filename) {
  shiny::downloadHandler(
    filename = filename,
    content = function(file) {
      file.copy(paste0("www/publications/", filename), file)
    }
  )
}
