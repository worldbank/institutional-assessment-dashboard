#' pub_function
#' 
#' Displays the publication cards. The main div (class = "pubs") contains two divs, 
#' one that carries the image (class = "pubs_image") and another
#' that holds the publication metadata (class = "pubs_content"). These details are read from 
#' the data/publicationsList.xlsx file saved in OneDrive
#'
#' @param image
#' @param title
#' @param country
#' @param year
#' @param authors
#'
#' @return
#' @export

## Load the file that contains publications metadata

## Note: Please pick the publicationsList.xlsx file from data/publicationsList.xlsx in 
## OneDrive and paste it in app/data/ to always get the latest list of publications

pubList <- readxl::read_excel("data/publicationsList.xlsx")

## Function that displays the publications as cards
pub_function <- function(image,
                         link,
                         title,
                         country,
                         year,
                         authors) {

  ## A link that redirects users to the publication site is embedded on the card  -----------
  tags$a(href = link,target="_blank",

  shiny::div(
    class = "pubs",
    
    ## image of publication -----------
    shiny::div(
      class = "pubs_image",
      shiny::img(
        src = image,
        style = "width: 80%; "
      )
    ),
    
    ## metadata -----------
    
    shiny::div(
      class = "pubs_content",
      shiny::span(paste0(title),
                  style = "font-size:16px; font-weight:bold; padding: 5px 0;"
      ),
      shiny::span(paste0(country),
                  style = "font-size:14px; font-weight:bold; padding: 5px 0;"
      ),
      shiny::span(paste0(year),
                  style = "font-size:12px; padding: 5px 0;"
      ),
      shiny::span(paste0(authors),
                  style = "font-size:12px; font-style:italic; padding: 5px 0;"
      )
    )
  )
  )
}
