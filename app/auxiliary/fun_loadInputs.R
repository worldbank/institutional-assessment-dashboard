#' buttons
#'
#' @description Generates app buttons
#'
#' @param id Input id
#' @param lab Button label
#' @param id icon. Defaults to None
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

buttons_func <- function(id, lab) {
  
  div(class = "load_save_btns", 
    shinyWidgets::actionBttn(
      inputId = id,
      label = lab, 
      icon = shiny::icon("upload"),
      style = "jelly",
      color = "primary",
      size = "sm"
    )
  )
  
}


#' Construct user app data directory path
#'
#' @description
#' Consists of three pieces of information:
#'
#' - System location
#' - name
#'
#' @importFrom rappdirs user_data_dir
#'
#' @return Character. Path to user's app data directory
user_data_dir <- function() {
  
  dir <- rappdirs::user_data_dir(
    appname = "CLIAR"
  )
  
  return(dir)
  
}


#' check_input_file_exists
#'
#' @description
#' Check if the input file exists in the user_data_dir() directory

#' @import fs
#'
#' @return Character. Path to user's app data directory
check_input_file_exists <- function(){
  fs::file_exists(fs::path(user_data_dir(), "cliar_inputs.rds"))
}


#' toast_messages_func
#'
#' @description Displays toast messages
#' @param type success or errors
#' @param text the message to be displayed
#'
#' @return a toast message informing the end user of an action that has just been carried out
#' @export
#'
#' @noRd
#'
toast_messages_func <- function(type, text) {
  shinyFeedback::showToast(
    type = type,
    message = text,
    .options = list(
      preventDuplicates = TRUE,
      positionClass = "toast-bottom-right"
    )
  )
}

#' modal_function
#'
#' @description Displays modal messages
#' @param title title of the message
#' @param text the message to be displayed
#'
#' @return a modal message 
#' @export
#'
#' @noRd
#'
modal_function <- function(title, mes){
  shiny::showModal(shiny::modalDialog(
    title = title,
    mes,
    footer = shiny::modalButton("Dismiss"),
  ))
}


