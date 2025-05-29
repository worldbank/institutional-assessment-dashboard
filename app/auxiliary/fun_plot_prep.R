#Plotting Prep Functions:

#======= Custom Item: this function prepares customItems
customItem <- 
  function(text, 
           icon = shiny::icon("warning"),
           href = NULL, ...) {
    
    if (is.null(href)) 
      
      tags$li(
        a(href = href, icon, text, class = "nav-link", target = "_blank"),
        class = "nav-item"
      )
  }

#=========== Bivariate Correlation Functions
# This function establishes x_axis variable choices
x_scatter_choices <- function(yvar){
  
  extract_xvar_choices <-
    function(x, yvar) {
      db_variables %>%
        dplyr::filter(
          var_name != yvar
        ) %>% 
        dplyr::filter(
          family_name == x 
        ) %>%
        pull(var_name)
    }
  
  xvar_choice_list <- purrr::map2(family_names$var_name, yvar, extract_xvar_choices)
  names(xvar_choice_list) <- family_names$var_name
  
  xvar_choice_list <- c("Log GDP per capita, PPP",xvar_choice_list)
  
  return(xvar_choice_list)
}