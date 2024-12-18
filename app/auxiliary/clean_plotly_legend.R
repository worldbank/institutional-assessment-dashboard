# Source: https://stackoverflow.com/questions/69289623/avoid-legend-duplication-in-plotly-conversion-from-ggplot-with-facet-wrap
# 
# 
# -----------------------------------------------------------------------------
# Function: clean_plotly_legend
# -----------------------------------------------------------------------------
# Purpose:
# Simplifies and cleans the legend of a Plotly object, particularly useful for 
# handling duplicated legends in facetted ggplots converted to Plotly.

# Parameters:
# - .pltly_obj: The Plotly object to be cleaned.
# - .new_legend: (Optional) A vector of new legend group names to replace existing ones.

# Functionality:
# - Cleans legend group names, removes duplicates, and updates legend visibility.
# - Ensures legends are grouped logically and hidden where unnecessary.


clean_plotly_legend <- function(.pltly_obj, .new_legend = c()) {
  # Cleans up a plotly object legend, particularly when ggplot is facetted
  
  assign_leg_grp <- function(.legend_group, .leg_nms) {
    # Assigns a legend group from the list of possible entries
    # Used to modify the legend settings for a plotly object
    
    leg_nms_rem <- .leg_nms
    
    parse_leg_nms <- function(.leg_options) {
      # Assigns a .leg_name, if possible
      # .leg_options is a 2-element list: 1 = original value; 2 = remaining options
      
      if (is.na(.leg_options)) {
        .leg_options
      } else if(length(leg_nms_rem) == 0) {
        # No more legend names to assign
        .leg_options
      } else {
        # Transfer the first element of the remaining options
        leg_nm_new <- leg_nms_rem[[1]]
        leg_nms_rem <<- leg_nms_rem[-1]
        
        leg_nm_new
      }
      
    }
    
    .legend_group %>% 
      map(~ parse_leg_nms(.))
    
  }
  
  simplify_leg_grps <- function(.legendgroup_vec) {
    # Simplifies legend groups by removing brackets, position numbers and then de-duplicating
    
    leg_grp_cln <-
      map_chr(.legendgroup_vec, ~ str_replace_all(., c("^\\(" = "", ",\\d+\\)$" = "")))
    
    modify_if(leg_grp_cln, duplicated(leg_grp_cln), ~ NA_character_)
    
  }
  
  pltly_obj_data <-
    .pltly_obj$x$data
  
  pltly_leg_grp <-
    # pltly_leg_grp is a character vector where each element represents a legend group. Element is NA if legend group not required or doesn't exist
    pltly_obj_data%>% 
    map(~ pluck(., "legendgroup")) %>% 
    map_chr(~ if (is.null(.)) {NA_character_} else {.}) %>%
    # Elements where showlegend = FALSE have legendgroup = NULL. 
    
    simplify_leg_grps() %>% 
    
    assign_leg_grp(.new_legend) 
  
  pltly_obj_data_new <-
    pltly_obj_data %>% 
    map2(pltly_leg_grp, ~ list_modify(.x, legendgroup = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, name = .y)) %>%
    map2(pltly_leg_grp, ~ list_modify(.x, showlegend = !is.na(.y)))
  # i.e. showlegend set to FALSE when is.na(pltly_leg_grp), TRUE when not is.na(pltly_leg_grp)
  
  .pltly_obj$x$data <- pltly_obj_data_new
  
  .pltly_obj
  
}
