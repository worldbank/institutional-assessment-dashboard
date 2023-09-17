
## We can use this script to test the output displayed in the benchmark plot

# tester_function <- function(var, selected_countries, group_name){
#   
#   out <- ctf_long %>% 
#          filter(var_name %in% var) %>% 
#          filter(country_name %in% selected_countries) %>% 
#          distinct(var_name, country_name, value) %>% 
#          mutate(grp = group_name) %>% 
#          group_by(grp, var_name) %>% 
#          mutate(med_value = median(value, na.rm = TRUE))
#   return(out)
# 
# }
# 
# 
# var <- "Central bank independence"
# rich_countries <- c("Luxembourg", "Switzerland", "Norway", "Netherlands", "Germany")
# poor_countries <- c("Austria","Bahrain", "Barbados", "Cambodia")
# group_name <- "Whatever"
# 
# tester_function(var, poor_countries, group_name)


