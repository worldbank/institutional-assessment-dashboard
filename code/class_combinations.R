## This script contains code used to create a new CLASS dataset that contains combinations of
## the CLASS variables.


library(readxl)
CLASS <- read_excel("~/Documents/Github/institutional-assessment-dashboard/data/raw/CLASS.xlsx")

## Add conflict classification as listed here: https://www.worldbank.org/en/topic/fragilityconflictviolence/brief/harmonized-list-of-fragile-situations
## and here: https://thedocs.worldbank.org/en/doc/608a53dd83f21ef6712b5dfef050b00b-0090082023/original/FCSListFY24-final.pdf

# FCS: Fragile and conflict-affected situations
# Conflict:


conflicts <- c("Afghanistan", "Burkina Faso", "Cameroon", "Central African Republic", "Congo, Democratic Republic of",
  "Ethiopia", "Iraq", "Mali", "Mozambique", "Myanmar", "Niger", "Nigeria", "Somalia", "South Sudan", "Sudan",
  "Syrian Arab Republic", "Ukraine", "West Bank and Gaza (territory)", "Yemen, Republic of")

isfs <- c("Burundi", "Chad", "Comoros", "Congo, Republic of", "Eritrea", "Guinea-Bissau", "Haiti",
  "Kiribati", "Kosovo", "Lebanon", "Libya", "Marshall Islands", "Micronesia, Federated States of",
  "Papua New Guinea", "São Tomé and Príncipe", "Solomon Islands", "Timor-Leste", "Tuvalu", "Venezuela, RB",
  "Zimbabwe")

CLASS <- CLASS %>%
           mutate(FCS = ifelse(Economy %in% conflicts, "Conflict",
                          ifelse(Economy %in% isfs, "Institutional and Social Fragility",
                            NA)))


var_names <- names(CLASS)[!names(CLASS) %in% c("Economy", "Code")]


# Function to get unique combinations
get_unique_combinations <- function(var_names) {
  all_unique_combinations <- list()
  for (i in 1:length(var_names)) {
    all_unique_combinations[[i]] <- as.data.frame(t(combn(var_names, i)))
  }
  return(all_unique_combinations)
}

# Create unique combinations of different lengths
all_unique_combinations <- get_unique_combinations(var_names)

# Filter out rows with duplicate var_names within each combination
filtered_combinations <- lapply(all_unique_combinations, function(df) {
  df[!duplicated(apply(df, 1, function(x) paste(sort(x), collapse = ","))), ]
})

## Create a dataset with these combinations
vec_combinations <- bind_rows(filtered_combinations[-1])

## Create a variable for each combination
CLASS_new <- CLASS

for (i in 1: nrow(vec_combinations)){

  vars <- unlist(as.vector(vec_combinations[i,]) , use.names = F)
  vars <- vars[!is.na(vars)]
  new_name <- paste(vars, collapse = ":")

  if(length(vars) == 2){
  var1 <-      sym(vars[1])
  var2 <-      sym(vars[2])
  CLASS_new <- CLASS_new %>%
                 rowwise %>%
                 mutate(var = paste({{var1}}, {{var2}}, sep = " : ")) %>%
                 mutate(var = ifelse(is.na({{var1}}) | is.na({{var2}}), NA, var))
  }

  if(length(vars) == 3){
  var1 <-      sym(vars[1])
  var2 <-      sym(vars[2])
  var3 <-      sym(vars[3])
  CLASS_new <- CLASS_new %>%
                 rowwise %>%
                 mutate(var = paste({{var1}}, {{var2}}, {{var3}}, sep = " : ")) %>%
                 mutate(var = ifelse(is.na({{var1}}) | is.na({{var2}})| is.na({{var3}}), NA, var))
    }

  if(length(vars) == 4){
  var1 <-      sym(vars[1])
  var2 <-      sym(vars[2])
  var3 <-      sym(vars[3])
  var4 <-      sym(vars[4])

  CLASS_new <- CLASS_new %>%
                 rowwise %>%
                 mutate(var = paste({{var1}}, {{var2}}, {{var3}}, {{var4}}, sep = " : ")) %>%
                 mutate(var = ifelse(is.na({{var1}}) | is.na({{var2}})| is.na({{var3}}) | is.na({{var4}}), NA, var))
  }

  if(length(vars) == 5){
    var1 <-      sym(vars[1])
    var2 <-      sym(vars[2])
    var3 <-      sym(vars[3])
    var4 <-      sym(vars[4])
    var5 <-      sym(vars[5])

    CLASS_new <- CLASS_new %>%
      rowwise %>%
      mutate(var = paste({{var1}}, {{var2}}, {{var3}}, {{var4}}, {{var5}}, sep = " : ")) %>%
      mutate(var = ifelse(is.na({{var1}}) | is.na({{var2}})| is.na({{var3}}) | is.na({{var4}}) | is.na({{var5}}), NA, var))
  }

  names(CLASS_new)[names(CLASS_new) == "var"] = new_name

}


CLASS_new <- CLASS_new %>%
  mutate(across(where(is.character), trimws))

CLASS_new <- CLASS_new %>%
  filter(!is.na(Region))

saveRDS(CLASS_new, "data/CLASS_new.rds")
