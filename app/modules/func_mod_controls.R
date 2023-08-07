CLASS_new <- readRDS("data/CLASS_new.rds")


combi_names <- grep(":", names(CLASS_new), value = TRUE, ignore.case = TRUE)
other_names <- names(CLASS_new)[!names(CLASS_new) %in% c("Economy", "Code", combi_names)]

func_comp_cat_choices <- function(comp_cat) {
  if (length(comp_cat) != 1) {
    for (i in 1:length(combi_names)) {
      split_combi_names <- strsplit(combi_names[i], split = ":")[[1]]

      if (all(comp_cat %in% split_combi_names) &
        length(comp_cat) == length(split_combi_names)) {
        label <- "xxx"
        choices <- unique(CLASS_new[, combi_names[i]])[!is.na(unique(CLASS_new[, combi_names[i]]))]
      }
    }
  }

  if (length(comp_cat) == 1) {
    label <- "xxx"
    choices <- unique(CLASS_new[, comp_cat])[!is.na(unique(CLASS_new[, comp_cat]))]
  }

  out <- list(label, choices)
  return(out)
}


func_custom_grp <- function(category, name, countriez) {
  out <- data.frame(Category = category, Grp = name, Countries = countriez)
  return(out)
}


predetermined_groups_table_func <- function(comp_cat, comp_cat_choices) {
  
  if (!is.null(comp_cat) & !is.null(comp_cat_choices)) {
    checker <- apply(CLASS_new, 2, function(x) any(comp_cat_choices %in% x))
    var <- names(checker[checker == TRUE])
    var_sym <- dplyr::sym(var)

    country_selection <- CLASS_new %>%
      select(Economy, {{ var_sym }}) %>%
      filter({{ var_sym }} %in% comp_cat_choices) %>% 
      mutate(Category = var) %>%
      rename(Grp = {{ var_sym }}, Countries = Economy) %>%
      select(Category, Grp, Countries) %>% 
      arrange(Grp)
  } else {
    country_selection <- NULL
  }

  return(country_selection)
}