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




func_custom_grp <- function(category, name, countriez){
 out <-  data.frame(Category = category, Grp = name,  Countries = countriez)
 return(out)
}

  
  
  
  
  