#Function that Removes Average Aggregate Items from Datasets
remove_average_items <- function(family) {
  family_filtered <- family[!grepl("Average", family)]
  return(family_filtered)
}