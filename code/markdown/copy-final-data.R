file.copy(
  list.files(
    here(
      "data",
      "final"
    ),
    full.names = TRUE
  ),
  here(
    "app",
    "data"
  ),
  recursive = TRUE
)

