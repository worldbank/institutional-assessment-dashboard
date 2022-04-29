pacman::p_load("tidyverse","here")

# Import variables data and variables control ==============================
source(file.path("app/auxiliary",
                 "vars-control.R"))

# Import original data =====================================================
source(file.path("code/R",
                 "01-import-original-data.R"))

# Import API data ==========================================================
source(file.path("code/R",
                 "02-import-api-data.R"))

# Import additional data ===================================================
source(file.path("code/R",
                 "03-import-additional-data.R"))

# Bind all data sources ====================================================
source(file.path("code/R",
                 "04-bind-datasets.R"))

# Calculate CTF ============================================================
source(file.path("code/R",
                 "05-indicator-calc.R"))

# Country groups ===========================================================
source(file.path("code/R",
                 "06-process-country-groups.R"))

# Create Layer for Map ===========================================================
source(file.path("code/R",
                 "07-create-layer-map.R"))

rm(list = ls(all.names = TRUE))
gc()


