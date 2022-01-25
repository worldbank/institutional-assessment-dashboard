# Import variables data and variables control ==============================
source(file.path("app/auxiliary",
                 "vars-control.R"))

# Import original data =====================================================
source(file.path("code/R",
                 "01-1-import-original-data.R"))

# Import API data ==========================================================
source(file.path("code/R",
                 "01-2-import-api-data.R"))

# Import additional data ===================================================
source(file.path("code/R",
                 "01-3-import-additional-data.R"))

# Bind all data sources ====================================================
source(file.path("code/R",
                 "01-4-bind-datasets.R"))

# Calculate CTF ============================================================
source(file.path("code/R",
                 "02-indicator-calc.R"))

# Country groups ===========================================================
source(file.path("code/R",
                 "03-process-country-groups.R"))

