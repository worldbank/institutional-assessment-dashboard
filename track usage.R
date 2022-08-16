#=================== Get RStudio Connect credentials ==========================#

library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(assertthat)
library(httr)
library(here)

get_credentials <- function(server = "internal") {
  
  # Check that server selection is valid
  assert_that(server %in% c("internal", "external"),
              msg = "Unrecognized server specified. The server argument can only take the values 'internal' and 'external'.")
  
  # Get credential from system environment
  server_env <- paste0(toupper(server),
                       "_SERVER")
  key_env <- paste0(toupper(server),
                    "_KEY")
  
  # Return credentials
  list(server = Sys.getenv(server_env),
       key = Sys.getenv(key_env))
  
}


credentials <- get_credentials(server = "internal")

# Request a page of up to 25 usage records.
resp <- GET(
  paste0(credentials$server, "__api__/v1/instrumentation/shiny/usage?limit=25"),
  add_headers(Authorization = paste("Key", credentials$key))
)
payload <- content(resp)
views <- content(resp)$results %>% bind_rows()

# Continue to page through additional records
# while we have a "next" reference
while(!is.null(payload$paging[["next"]])) {
  resp <- GET(
    payload$paging[["next"]],
    add_headers(Authorization = paste("Key", credentials$key))
  )
  payload <- content(resp)
  page <- content(resp)$results %>% bind_rows()
  # print the results on this page
  views <-
    views %>% bind_rows(page)
}

views <-
  views %>%
    filter(
      content_guid == "b05a3993-7ff7-4f25-9c54-99522f1a3bd6",
      user_guid != "ecc2e516-1467-440d-9bbd-a4550d1daa31" | is.na(user_guid)
    ) %>%
  mutate(
    date = as_date(started),
    started = as_datetime(started),
    ended = as_datetime(ended),
    timespent = difftime(ended, started, units='mins'),
    week = week(date),
    year = year(date),
    time = as.Date(paste(year, week, 1, sep = "-"), "%Y-%U-%u")
  )

views_bydate <-
  views %>%
    group_by(time) %>%
    summarise(
      viewers = n(),
      timespent = sum(timespent)
    )
  
  ggplot(
    views_bydate,
    aes(
      x = time,
      y = timespent
    )
  ) +
    geom_area(fill = "#69b3a2", alpha = 0.4) +
    geom_line(color = "#69b3a2", size = 2) +
    geom_point(size = 3, color = "#69b3a2") +
    theme_ipsum() +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    labs(
      x = "Date",
      y = "Weekly usage (minutes)"
    )
  
  ggplot(
    views_bydate,
    aes(
      x = time,
      y = viewers
    )
  ) +
    geom_segment(
      aes(
        y = 0,
        yend = viewers,
        xend = time
      ),
      color = "#69b3a2", 
      alpha = 0.4,
      size = 1.5
    ) +
    geom_point(size = 3, color = "#69b3a2") +
    theme_ipsum() +
    theme(
      panel.grid.minor.x = element_blank()
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b"
    ) +
    labs(
      x = "Date",
      y = "Weekly views"
    )
    
  
