# utils.R
library(tidyverse)
library(httr)
library(jsonlite)
library(stringi)

#' Create basic headers for the request
create_basic_headers <- function() {
  c(
    "Accept" = "application/json",
    "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "Accept-Language" = "en-US,en;q=0.9",
    "Accept-Encoding" = "gzip, deflate, br",
    "Referer" = "https://www.fotmob.com/",
    "Origin" = "https://www.fotmob.com"
  )
}

#' Fetch data from URL
fetch_data <- function(url, retry_count = 3, delay = 2) {
  cat("\nFetching data from:", url, "\n")

  for (i in 1:retry_count) {
    cat("Attempt", i, "of", retry_count, "\n")

    tryCatch({
      Sys.sleep(delay)
      response <- GET(url, add_headers(create_basic_headers()))

      if (status_code(response) == 200) {
        return(response)
      }
      cat("Request failed with status", status_code(response), "\n")
    }, error = function(e) {
      cat("Error in attempt", i, ":", e$message, "\n")
    })
  }
  NULL
}

#' Parse JSON response
parse_json_response <- function(response) {
  tryCatch({
    content <- rawToChar(response$content)
    fromJSON(content)
  }, error = function(e) {
    cat("Error parsing JSON:", e$message, "\n")
    NULL
  })
}

#' Merge stats data frames
merge_stats <- function(existing_df, new_df, stat_title) {
  if (is.null(existing_df)) return(new_df)

  full_join(
    existing_df,
    new_df,
    by = "ParticipiantId"
  ) %>%
    mutate(
      ParticipantName = coalesce(ParticipantName.x, ParticipantName.y),
      MinutesPlayed = pmax(MinutesPlayed.x, MinutesPlayed.y, na.rm = TRUE),
      MatchesPlayed = pmax(MatchesPlayed.x, MatchesPlayed.y, na.rm = TRUE)
    ) %>%
    select(-ends_with(".x"), -ends_with(".y"))
}
