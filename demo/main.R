# main.R
library(tidyverse)
library(httr)
library(jsonlite)
library(stringi)

# Load utility functions
source("Project\\Main\\demo\\utils.R")
source("Project\\Main\\demo\\stats_processor.R")

#' Main function to fetch and process all statistics
#' @param api_url URL of the main JSON
#' @return Combined data frame of all statistics
process_all_stats <- function(api_url) {
  # Step 1: Fetch the main JSON
  main_response <- fetch_data(api_url)
  if (is.null(main_response)) {
    cat("Failed to fetch main JSON\n")
    return(NULL)
  }

  main_json <- parse_json_response(main_response)
  if (is.null(main_json)) {
    cat("Failed to parse main JSON\n")
    return(NULL)
  }

  # Step 2: Extract statistic URLs and titles
  stats <- main_json$pageProps$stats$players
  stat_urls <- stats$fetchAllUrl
  stat_titles <- stats$header

  # Step 3: Initialize empty list to store all data frames
  all_dfs <- list()

  # Step 4: Fetch and process each statistic
  for (i in seq_along(stat_urls)) {
    url <- stat_urls[i]
    title <- stri_replace_all_regex(stat_titles[i], "\\s+", "_")

    cat("\nProcessing stat:", title, "\n")

    response <- fetch_data(url)
    if (is.null(response)) {
      cat("Failed to fetch data for", title, "\n")
      next
    }

    stat_data <- parse_json_response(response)
    if (is.null(stat_data)) {
      cat("Failed to parse data for", title, "\n")
      next
    }

    df <- process_stat_data(stat_data, title)
    if (!is.null(df)) {
      all_dfs[[length(all_dfs) + 1]] <- df
    }
  }

  # Step 5: Combine all data frames
  if (length(all_dfs) == 0) {
    cat("No data frames were processed\n")
    return(NULL)
  }

  combined_df <- all_dfs %>%
    reduce(function(x, y) {
      full_join(
        x,
        y %>% select(-MinutesPlayed, -MatchesPlayed, -TeamName),
        by = c("player_id", "ParticipantName")
      )
    })

  # Step 6: Final cleanup
  final_df <- combined_df %>%
    arrange(player_id) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))

  cat("\nFinal dataset created with", nrow(final_df), "rows and",
      ncol(final_df), "columns\n")

  return(final_df)
}

# API URL
api_url <- "https://www.fotmob.com/_next/data/zcN7DSXE8djGgU4rVG_Jk/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"
api_url <- "https://www.fotmob.com/_next/data/N1P1Woq7NeGHTBddylp3m/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"

# Process all statistics
cat("\n=== Fetching and Processing All Statistics ===\n")
result <- process_all_stats(api_url)

if (!is.null(result)) {
  cat("\n=== Results Summary ===\n")
  cat("Number of players:", nrow(result), "\n")
  cat("Number of statistics:", ncol(result) - 3, "\n") # Excluding basic columns

  cat("\nColumns in final dataset:\n")
  print(names(result))

  cat("\nFirst few rows:\n")
  print(head(result))

  # Save to CSV
  saved_file <- save_to_csv(result)

  # Verify saved data
  cat("\nVerifying saved data...\n")
  verified_data <- read_csv(saved_file)
  cat("Verified rows:", nrow(verified_data), "\n")
  cat("Verified columns:", ncol(verified_data), "\n")
} else {
  cat("No results produced\n")
}
