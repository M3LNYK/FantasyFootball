# Required libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(stringi)
library(fs)

#' Base configuration
CONFIG <- list(
  base_url = "https://www.fotmob.com",
  user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  rate_limit_delay = 2,
  max_retries = 3
)

#' Create standard HTTP headers
#' @return List of HTTP headers
create_headers <- function() {
  list(
    "Accept" = "application/json",
    "Accept-Language" = "en-US,en;q=0.9",
    "Accept-Encoding" = "gzip, deflate, br",
    "Referer" = CONFIG$base_url,
    "Origin" = CONFIG$base_url
  )
}

#' Fetch data with retry mechanism
#' @param url API endpoint URL
#' @param headers HTTP headers
#' @param max_retries Maximum number of retry attempts
#' @param delay Delay between retries in seconds
#' @return HTTP response or NULL on failure
fetch_with_retry <- function(url,
                             headers = create_headers(),
                             max_retries = CONFIG$max_retries,
                             delay = CONFIG$rate_limit_delay) {
  for (i in 1:max_retries) {
    tryCatch({
      Sys.sleep(delay)  # Rate limiting
      response <- GET(
        url,
        add_headers(.headers = headers),
        user_agent(CONFIG$user_agent)
      )

      if (status_code(response) == 200) {
        log_info(sprintf("Successfully fetched data from %s", url))
        return(response)
      } else {
        log_warn(sprintf("Attempt %d failed with status %d", i,
                         status_code(response)))
      }
    }, error = function(e) {
      log_error(sprintf("Error in attempt %d: %s", i, e$message))
    })
  }
  NULL
}

#' Parse JSON response
#' @param response HTTP response
#' @return Parsed JSON data or NULL on failure
parse_json_response <- function(response) {
  tryCatch({
    content <- rawToChar(response$content)
    fromJSON(content, flatten = TRUE)
  }, error = function(e) {
    log_error(sprintf("JSON parsing error: %s", e$message))
    NULL
  })
}

#' Process player statistics
#' @param json_data Parsed JSON data
#' @return Processed data frame
process_player_stats <- function(json_data) {
  if (!"TopLists" %in% names(json_data)) {
    log_error("Invalid JSON structure: missing TopLists")
    return(NULL)
  }

  top_lists <- json_data$TopLists
  if (length(top_lists) == 0) {
    log_error("Empty TopLists in JSON")
    return(NULL)
  }

  tryCatch({
    stat_list <- top_lists$StatList[[1]]

    if (is.null(stat_list)) {
      log_error("No StatList found in JSON")
      return(NULL)
    }

    stats_df <- as.data.frame(stat_list) %>%
      select(
        player_name = ParticipantName,
        player_id = ParticipiantId,
        team_id = TeamId,
        team_name = TeamName,
        stat_value = StatValue,
        sub_stat_value = SubStatValue,
        minutes_played = MinutesPlayed,
        matches_played = MatchesPlayed,
        stat_value_count = StatValueCount
      ) %>%
      mutate(across(where(is.character), ~na_if(., "")))

    log_info(sprintf("Processed %d player records", nrow(stats_df)))
    stats_df

  }, error = function(e) {
    log_error(sprintf("Error processing player stats: %s", e$message))
    NULL
  })
}

#' Process all player links
#' @param links Vector of URLs to process
#' @return Combined data frame of all processed data
process_all_links <- function(links) {
  log_info(sprintf("Processing %d links", length(links)))

  results <- map(links, function(link) {
    response <- fetch_with_retry(link)
    if (is.null(response)) return(NULL)

    json_data <- parse_json_response(response)
    if (is.null(json_data)) return(NULL)

    process_player_stats(json_data)
  })

  # Remove NULL results and combine
  results <- compact(results)
  bind_rows(results)
}

#' Main function to extract and process player statistics
#' @param api_link Initial API endpoint
#' @param save_path Path to save output files
#' @return Processed data frame
extract_player_stats <- function(api_link, save_path = NULL) {
  log_info("Starting player stats extraction")

  # Create save directory if needed
  if (!is.null(save_path)) {
    dir_create(save_path, recursive = TRUE)
  }

  # Fetch initial data
  response <- fetch_with_retry(api_link)
  if (is.null(response)) {
    log_error("Failed to fetch initial data")
    return(NULL)
  }

  # Parse initial JSON
  json_data <- parse_json_response(response)
  if (is.null(json_data)) return(NULL)

  # Save raw data if path provided
  if (!is.null(save_path)) {
    write_json(json_data,
               path = file.path(save_path, "raw_data.json"),
               pretty = TRUE)
  }

  # Extract links
  player_links <- json_data$pageProps$stats$players$fetchAllUrl
  if (length(player_links) == 0) {
    log_error("No player links found")
    return(NULL)
  }

  # Process all links
  results_df <- process_all_links(player_links)

  # Save processed data if path provided
  if (!is.null(save_path) && !is.null(results_df)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    write_csv(results_df,
              file = file.path(save_path,
                               sprintf("processed_stats_%s.csv", timestamp)))
  }

  results_df
}

# Usage example
if (FALSE) {  # Prevent automatic execution
  api_link <- paste0(
    "https://www.fotmob.com/_next/data/zcN7DSXE8djGgU4rVG_Jk/",
    "en/leagues/47/stats/premier-league/players.json",
    "?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"
  )

  save_path <- file.path("Project", "Main", "temp",
                         format(Sys.time(), "%Y%m%d_%H%M"))

  results <- extract_player_stats(api_link, save_path = save_path)
}
