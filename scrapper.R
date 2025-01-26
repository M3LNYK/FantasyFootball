library(tidyverse)
library(rvest)
library(httr)
library(stringi)
library(xml2)

# Web Scraping Function for Football Player Statistics
scrape_football_players_stats <- function(competition_url) {
  tryCatch({
    # Verbose logging function
    verbose_log <- function(message) {
      cat(message, "\n")
    }

    # Step 1: Fetch the competition page
    verbose_log("Fetching competition page...")
    competition_response <- GET(competition_url,
                                add_headers(
                                  `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
                                ))
    competition_content <- content(competition_response, "text", encoding = "UTF-8")

    # Debugging: Print out the first 1000 characters of the content
    verbose_log("First 1000 characters of page content:")
    verbose_log(substr(competition_content, 1, 1000))

    # Attempt to find links manually
    verbose_log("Attempting to extract links...")
    all_links <- stri_extract_all_regex(competition_content, "href=[\"']([^\"']+)[\"']")[[1]]

    verbose_log("Found links:")
    verbose_log(paste(all_links, collapse = "\n"))

    # For FotMob, we might need to use a different approach
    # Look for players or stats links
    players_link_candidates <- all_links %>%
      str_subset("players|stats") %>%
      str_subset("premier-league", negate = FALSE)

    verbose_log("Potential players link candidates:")
    verbose_log(paste(players_link_candidates, collapse = "\n"))

    # If no links found, stop and provide detailed error
    if (length(players_link_candidates) == 0) {
      stop("No players links found. Please check the URL or page structure.")
    }

    # Select the first candidate link
    players_stats_link <- players_link_candidates[1] %>%
      str_extract("(?<=\").*?(?=\")")

    # Ensure absolute URL
    if (!str_starts(players_stats_link, "http")) {
      players_stats_link <- paste0("https://www.fotmob.com", players_stats_link)
    }

    verbose_log(paste("Selected players stats link:", players_stats_link))

    # Fetch players stats page
    verbose_log("Fetching players stats page...")
    players_stats_response <- GET(players_stats_link,
                                  add_headers(
                                    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
                                  ))
    players_stats_content <- content(players_stats_response, "text", encoding = "UTF-8")

    # At this point, you'll need to adapt the rest of the scraping logic
    # This is a placeholder - you'll need to customize based on the actual page structure
    verbose_log("Placeholder for further processing")

    return(NULL)

  }, error = function(e) {
    message("Error in scraping process: ", e$message)
    return(NULL)
  })
}

# Example Usage
# premier_league_url <- "https://your-football-stats-website.com/premier-league-2023-24"
# player_stats <- scrape_football_players_stats(premier_league_url)
# print(player_stats)
premier_league_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
player_stats <- scrape_football_players_stats(premier_league_url)
print(player_stats)