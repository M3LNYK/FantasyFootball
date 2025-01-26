library(tidyverse)
library(rvest)
library(httr2)
library(stringi)
library(xml2)

# Web Scraping Function for Football Player Statistics
extract_all_links <- function(competition_url) {
  tryCatch({
    # Create a request object
    req <- request(competition_url) %>%
      req_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
      )

    # Send the request and get the response
    resp <- req_perform(req)

    # Get the content as text
    content <- resp_body_string(resp)

    # Extract ALL links using stringi
    all_links <- stri_extract_all_regex(content, "href=[\"']([^\"']+)[\"']")[[1]]

    # Create a tibble with the links
    processed_links <- tibble(
      original_link = all_links,
      clean_link = stri_extract_first_regex(all_links, "(?<=\").*?(?=\")")
    )

    return(processed_links)

  }, error = function(e) {
    message("Error in link extraction: ", e$message)
    return(tibble())
  })
}

# Example Usage
# premier_league_url <- "https://your-football-stats-website.com/premier-league-2023-24"
# player_stats <- scrape_football_players_stats(premier_league_url)
# print(player_stats)
premier_league_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
player_stats <- extract_fotmob_links(premier_league_url)
player_stats %>%
  select(full_url) %>%
  print(n=50)