library(tidyverse)
library(rvest)
library(httr2)
library(stringi)
library(xml2)

# Web Scraping Function for Football Player Statistics
extract_fotmob_links <- function(competition_url) {
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

    # Extract all links
    all_links <- stri_extract_all_regex(content, "href=[\"']([^\"']+)[\"']")[[1]]

    # Process links
    processed_links <- tibble(link = all_links) %>%
      # Extract actual link content
      mutate(
        clean_link = str_extract(link, "(?<=\").*?(?=\")"),
        # Categorize links
        is_stats = str_detect(clean_link, "leagues/47/stats/premier-league"),
        is_players = str_detect(clean_link, "players"),
        is_english = str_detect(clean_link, "en-GB"),
        is_absolute = str_starts(clean_link, "http")
      ) %>%
      # Mutate to create full URLs
      mutate(
        full_url = case_when(
          is_absolute ~ clean_link,
          str_starts(clean_link, "/") ~ paste0("https://www.fotmob.com", clean_link),
          TRUE ~ paste0("https://www.fotmob.com/", clean_link)
        )
      )

    # Return processed links
    return(processed_links)

  }, error = function(e) {
    message("Error in link extraction: ", e$message)
    return(NULL)
  })
}

# Example Usage
# premier_league_url <- "https://your-football-stats-website.com/premier-league-2023-24"
# player_stats <- scrape_football_players_stats(premier_league_url)
# print(player_stats)
premier_league_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
player_stats <- extract_fotmob_links(premier_league_url)
player_stats %>%
  filter(is_stats | is_players) %>%
  select(full_url, is_english) %>%
  print( n = 30)