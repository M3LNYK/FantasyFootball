library(tidyverse)
library(rvest)
library(httr2)
library(stringi)
library(xml2)

# Web Scraping Function for Football Player Statistics
extract_filtered_links <- function(competition_url) {
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
    ) %>%
      # Filter out unwanted links
      filter(
        !stri_detect_regex(clean_link, "\\.css", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "/news", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "/transfers", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "android", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "twitter", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "facebook", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "careers", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "company", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "instagram", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "tiktok", opts_regex = stri_opts_regex(case_insensitive = TRUE)) &
          !stri_detect_regex(clean_link, "faq", opts_regex = stri_opts_regex(case_insensitive = TRUE))
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
player_stats <- extract_filtered_links(premier_league_url)
player_stats %>%
  select(clean_link) %>%
  print(n=60)