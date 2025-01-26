library(tidyverse)
library(rvest)
library(httr2)
library(stringi)
library(xml2)

# Web Scraping Function for Football Player Statistics
scrape_football_players_stats <- function(competition_url) {
  tryCatch({
    # Create a request object with httr2
    req <- request(competition_url) %>%
      req_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"
      )

    # Send the request and get the response
    resp <- req_perform(req)

    # Get the content as text
    content <- resp_body_string(resp)

    # Extract all links that might be relevant to stats or players
    all_links <- stri_extract_all_regex(content, "href=[\"']([^\"']+)[\"']")[[1]]

    # Filter links for stats pages, preferring English version
    stats_links <- all_links %>%
      str_subset("leagues/47/stats/premier-league") %>%
      # Prioritize English links
      str_subset("en-GB", negate = FALSE)

    # If no English links, use any available
    if (length(stats_links) == 0) {
      stats_links <- all_links %>% str_subset("leagues/47/stats/premier-league")
    }

    # Ensure we have a link
    if (length(stats_links) == 0) {
      stop("No stats links found")
    }

    # Clean and prepare the link
    stats_link <- stats_links[1] %>%
      str_extract("(?<=\").*?(?=\")") %>%
      # Ensure absolute URL
    {if(str_starts(., "http")) . else paste0("https://www.fotmob.com", .)}

    # Add season parameter if not already present
    if (!str_detect(stats_link, "season=")) {
      stats_link <- paste0(stats_link, "?season=2023-2024")
    }

    # Verbose output
    message("Using stats link: ", stats_link)

    # Create a new request for the stats page
    stats_req <- request(stats_link) %>%
      req_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"
      )

    # Perform the request
    stats_resp <- req_perform(stats_req)
    stats_content <- resp_body_string(stats_resp)

    # At this point, you'll need to inspect the stats_content
    # Save the content to a file for inspection
    writeLines(stats_content, "stats_page_content.txt")

    message("Stats page content saved to stats_page_content.txt")

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