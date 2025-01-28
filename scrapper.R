library(tidyverse)
library(rvest)  # Not used in this specific code, but might be helpful for future scraping
library(httr2)
library(httr)
library(stringi)
library(xml2)  # Not used in this specific code, but might be helpful for future scraping

extract_player_stats_links <- function(competition_url) {
  tryCatch({
    api_link <- "https://www.fotmob.com/_next/data/bhtACeKZ4e_Lvxde8cpLT/en/leagues/47/stats/premier-league/players.json?lng=en&id=47&tab=stats&slug=premier-league&slug=players"
    main_json <- GET(api_link,
                     user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"))

    # Analysis of main_json



  }, error = function(e) {
    message("Error: ", e$message)
    return(list())
  })
}

# Example usage
competition_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
player_stats <- extract_player_stats_links(competition_url)

api_link <- "https://www.fotmob.com/_next/data/bhtACeKZ4e_Lvxde8cpLT/en/leagues/47/stats/premier-league/players.json?lng=en&id=47&tab=stats&slug=premier-league&slug=players"
extract_player_stats_links(competition_url)