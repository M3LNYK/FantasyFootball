library(tidyverse)
library(httr)
library(stringr)

extract_player_stats_links <- function(competition_url) {
  tryCatch({
    player_stats_url <- "https://www.fotmob.com/en/leagues/47/stats/premier-league/players?season=2023-2024"

    # Get response
    stats_req <- GET(player_stats_url,
                     user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"))

    if (stats_req$status_code == 200) {
      # Convert content to text
      content_str <- rawToChar(stats_req$content)

      # Extract meta tags
      # Get specific div with class
      divs <- str_extract_all(content_str, '<div[^>]*>.*?</div>')
      print(divs)


    } else {
      message(sprintf("Request failed with status code: %d", stats_req$status_code))
      return(NULL)
    }

  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

# Example usage
competition_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
results <- extract_player_stats_links(competition_url)