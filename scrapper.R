library(tidyverse)
library(httr)
library(stringi)

extract_player_stats_links <- function(competition_url) {
  tryCatch({
    player_stats_url <- "https://www.fotmob.com/en/leagues/47/stats/premier-league/players?season=2023-2024"

    # Get response
    stats_req <- GET(player_stats_url,
                     user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"))

    if (stats_req$status_code == 200) {
      # Convert content to text
      content_str <- rawToChar(stats_req$content)

      # Pattern to match css-[alphanumeric]-Column [alphanumeric]
      class_pattern <- 'class="css-[a-zA-Z0-9]+-Column\\s+[a-zA-Z0-9]+"'

      # Extract divs with this class pattern using stringi
      matching_divs <- stri_extract_all_regex(content_str,
                                              sprintf('<div[^>]*%s[^>]*>.*?</div>', class_pattern))

      stats_page_css_pattern <- 'class="css-[a-zA-Z0-9]+-TLStatsPageCSS\\s+[a-zA-Z0-9]+"'

      demo <- stri_extract_all_regex(matching_divs,
                                     sprintf('<section[^>]*%s[^>]*>.*?</div>', stats_page_css_pattern))

      print(demo)

      # New regex pattern for "StatContainerCSS" divs
      stat_container_pattern <- 'class="css-[a-zA-Z0-9]+-StatContainerCSS\\s+[a-zA-Z0-9]+"'

      # Extract matching divs inside the previously found div
      stat_containers <- stri_extract_all_regex(matching_divs[[1]],
                                                sprintf('<div[^>]*%s[^>]*>.*?</div>', stat_container_pattern))

      # print(stat_containers)

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