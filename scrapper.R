library(tidyverse)
library(rvest)  # Not used in this specific code, but might be helpful for future scraping
library(httr2)
library(stringi)
library(xml2)  # Not used in this specific code, but might be helpful for future scraping

extract_player_stats_links <- function(competition_url) {
  tryCatch({
    player_stats_url <- "https://www.fotmob.com/_next/data/pvJAuiZ8YEQispGXaw-zM/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"

    # Create the request object
    stats_req <- request(player_stats_url) %>%
      req_headers(
        `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36",
        `Accept` = "*/*"
      )

    # Perform the request
    stats_resp <- req_perform(stats_req)

    # Check if stats_resp is a valid response object
    if (!is.response(stats_resp)) {
      message("Error: stats_resp is not a valid HTTP response object.")
      return(list())
    }

    # Check for HTTP errors
    if (resp_status(stats_resp) != 200) {
      message(paste("HTTP error:", resp_status(stats_resp), resp_status_desc(stats_resp)))
      return(list())
    }

    # Process the JSON response
    if (http_type(stats_resp) == "application/json") {
      json_data <- resp_body_json(stats_resp)
      player_stats <- json_data$pageProps$pageData$competition$players
      return(player_stats)
    } else {
      message("Request did not return JSON. Response type: ", http_type(stats_resp))
      return(list())
    }
  }, error = function(e) {
    message("Error: ", e$message)
    return(list())
  })
}

# Example usage
competition_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
player_stats <- extract_player_stats_links(competition_url)

# Example of how to access the data
if (!is.null(player_stats)) {
  for (player in player_stats) {
    print(paste(player$name$fullName, ": Goals -", player$stats$main$goals$value))
  }
}