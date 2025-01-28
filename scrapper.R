library(tidyverse)
library(httr)
library(stringi)
library(jsonlite)

extract_player_stats_links <- function(api_link) {
  tryCatch({
    # Make the GET request
    response <- GET(api_link,
                    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"))

    # Check if the request was successful
    if (status_code(response) == 200) {
      # Convert raw content to text
      json_text <- rawToChar(response$content)

      # Parse JSON
      json_data <- fromJSON(json_text)

      # Examine the structure
      str(json_data)

      # Print available top-level keys
      cat("\nTop-level keys in JSON:\n")
      print(names(json_data))

      # Function to recursively explore nested lists
      explore_nested <- function(data, prefix = "") {
        if (is.list(data)) {
          for (name in names(data)) {
            cat(sprintf("%s%s\n", prefix, name))
            explore_nested(data[[name]], paste0(prefix, "  "))
          }
        }
      }

      # Explore the nested structure
      cat("\nNested structure:\n")
      explore_nested(json_data)

      return(json_data)
    } else {
      stop(sprintf("API request failed with status code: %d", status_code(response)))
    }

  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

# Usage example:
api_link <- "https://www.fotmob.com/_next/data/bhtACeKZ4e_Lvxde8cpLT/en/leagues/47/stats/premier-league/players.json?lng=en&id=47&tab=stats&slug=premier-league&slug=players"
json_result <- extract_player_stats_links(api_link)