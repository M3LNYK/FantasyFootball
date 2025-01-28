library(tidyverse)
library(httr)
library(jsonlite)
library(stringi)

extract_player_stats_links <- function(api_link, save_path = NULL) {
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

      # Save the raw JSON if path is provided
      if (!is.null(save_path)) {
        # Save as RDS (R object)
        saveRDS(json_data, file = paste0(save_path, "_data.rds"))

        # Save as JSON file
        write_json(json_data, path = paste0(save_path, "_data.json"), pretty = TRUE)

        # Save as CSV if it's a data frame
        if (is.data.frame(json_data)) {
          write_csv(json_data, file = paste0(save_path, "_data.csv"))
        }

        cat(sprintf("Data saved to %s with different extensions\n", save_path))
      }

      # here I want to continue working



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
time <- format(Sys.time(), "%Y%m%d_%H%M")
base_path <- "Project/Main/temp/"
path <- paste0(base_path, time)

api_link <- "https://www.fotmob.com/_next/data/uKh5nioc0sOEP3_mHPgRL/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"

# Save to directory and get results
result <- extract_player_stats_links(api_link, save_path = path)

# Access the JSON data
json_data <- result$json_data

# Access the extracted URLs
urls_df <- result$urls

# Print found URLs
print(urls_df)