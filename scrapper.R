library(tidyverse)
library(httr)
library(jsonlite)
library(stringi)

extract_player_stats_links <- function(api_link, save_path = NULL) {
  tryCatch({
    response <- GET(api_link,
                    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36"))

    if (status_code(response) == 200) {
      json_text <- rawToChar(response$content)
      json_data <- fromJSON(json_text)

      # Save the raw data if path is provided
      if (!is.null(save_path)) {
        write_json(json_data, path = paste0(save_path, "_data.json"), pretty = TRUE)

        if (is.data.frame(json_data)) {
          write_csv(json_data, file = paste0(save_path, "_data.csv"))
        }
        cat(sprintf("Data saved to %s with different extensions\n", save_path))
      }

      # First, let's print the structure to debug
      str(json_data, max.level = 3)

      # Extract stats specifically from the players section
      extract_stats <- function(data) {
        # Try to access the data with more detailed path checking
        players_stats <- data$pageProps$data$stats$players

        if (!is.null(players_stats)) {
          # Create vectors to store our data
          stat_names <- character()
          fetch_urls <- character()
          headers <- character()

          # Loop through each stat entry
          for (stat in players_stats) {
            stat_names <- c(stat_names, stat$name)
            fetch_urls <- c(fetch_urls, stat$fetchAllUrl)
            headers <- c(headers, stat$header)
          }

          # Create the dataframe
          stats_df <- data.frame(
            stat_name = stat_names,
            fetch_url = fetch_urls,
            header = headers,
            stringsAsFactors = FALSE
          )

          return(stats_df)
        }

        # If we couldn't find the data, print the available paths
        # print("Available paths in data:")
        # print(names(data))
        if (!is.null(data$pageProps)) {
          print("Available paths in pageProps:")
          print(names(data$pageProps))
        }

        return(NULL)
      }

      # Get the stats
      stats_df <- extract_stats(json_data)

      # Save stats to CSV if path is provided and stats were found
      if (!is.null(save_path) && !is.null(stats_df)) {
        write_csv(stats_df, file = paste0(save_path, "_stats_urls.csv"))
      }

      # Return both the original JSON data and the extracted stats
      return(list(
        json_data = json_data,
        stats = stats_df
      ))

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

# Get results
result <- extract_player_stats_links(api_link, save_path = path)

# Access the stats dataframe
stats_df <- result$stats

# View the structure of the result
str(result$json_data, max.level = 3)

# View the extracted stats and URLs
print(stats_df)