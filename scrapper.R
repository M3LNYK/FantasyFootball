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

      # data_frame %>% select(json_data$pageProps$stats$players$header, $fetchAllUrl)
      # Testing stuff
      # print(data_frame(json_data$pageProps$stats$players$header, $fetchAllUrl))
      demo_df = json_data$pageProps$stats$players
      # print(demo_df$fetchAllUrl)
      print(subset(demo_df, select = c("header", "fetchAllUrl")))


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

api_link_14 <-"https://www.fotmob.com/_next/data/XIZGn_V5s8J4GpjMVz7ts/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"
api_link_15 <-"https://www.fotmob.com/_next/data/XIZGn_V5s8J4GpjMVz7ts/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"
api_link <-"https://www.fotmob.com/_next/data/zcN7DSXE8djGgU4rVG_Jk/en/leagues/47/stats/premier-league/players.json?season=2023-2024&lng=en&id=47&tab=stats&slug=premier-league&slug=players"
# Get results
result <- extract_player_stats_links(api_link, save_path = path)