library(tidyverse)
library(httr)
library(stringi)
library(rvest)

extract_player_stats <- function(competition_url) {
  tryCatch({
    stats_req <- GET(
      competition_url,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36")
    )


    if (stats_req$status_code == 200) {
      content(stats_req) %>%
        html_elements("div")

      #   content_str <- rawToChar(stats_req$content)
    #
    #   # Let's print the first part of content to see what we're working with
    #   # cat("First 2000 characters of content:\n")
    #   # cat(substr(content_str, 1, 2000))
    #
    #   # Find the main stats container
    #   stats_container_pattern <- '(?s)<div[^>]*class="[^"]*StatsTable[^"]*"[^>]*>.*?</div>'
    #   stats_container <- stri_extract_first_regex(content_str, stats_container_pattern)
    #
    #   if (!is.na(stats_container)) {
    #     # Extract player rows
    #     player_row_pattern <- '(?s)<tr[^>]*class="[^"]*TableRow[^"]*"[^>]*>.*?</tr>'
    #     player_rows <- stri_extract_all_regex(stats_container, player_row_pattern)[[1]]
    #
    #     # Initialize data frame to store results
    #     player_stats <- data.frame(
    #       rank = character(),
    #       player = character(),
    #       team = character(),
    #       goals = character(),
    #       stringsAsFactors = FALSE
    #     )
    #
    #     for (row in player_rows) {
    #       # Extract individual stats
    #       rank_pattern <- '<td[^>]*>([0-9]+)</td>'
    #       player_pattern <- '<td[^>]*class="[^"]*PlayerName[^"]*"[^>]*>(.*?)</td>'
    #       team_pattern <- '<td[^>]*class="[^"]*TeamName[^"]*"[^>]*>(.*?)</td>'
    #       goals_pattern <- '<td[^>]*class="[^"]*StatValue[^"]*"[^>]*>(.*?)</td>'
    #
    #       rank <- stri_extract_first_regex(row, rank_pattern)
    #       player <- stri_extract_first_regex(row, player_pattern)
    #       team <- stri_extract_first_regex(row, team_pattern)
    #       goals <- stri_extract_first_regex(row, goals_pattern)
    #
    #       # Add to data frame
    #       player_stats <- rbind(player_stats,
    #                             data.frame(
    #                               rank = rank,
    #                               player = player,
    #                               team = team,
    #                               goals = goals,
    #                               stringsAsFactors = FALSE
    #                             ))
    #     }
    #
    #     return(player_stats)
    #   }
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
competition_url <- "https://www.fotmob.com/leagues/47/stats/season/20720/players/goals/premier-league-players"
stats <- extract_player_stats(competition_url)
