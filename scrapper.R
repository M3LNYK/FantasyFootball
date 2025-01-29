library(tidyverse)
library(httr)
library(stringi)
library(rvest)

extract_stats_table <- function(competition_url) {
  tryCatch({
    stats_req <- GET(
      competition_url,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36")
    )

    if (stats_req$status_code == 200) {
      content_str <- rawToChar(stats_req$content)

      # Pattern to match elements with class e15r3kn213
      # span_pattern <- '(?s)<span[^>]*class="[^"]*e15r3kn213[^"]*"[^>]*>(.*?)</span>'
      # spans <- stri_extract_all_regex(content_str, span_pattern)[[1]]

      offers <- content_str %>%
        html_elements('h2 a') %>%
        html_attr('href') %>% print()

      # Clean up the matches (remove HTML tags)
      # if (!is.null(spans) && length(spans) > 0) {
      #   clean_spans <- spans %>%
      #     stri_replace_all_regex("<[^>]+>", "") %>%
      #     stri_trim_both()
      #
      #   cat("Found spans:", length(clean_spans), "\n")
      #   cat("Content:\n")
      #   print(clean_spans)
      #
      #   return(clean_spans)
      # } else {
      #   cat("No matching spans found\n")
      #   return(NULL)
      # }

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
# competition_url <- "https://www.fotmob.com/leagues/47/stats/season/20720/players/goals/premier-league-players"
competition_url <- "https://www.fotmob.com/api/matches?date=20250129&timezone=Europe%2FWarsaw&ccode3"
  result <- extract_stats_table(competition_url)
