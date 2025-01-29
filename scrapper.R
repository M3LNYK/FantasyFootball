library(tidyverse)
library(httr)
library(stringi)

extract_stats_table <- function(competition_url) {
  tryCatch({
    stats_req <- GET(
      competition_url,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36")
    )

    if (stats_req$status_code == 200) {
      content_str <- rawToChar(stats_req$content)

      # Remove style tags first
      content_no_style <- stri_replace_all_regex(
        content_str,
        '(?s)<style[^>]*>.*?</style>',
        ''
      )

      # Patterns for both classes
      patterns <- list(
        # First class pattern (e15r3kn28)
        pattern1 = '(?s)<[^>]*class="[^"]*e15r3kn28[^"]*"[^>]*>.*?<span[^>]*>(.*?)</span>',
        # Second class pattern (e15r3kn26)
        pattern2 = '(?s)<[^>]*class="[^"]*e15r3kn26[^"]*"[^>]*>(.*?)</[^>]*>'
      )

      # Extract elements for both patterns
      for (name in names(patterns)) {
        elements <- stri_extract_all_regex(content_no_style, patterns[[name]])[[1]]
        cat(name, "elements found:", length(elements), "\n")
        if (length(elements) > 0) {
          cat("\nFirst few", name, "elements:\n")
          for (i in 1:min(5, length(elements))) {
            # Clean up the content (remove HTML tags)
            clean_content <- stri_replace_all_regex(elements[i], "<[^>]+>", "")
            clean_content <- stri_trim_both(clean_content)
            cat(i, ":", clean_content, "\n")
          }
        }
      }

      return(NULL)
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
result <- extract_stats_table(competition_url)
