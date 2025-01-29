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
      # Get content and decode if needed
      # content_str <- rawToChar(stats_req$content)
      content(stats_req) %>%
        html_elements("span")


      # Look for specific elements with our target classes
      # class_patterns <- list(
      #   stats = '(?s)<tr[^>]*class="[^"]*e15r3kn28[^"]*">(.*?)</tr>',
      #   player = '(?s)<td[^>]*class="[^"]*e15r3kn26[^"]*">(.*?)</td>'
      # )
      #
      # # Try to find matches for each pattern
      # for (name in names(class_patterns)) {
      #   matches <- stri_extract_all_regex(content_str, class_patterns[[name]])[[1]]
      #   cat("\nFound", length(matches), name, "elements\n")
      #
      #   if (!is.null(matches) && length(matches) > 0 && !all(is.na(matches))) {
      #     cat("First 3 matches:\n")
      #     for (i in 1:min(3, length(matches))) {
      #       # Clean up the content
      #       clean_match <- stri_replace_all_regex(matches[i], "<[^>]+>", " ")
      #       clean_match <- stri_trim_both(clean_match)
      #       cat(i, ":", clean_match, "\n")
      #     }
      #   }
      # }
      #
      # Also try to find any elements with these classes
      # all_elements <- stri_extract_all_regex(
      #   content_str,
      #   '(?s)<[^>]*class="[^"]*(?:e15r3kn28|e15r3kn26)[^"]*"[^>]*>(.*?)</[^>]*>'
      # )[[1]]
      #
      # cat("\nTotal elements with target classes:", length(all_elements), "\n")
      # if (!is.null(all_elements) && length(all_elements) > 0 && !all(is.na(all_elements))) {
      #   cat("First few elements found:\n")
      #   for (i in 1:min(3, length(all_elements))) {
      #     clean_element <- stri_replace_all_regex(all_elements[i], "<[^>]+>", " ")
      #     clean_element <- stri_trim_both(clean_element)
      #     cat(i, ":", clean_element, "\n")
      #   }
      # }
      #
      # return(NULL)
    } else {
      message(sprintf("Request failed with status code: %d", stats_req$status_code))
      return(NULL)
    }
  } error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

# Example usage
competition_url <- "https://www.fotmob.com/leagues/47/stats/season/20720/players/goals/premier-league-players"
result <- extract_stats_table(competition_url)
