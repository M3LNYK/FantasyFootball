library(tidyverse)
library(rvest)
library(httr)
library(stringi)

# Web Scraping Function for Football Player Statistics
scrape_football_players_stats <- function(competition_url) {
  # Error handling wrapper
  tryCatch({
    # Step 1: Fetch the competition page
    competition_page <- GET(competition_url) %>%
      content(as = "text", encoding = "UTF-8")

    # Step 2: Extract stats link
    stats_link <- competition_page %>%
      read_html() %>%
      html_elements("a[href*='stats']") %>%
      html_attr("href") %>%
      first()

    # Ensure absolute URL
    if (!str_starts(stats_link, "http")) {
      base_url <- urltools::url_parse(competition_url)$scheme
      stats_link <- paste0(base_url, "://", base_url$domain, stats_link)
    }

    # Step 3: Fetch stats page and get players stats link
    players_stats_page <- GET(stats_link) %>%
      content(as = "text", encoding = "UTF-8")

    players_stats_link <- players_stats_page %>%
      read_html() %>%
      html_elements("a[href*='players']") %>%
      html_attr("href") %>%
      first()

    # Ensure absolute URL
    if (!str_starts(players_stats_link, "http")) {
      players_stats_link <- paste0(base_url, "://", base_url$domain, players_stats_link)
    }

    # Step 4: Prepare for data collection
    all_players_data <- list()
    current_page <- 1

    # Use slowly() for rate limiting
    safe_get <- slowly(~{
      Sys.sleep(runif(1, 1, 3))  # Random delay between 1-3 seconds
      GET(.x)
    }, rate = rate_delay(1, 1))

    # Pagination and data collection
    repeat {
      # Construct paginated URL
      paginated_url <- paste0(players_stats_link, "?page=", current_page)

      # Fetch page with rate limiting
      page_response <- safe_get(paginated_url)
      page_content <- content(page_response, as = "text", encoding = "UTF-8")

      # Parse HTML
      page_html <- read_html(page_content)

      # Extract tables
      tables <- page_html %>%
        html_table()

      # Check if no more data
      if (length(tables) == 0 || nrow(tables[[1]]) == 0) {
        break
      }

      # Process each table
      for (table in tables) {
        # Clean and structure data
        player_data <- table %>%
          janitor::clean_names() %>%
          mutate(across(everything(), as.character))

        all_players_data[[length(all_players_data) + 1]] <- player_data
      }

      # Check total entries to determine pagination
      total_entries <- page_html %>%
        html_elements(".pagination-info") %>%
        html_text() %>%
        str_extract("\\d+$") %>%
        as.numeric()

      if (nrow(tables[[1]]) < total_entries) {
        current_page <- current_page + 1
      } else {
        break
      }
    }

    # Step 5: Combine data
    final_players_data <- bind_rows(all_players_data)

    return(final_players_data)

  }, error = function(e) {
    # Comprehensive error handling
    message("Error in scraping process: ", e$message)
    return(NULL)
  })
}

# Example Usage
# premier_league_url <- "https://your-football-stats-website.com/premier-league-2023-24"
# player_stats <- scrape_football_players_stats(premier_league_url)
# print(player_stats)