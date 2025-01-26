library(tidyverse)
library(rvest)
library(httr)
library(stringi)

# Web Scraping Function for Football Player Statistics
library(tidyverse)
library(httr)
library(stringi)
library(xml2)

# Web Scraping Function for Football Player Statistics
scrape_football_players_stats <- function(competition_url) {
  # Error handling wrapper
  tryCatch({
    # Step 1: Fetch the competition page
    competition_response <- GET(competition_url)
    competition_content <- content(competition_response, "text", encoding = "UTF-8")

    # Step 2: Extract stats link using string manipulation
    stats_link <- stri_extract_first_regex(competition_content,
                                           "href=[\"']([^\"']*stats[^\"']*)[\"']",
                                           opts_regex = stri_opts_regex(case_insensitive = TRUE)) %>%
      str_extract("(?<=\").*?(?=\")")

    # Ensure absolute URL
    if (is.na(stats_link)) stop("Could not find stats link")
    if (!str_starts(stats_link, "http")) {
      base_url <- urltools::url_parse(competition_url)$domain
      stats_link <- paste0("https://", base_url, stats_link)
    }

    # Step 3: Fetch stats page and get players stats link
    stats_response <- GET(stats_link)
    stats_content <- content(stats_response, "text", encoding = "UTF-8")

    # Extract players stats link
    players_stats_link <- stri_extract_first_regex(stats_content,
                                                   "href=[\"']([^\"']*players[^\"']*)[\"']",
                                                   opts_regex = stri_opts_regex(case_insensitive = TRUE)) %>%
      str_extract("(?<=\").*?(?=\")")

    # Ensure absolute URL
    if (is.na(players_stats_link)) stop("Could not find players stats link")
    if (!str_starts(players_stats_link, "http")) {
      base_url <- urltools::url_parse(stats_link)$domain
      players_stats_link <- paste0("https://", base_url, players_stats_link)
    }

    # Step 4: Prepare for data collection
    all_players_data <- list()
    current_page <- 1

    # Use slowly() for rate limiting
    safe_get <- slowly(~{
      Sys.sleep(runif(1, 1, 3))  # Random delay between 1-3 seconds
      GET(.x)
    }, rate = rate_delay(1, 1))

    # Function to extract table data using regex
    extract_table_data <- function(content) {
      # Split content into potential table rows
      rows <- stri_split_regex(content, "(<tr[^>]*>|</tr>)")[[1]]

      # Extract cell data
      table_data <- lapply(rows, function(row) {
        cells <- stri_extract_all_regex(row, "<td[^>]*>(.*?)</td>",
                                        opts_regex = stri_opts_regex(case_insensitive = TRUE))
        if (!is.null(cells[[1]])) {
          # Clean cell contents
          cleaned_cells <- lapply(cells[[1]], function(cell) {
            # Remove HTML tags
            clean_cell <- stri_replace_all_regex(cell, "<[^>]*>", "")
            # Trim whitespace
            clean_cell <- stri_trim_both(clean_cell)
            return(clean_cell)
          })
          return(cleaned_cells)
        }
        return(NULL)
      })

      # Remove NULL and empty entries
      table_data <- table_data[!sapply(table_data, is.null)]
      table_data <- table_data[lengths(table_data) > 0]

      # Convert to dataframe
      if (length(table_data) > 0) {
        df <- do.call(rbind, lapply(table_data, function(x) {
          # Pad or truncate to ensure consistent length
          x <- x[1:min(length(x), 10)]
          x <- c(x, rep(NA, 10 - length(x)))
          as.data.frame(t(x))
        }))

        # Use generic column names
        names(df) <- paste0("V", 1:ncol(df))
        return(df)
      }

      return(NULL)
    }

    # Pagination and data collection
    repeat {
      # Construct paginated URL
      paginated_url <- paste0(players_stats_link, "?page=", current_page)

      # Fetch page with rate limiting
      page_response <- safe_get(paginated_url)
      page_content <- content(page_response, "text", encoding = "UTF-8")

      # Extract table data
      current_table_data <- extract_table_data(page_content)

      # Check if no more data
      if (is.null(current_table_data) || nrow(current_table_data) == 0) {
        break
      }

      # Store data
      all_players_data[[length(all_players_data) + 1]] <- current_table_data

      # Check for pagination (you may need to adjust this based on the specific website)
      # This is a placeholder and might need customization
      if (nrow(current_table_data) < 20) {  # Assuming 20 rows per page
        break
      }

      current_page <- current_page + 1
    }

    # Step 5: Combine data
    if (length(all_players_data) > 0) {
      final_players_data <- bind_rows(all_players_data)
      return(final_players_data)
    }

    return(NULL)

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
premier_league_url <- "https://www.fotmob.com/en-GB/leagues/47/stats/premier-league?season=2023-2024"
