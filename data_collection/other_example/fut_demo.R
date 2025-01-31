library(httr)
library(XML)
library(dplyr)
library(stringi)
library(tidyverse)

# Function to safely extract text from XML nodes
safe_extract <- function(doc, xpath) {
  tryCatch({
    result <- xpathSApply(doc, xpath, xmlValue)
    if (length(result) == 0) return(NA)
    return(result)
  }, error = function(e) {
    return(NA)
  })
}

# Function to scrape individual player page
scrape_player_page <- function(url) {
  Sys.sleep(3)

  response <- GET(
    url,
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0")
  )

  if (status_code(response) == 200) {
    doc <- htmlParse(rawToChar(response$content))

    # Extract all data using new XPaths
    player_name <- safe_extract(
      doc,
      '//*[contains(concat( " ", @class, " " ), concat( " ", "group-[.-sticky-header]:mt-0", " " ))]'
    )

    rarity <- safe_extract(
      doc,
      '//*[contains(concat( " ", @class, " " ), concat( " ", "inline-block", " " ))]//a'
    )

    price <- safe_extract(
      doc,
      '//*[(@id = "prices")]//*[contains(concat( " ", @class, " " ), concat( " ", "text-3xl", " " ))]'
      # '//tr[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "font-bold", " " )) and (((count(preceding-sibling::*) + 1) = 2) and parent::*)]'
      # '//*[contains(concat( " ", @class, " " ), concat( " ", "justify-self-end", " " ))]'
    )

    # Stats extraction
    pace <- safe_extract(
      doc,
      paste0(
        '//*[contains(concat( " ", @class, " " ), concat( " ", "xl:grid-cols-3", " " ))]',
        '//div[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]',
        '//*[contains(concat( " ", @class, " " ), concat( " ", "grid-cols-[1fr_46px]", " " ))]',
        '//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]'
      )
    )

    shooting <- safe_extract(
      doc,
      paste0(
        '//*[contains(concat( " ", @class, " " ), concat( " ", "xl:grid-cols-3", " " ))]',
        '//div[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]',
        '//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]'
      )
    )

    passing <- safe_extract(
      doc,
      paste0(
        '//*[contains(concat( " ", @class, " " ), concat( " ", "xl:grid-cols-3", " " ))]',
        '//div[(((count(preceding-sibling::*) + 1) = 3) and parent::*)]',
        '//*[contains(concat( " ", @class, " " ), concat( " ", "grid-cols-[1fr_46px]", " " ))]',
        '//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]'
      )
    )

    dribbling <- safe_extract(
      doc,
      '//div[(((count(preceding-sibling::*) + 1) = 4) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]'
    )

    defending <- safe_extract(
      doc,
      '//div[(((count(preceding-sibling::*) + 1) = 5) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]'
    )

    physicality <- safe_extract(
      doc,
      '//div[(((count(preceding-sibling::*) + 1) = 6) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "text-center", " " ))]'
    )

    free(doc)

    # Create a data frame for the player
    player_data <- data.frame(
      player_name = player_name[1],
      rarity = rarity[1],
      price = price[1],
      pace = pace[1],
      shooting = shooting[1],
      passing = passing[1],
      dribbling = dribbling[1],
      defending = defending[1],
      physicality = physicality[1],
      stringsAsFactors = FALSE
    )

    return(player_data)
  }

  return(NULL)
}
# Main scraping function
tryCatch({
  base_url <- "https://www.fut.gg/players/"

  # Function to get total pages
  get_total_pages <- function(url) {
    response <- GET(
      url,
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0")
    )

    if (status_code(response) == 200) {
      doc <- htmlParse(rawToChar(response$content))

      next_page_element <- xpathSApply(
        doc,
        paste0(
          '//*[contains(concat( " ", @class, " " ), concat( " ", "pagination__control--next", " " ))]',
          '//*[contains(concat( " ", @class, " " ), concat( " ", "pagination__link", " " )) ',
          'and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]'
        ),
        xmlGetAttr,
        "href"
      )

      free(doc)

      if (length(next_page_element) > 0) {
        page_number <- stri_extract_first_regex(next_page_element[1], "page=(\\d+)") %>%
          stri_replace_first_regex("page=", "")
        return(as.numeric(page_number))
      }
    }
    return(NULL)
  }

  # For testing with 2 pages:
  # MAX_PAGES <- 2

  # For full scraping:
  MAX_PAGES <- NULL

  # Get total pages and apply limit if specified
  total_pages <- get_total_pages(base_url)
  if (!is.null(MAX_PAGES)) {
    total_pages <- min(total_pages, MAX_PAGES)
    cat("Limited to", total_pages, "pages for testing\n")
  } else {
    cat("Processing all", total_pages, "pages\n")
  }

  if (!is.null(total_pages)) {
    cat("Total pages to scrape:", total_pages, "\n")

    # Initialize list to store data
    all_data <- list()

    # Create progress bar for pages
    pb_pages <- txtProgressBar(min = 1, max = total_pages, style = 3)

    # First phase: Collect all player links
    for (page in 1:total_pages) {
      current_url <- if (page == 1) {
        base_url
      } else {
        stri_c(base_url, "?page=", page)
      }

      response <- GET(
        current_url,
        user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:108.0) Gecko/20100101 Firefox/108.0")
      )

      if (status_code(response) == 200) {
        doc <- htmlParse(rawToChar(response$content))

        # Extract player card links
        player_links <- xpathSApply(
          doc,
          '//*[contains(concat( " ", @class, " " ), concat( " ", "fut-card", " " ))]/parent::a',
          xmlGetAttr,
          "href"
        )

        all_data[[page]] <- data.frame(
          player_link = player_links,
          stringsAsFactors = FALSE
        )

        free(doc)
        setTxtProgressBar(pb_pages, page)
        Sys.sleep(2)
      }
    }

    close(pb_pages)

    # Combine all links
    player_links_df <- do.call(rbind, all_data)

    # Second phase: Scrape individual player pages
    cat("\nStarting to scrape individual player pages...\n")
    pb_players <- txtProgressBar(min = 1, max = nrow(player_links_df), style = 3)

    all_player_data <- list()

    for (i in seq_len(nrow(player_links_df))) {
      player_url <- player_links_df$player_link[i]

      # Make sure URL is absolute
      if (!stri_startswith_fixed(player_url, "http")) {
        player_url <- file.path("https://www.fut.gg", player_url)
      }

      # Scrape player page
      player_data <- scrape_player_page(player_url)

      if (!is.null(player_data)) {
        all_player_data[[i]] <- player_data
      }

      setTxtProgressBar(pb_players, i)
    }

    close(pb_players)

    # Combine all player data
    final_player_data <- do.call(rbind, all_player_data)
    rownames(final_player_data) <- NULL

    # Save detailed data to CSV
    write.csv(
      final_player_data,
      file = stri_c("fifa_players_detailed_", Sys.Date(), ".csv"),
      row.names = FALSE
    )

    cat("\nScraping completed!\n")
    cat("Total players processed:", nrow(final_player_data), "\n")
    cat("Data saved to CSV file\n")
  }

}, error = function(e) {
  cat("\nError occurred:", stri_c("Error: ", e$message), "\n")
  NULL
}, warning = function(w) {
  cat("\nWarning occurred:", stri_c("Warning: ", w$message), "\n")
  NULL
})
