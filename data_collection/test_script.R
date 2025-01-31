#' Process single stat data with improved error handling and debugging
#' @param stat_data Raw stat data
#' @param stat_title Title of the statistic
#' @return Processed data frame
process_stat_data <- function(stat_data, stat_title) {
  tryCatch({
    # Debug print initial data structure
    cat("\n=== Processing stat:", stat_title, "===\n")
    cat("Initial data structure:\n")
    print(str(stat_data, max.level = 2))

    # Check data structure
    if ("TopLists" %in% names(stat_data)) {
      cat("\nFound TopLists structure\n")
      if (length(stat_data$TopLists) > 0) {
        stat_list <- stat_data$TopLists[[1]]$StatList
      } else {
        cat("TopLists is empty\n")
        return(NULL)
      }
    } else if ("statList" %in% names(stat_data)) {
      cat("\nFound statList structure\n")
      stat_list <- stat_data$statList
    } else {
      cat("\nAvailable names in data:\n")
      print(names(stat_data))
      return(NULL)
    }

    # Debug print stat_list structure
    cat("\nStat list structure:\n")
    print(str(stat_list))

    if (is.null(stat_list) || length(stat_list) == 0) {
      cat("No data found in stat_list\n")
      return(NULL)
    }

    # Convert to data frame
    if (is.list(stat_list) && !is.data.frame(stat_list)) {
      cat("\nConverting list to data frame\n")
      stat_list <- bind_rows(stat_list)
    }

    # Debug print available columns
    cat("\nAvailable columns in stat_list:\n")
    print(names(stat_list))

    # Process the data frame
    stats_df <- as.data.frame(stat_list)

    # Check for required columns
    required_cols <- c("ParticipantName", "ParticipiantId", "MinutesPlayed",
                       "MatchesPlayed", "StatValue", "SubStatValue", "StatValueCount")
    available_cols <- intersect(names(stats_df), required_cols)

    cat("\nFound columns:", paste(available_cols, collapse = ", "), "\n")

    stats_df <- stats_df %>%
      select(any_of(required_cols)) %>%
      mutate(across(everything(), ~if(is.character(.)) na_if(., "")))

    # Add missing columns
    for (col in required_cols) {
      if (!col %in% names(stats_df)) {
        stats_df[[col]] <- NA
        cat("Added missing column:", col, "\n")
      }
    }

    # Rename columns
    stats_df <- stats_df %>%
      rename(
        !!sym(stat_title) := StatValue,
        !!paste0(stat_title, "_subtitle") := SubStatValue,
        !!paste0(stat_title, "_count") := StatValueCount
      ) %>%
      mutate(
        across(c(!!sym(stat_title),
                 !!sym(paste0(stat_title, "_subtitle")),
                 !!sym(paste0(stat_title, "_count"))),
               ~as.numeric(as.character(.)))
      )

    cat(sprintf("\nProcessed %d records for %s\n", nrow(stats_df), stat_title))
    cat("Final columns:", paste(names(stats_df), collapse = ", "), "\n")

    return(stats_df)

  }, error = function(e) {
    cat(sprintf("\nError processing %s: %s\n", stat_title, e$message))
    cat("Error occurred in:\n")
    print(sys.calls())
    return(NULL)
  })
}

#' Modified extract_player_stats function with additional debugging
extract_player_stats <- function(api_url) {
  cat("\n=== Starting Player Stats Extraction ===\n")

  # Initial data fetch
  cat("\nFetching initial data...\n")
  initial_response <- fetch_data(api_url)
  if (is.null(initial_response)) {
    cat("Failed to fetch initial data\n")
    return(NULL)
  }

  # Parse initial JSON
  cat("\nParsing initial JSON...\n")
  json_data <- parse_json_response(initial_response)
  if (is.null(json_data)) {
    cat("Failed to parse initial JSON\n")
    return(NULL)
  }

  # Debug print initial JSON structure
  cat("\nInitial JSON structure:\n")
  print(str(json_data$pageProps$stats$players, max.level = 2))

  # Extract player links and titles
  players_data <- json_data$pageProps$stats$players
  if (is.null(players_data)) {
    cat("No players data found\n")
    return(NULL)
  }

  # Create pairs of URLs and titles
  url_title_pairs <- tibble(
    url = players_data$fetchAllUrl,
    title = players_data$header
  )

  # Debug print URL-title pairs
  cat("\nURL-Title pairs:\n")
  print(url_title_pairs)

  url_title_pairs <- url_title_pairs %>%
    filter(!is.na(url)) %>%
    mutate(
      title = make.names(tolower(title)),
      title = str_replace_all(title, "\\.", "_")
    )

  cat(sprintf("\nFound %d stat categories to process\n", nrow(url_title_pairs)))

  # Process each URL and accumulate results
  combined_stats <- NULL

  for (i in 1:nrow(url_title_pairs)) {
    url <- url_title_pairs$url[i]
    title <- url_title_pairs$title[i]

    cat(sprintf("\n=== Processing %s (%d/%d) ===\n", title, i, nrow(url_title_pairs)))
    cat("URL:", url, "\n")

    response <- fetch_data(url)
    if (is.null(response)) {
      cat("Failed to fetch data for", title, "\n")
      next
    }

    stat_data <- parse_json_response(response)
    if (is.null(stat_data)) {
      cat("Failed to parse data for", title, "\n")
      next
    }

    # Save raw data for debugging
    debug_file <- paste0("debug_", title, ".rds")
    saveRDS(stat_data, file = debug_file)
    cat("Saved debug data to:", debug_file, "\n")

    # Process this stat category
    new_stats <- process_stat_data(stat_data, title)
    if (is.null(new_stats)) {
      cat("No data processed for", title, "\n")
      next
    }

    # Debug print new stats
    cat("\nNew stats structure:\n")
    print(str(new_stats))

    # Merge with existing data
    combined_stats <- merge_stats(combined_stats, new_stats, title)

    # Debug print combined stats
    cat(sprintf("\nCombined stats now has %d rows and %d columns\n",
                nrow(combined_stats), ncol(combined_stats)))
  }

  if (is.null(combined_stats)) {
    cat("No stats were processed successfully\n")
    return(NULL)
  }

  # Final cleanup and organization
  final_stats <- combined_stats %>%
    arrange(ParticipiantId) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))

  cat("\nFinal dataset has", nrow(final_stats), "rows and",
      ncol(final_stats), "columns\n")

  return(final_stats)
}


# After running the main code, load one of the debug files
debug_file <- list.files(pattern = "debug_.*\\.rds")[1]
if (!is.null(debug_file)) {
  debug_data <- readRDS(debug_file)
  str(debug_data, max.level = 3)
}