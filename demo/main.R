# main.R
library(tidyverse)
library(httr)
library(jsonlite)
library(stringi)

#' Process single stat file
#' @param file_path Path to the debug RDS file
#' @return Processed data frame
process_debug_file <- function(file_path) {
  # Extract stat name from file name
  stat_name <- str_remove(basename(file_path), "debug_")
  stat_name <- str_remove(stat_name, "\\.rds$")

  cat("\nProcessing stat:", stat_name, "\n")

  # Read the debug file
  data <- readRDS(file_path)

  # Extract the StatList data
  if (!is.null(data$TopLists$StatList[[1]])) {
    stats_df <- data$TopLists$StatList[[1]] %>%
      as_tibble()

    cat("Available columns:", paste(names(stats_df), collapse = ", "), "\n")

    # Select and rename columns
    stats_df <- stats_df %>%
      select(
        ParticipantName,
        ParticiantId,
        TeamName,
        MinutesPlayed,
        MatchesPlayed,
        StatValue,
        SubStatValue
      ) %>%
      rename(
        player_id = ParticiantId,
        !!sym(stat_name) := StatValue,
        !!paste0(stat_name, "_subtitle") := SubStatValue
      ) %>%
      # Convert to numeric
      mutate(
        across(c(!!sym(stat_name),
                 !!sym(paste0(stat_name, "_subtitle")),
                 MinutesPlayed,
                 MatchesPlayed),
               ~as.numeric(as.character(.)))
      )

    cat("Processed", nrow(stats_df), "records\n")
    return(stats_df)
  }

  NULL
}

#' Combine all debug files into one dataset
process_all_debug_files <- function() {
  # Get all debug files
  debug_files <- list.files(pattern = "debug_.*\\.rds", full.names = TRUE)
  cat("Found", length(debug_files), "debug files to process\n")

  # Initialize empty list to store all data frames
  all_dfs <- list()

  # Process all files
  for (file in debug_files) {
    df <- process_debug_file(file)
    if (!is.null(df)) {
      all_dfs[[length(all_dfs) + 1]] <- df
    }
  }

  if (length(all_dfs) == 0) {
    cat("No data frames were processed\n")
    return(NULL)
  }

  # Combine all data frames
  cat("\nCombining all statistics...\n")
  combined_df <- all_dfs %>%
    reduce(function(x, y) {
      full_join(
        x,
        y %>% select(-MinutesPlayed, -MatchesPlayed, -TeamName),
        by = c("player_id", "ParticipantName")
      )
    })

  # Final cleanup
  final_df <- combined_df %>%
    arrange(player_id) %>%
    mutate(across(where(is.numeric), ~replace_na(., 0)))

  cat("\nFinal dataset created with", nrow(final_df), "rows and",
      ncol(final_df), "columns\n")

  return(final_df)
}

#' Save results to CSV
#' Save results to CSV with specific path
#' @param data Data frame to save
#' @param base_path Base path for saving the file
#' @return Path to the saved file
save_to_csv <- function(data, base_path = "Project/Main/temp/data") {
  # Create full path if it doesn't exist
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE)
    cat("\nCreated directory:", base_path, "\n")
  }

  # Verify path exists
  if (!dir.exists(base_path)) {
    stop("Failed to create or access directory:", base_path)
  }

  # Create filename with timestamp
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(base_path, sprintf("player_stats_%s.csv", timestamp))

  # Save the file
  tryCatch({
    write_csv(data, filename)
    cat("\nData saved successfully to:", filename, "\n")
    cat("File size:", file.size(filename), "bytes\n")

    # Verify file was created
    if (!file.exists(filename)) {
      stop("File was not created successfully")
    }

  }, error = function(e) {
    cat("\nError saving file:", e$message, "\n")
    cat("Current working directory:", getwd(), "\n")
    return(NULL)
  })

  return(filename)
}

# At the end of your main script, modify the saving part:
if (!is.null(result)) {
  cat("\n=== Results Summary ===\n")
  cat("Number of players:", nrow(result), "\n")
  cat("Number of statistics:", ncol(result) - 3, "\n")

  # Save with specific path
  saved_file <- save_to_csv(result, base_path = "Project/Main/temp/data")

  if (!is.null(saved_file)) {
    # Verify saved data
    cat("\nVerifying saved data...\n")
    if (file.exists(saved_file)) {
      verified_data <- read_csv(saved_file)
      cat("Verified rows:", nrow(verified_data), "\n")
      cat("Verified columns:", ncol(verified_data), "\n")

      # Show the actual file location
      cat("\nFile location details:\n")
      cat("Absolute path:", normalizePath(saved_file), "\n")
      cat("Directory contents:\n")
      print(list.files(dirname(saved_file)))
    } else {
      cat("Warning: File was not found after saving\n")
    }
  }
} else {
  cat("No results produced\n")
}

# Add this at the end to help debug path issues
cat("\nCurrent working directory:", getwd(), "\n")
cat("Path exists check:", dir.exists("Project/Main/temp/data"), "\n")
