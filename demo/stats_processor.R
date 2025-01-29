# stats_processor.R
source("E:/UNI_stuff/2-sem/Web Scraping/Project/Main/demo/utils.R")

#' Process single stat data
process_stat_data <- function(stat_data, stat_title) {
  # Your process_stat_data function code here
  # (The full function from your test_script.R)
}

#' Extract player stats
extract_player_stats <- function(api_url) {
  # Your extract_player_stats function code here
  # (The full function from your test_script.R)
}

#' Save results to CSV
save_to_csv <- function(data, base_path = "Project/Main/temp/data/s") {
  if (!dir.exists(base_path)) {
    dir.create(base_path, recursive = TRUE)
  }

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(base_path, sprintf("player_stats_2_%s.csv", timestamp))

  write_csv(data, filename)
  cat("\nData saved to:", filename, "\n")

  filename
}
