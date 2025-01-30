# stats_processor.R
library(tidyverse)
library(stringi)

#' Process single statistic data
#' @param stat_data Raw statistic data
#' @param stat_title Name of the statistic
#' @return Processed data frame
process_stat_data <- function(stat_data, stat_title) {
  tryCatch({
    # Extract the StatList data
    if (!is.null(stat_data$TopLists$StatList[[1]])) {
      stats_df <- stat_data$TopLists$StatList[[1]] %>%
        as_tibble()

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
          !!sym(stat_title) := StatValue,
          !!paste0(stat_title, "_subtitle") := SubStatValue
        ) %>%
        # Convert to numeric
        mutate(
          across(c(!!sym(stat_title),
                   !!sym(paste0(stat_title, "_subtitle")),
                   MinutesPlayed,
                   MatchesPlayed),
                 ~as.numeric(as.character(.)))
        )

      cat("Processed", nrow(stats_df), "records for", stat_title, "\n")
      return(stats_df)
    }

    NULL
  }, error = function(e) {
    cat("Error processing", stat_title, ":", e$message, "\n")
    NULL
  })
}
