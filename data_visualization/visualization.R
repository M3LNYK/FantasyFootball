library(tidyverse)
library(ggplot2)


data_player <- read_csv("Project/Main/temp/data/player_stats_20250130_010255.csv")
head(data_player)

# Create the scatter plot
ggplot(data_player, aes(x = Top_scorer, y = `Expected_goals_(xG)`)) +
  # Add points
  geom_point(color = "blue", alpha = 0.6) +
  # Add labels for filtered data
  geom_text(
    data = data_player %>%
      filter(Top_scorer > 12 & `Expected_goals_(xG)` > 7),
    aes(label = ParticipantName),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  # Add title and axis labels
  labs(
    title = "Goals Scored vs Expected Goals (xG) per Season",
    x = "Goals Scored",
    y = "Expected Goals (xG)"
  ) +
  # Add theme
  theme_minimal()

data_player$`Expected_assist_(xA)`
ggplot(data_player, aes(x = Assists, y = `Expected_assist_(xA)`)) +
  # Add points
  geom_point(color = "green", alpha = 0.6) +
  # Add labels for filtered data
  geom_text(
    data = data_player %>%
      filter(Assists > 9 & `Expected_assist_(xA)` > 4),
    aes(label = ParticipantName),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5
  ) +
  # Add title and axis labels
  labs(
    title = "Assists vs Expected Assists (xA) per Season",
    x = "Assists",
    y = "Expected Assists (xA)"
  ) +
  # Add theme
  theme_minimal()

