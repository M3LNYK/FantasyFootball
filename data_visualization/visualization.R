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
    point.padding = 0.5,
    check_overlap = T
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
    point.padding = 0.5,
    check_overlap = T
  ) +
  # Add title and axis labels
  labs(
    title = "Assists vs Expected Assists (xA) per Season",
    x = "Assists",
    y = "Expected Assists (xA)"
  ) +
  # Add theme
  theme_minimal()

# Top scorrers by team
# Second version - Enhanced bar plot with colors and formatting
ggplot(
  data_player %>%
    group_by(TeamName) %>%
    summarise(
      total_goals = sum(Top_scorer, na.rm = TRUE),
      avg_goals = mean(Top_scorer, na.rm = TRUE)
    ) %>%
    arrange(desc(total_goals)),
  aes(x = reorder(TeamName, total_goals), y = total_goals)
) +
  geom_bar(stat = "identity", aes(fill = total_goals)) +
  coord_flip() +
  scale_fill_viridis_c() +  # Color gradient
  geom_text(
    aes(label = sprintf("%.0f", total_goals)),
    hjust = -0.2,
    size = 3
  ) +
  labs(
    title = "Total Goals Scored by Team",
    x = "Teams",
    y = "Total Goals",
    fill = "Goals"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "none"
  )

ggplot(
  data_player %>%
    arrange(desc(Assists)) %>%
    head(15),  # Top 15 players
  aes(x = reorder(ParticipantName, Assists), y = Assists, fill = TeamName)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(
    aes(label = sprintf("%d", Assists)),
    hjust = -0.2,
    size = 3
  ) +
  labs(
    title = "Top 15 Players by Assists",
    subtitle = "Premier League 2023/24 Season",
    x = "",
    y = "Number of Assists",
    fill = "Team"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0)) +  # Adjust y-axis limits
  guides(fill = guide_legend(nrow = 2))  # Arrange legend in two rows


# Box plot
# Basic box plot
ggplot(data_player, aes(x = reorder(TeamName, FotMob_rating, median),
                        y = FotMob_rating)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +  # Horizontal orientation for better readability
  labs(
    title = "Distribution of FotMob Ratings by Team",
    subtitle = "Premier League 2023/24 Season",
    x = "",
    y = "FotMob Rating"
  ) +
  theme_minimal()

# Box plot with mean points and statistics
ggplot(data_player,
       aes(x = reorder(TeamName, FotMob_rating, median),
           y = FotMob_rating)) +
  # Add box plot
  geom_boxplot(aes(fill = TeamName), alpha = 0.7) +
  # Add mean points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white") +
  # Add labels for median values
  stat_summary(fun = median,
               geom = "text",
               aes(label = sprintf("%.1f", ..y..)),
               vjust = -0.5) +
  coord_flip() +
  labs(
    title = "Distribution of FotMob Ratings by Team",
    subtitle = "Premier League 2023/24 Season\nWhite diamonds show mean values",
    x = "",
    y = "FotMob Rating"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  )

