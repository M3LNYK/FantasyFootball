library(tidyverse)
library(ggplot2)
library(fmsb)


data_player <- read_csv("Project/Main/temp/data/player_stats_20250130_010255.csv")
glimpse(data_player)
head(data_player)

# Create the scatter plot
ggplot(data_player, aes(x = Top_scorer, y = `Expected_goals_(xG)`)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(
    data = data_player %>%
      filter(Top_scorer > 12 & `Expected_goals_(xG)` > 7),
    aes(label = ParticipantName),
    size = 3,
    check_overlap = T
  ) +
  labs(
    title = "Goals scored vs Expected goals (xG) per Season",
    x = "Goals scored",
    y = "Expected goals (xG)"
  ) +
  theme_minimal()

# Assists vs xA
ggplot(data_player, aes(x = Assists, y = `Expected_assist_(xA)`)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_text(
    data = data_player %>%
      filter(Assists > 9 & `Expected_assist_(xA)` > 4),
    aes(label = ParticipantName),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    check_overlap = T
  ) +
  labs(
    title = "Assists vs Expected assists (xA) per Season",
    x = "Assists",
    y = "Expected assists (xA)"
  ) +
  theme_minimal()

# Top scorrers by team
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
    title = "Total goals scored by Team",
    x = "Teams",
    y = "Total goals",
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
    title = "Top 15 players by assists",
    x = "",
    y = "Assists",
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
    title = "Distribution of FotMob ratings by team",
    x = "",
    y = "FotMob rating"
  ) +
  theme_minimal()

# Box plot with mean points and statistics
ggplot(data_player,
       aes(x = reorder(TeamName, FotMob_rating, median),
           y = FotMob_rating)) +
  geom_boxplot(aes(fill = TeamName), alpha = 0.7) +
  # Add mean points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
               fill = "white") +
  stat_summary(fun = median,
               geom = "text",
               aes(label = sprintf("%.1f", ..y..)),
               vjust = -0.5) +
  coord_flip() +
  labs(
    title = "Distribution of FotMob ratings by team",
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

# Stacked Bar Chart for Cards
ggplot(
  data_player %>%
    # Filter out NA TeamName before grouping
    filter(!is.na(TeamName)) %>%
    group_by(TeamName) %>%
    summarise(
      Yellow = sum(Yellow_cards, na.rm = TRUE),
      Red = sum(Red_cards, na.rm = TRUE)
    ) %>%
    pivot_longer(cols = c(Yellow, Red),
                 names_to = "Card_Type",
                 values_to = "Count"),
  aes(x = reorder(TeamName, -Count), y = Count, fill = Card_Type)
) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("Red" = "red", "Yellow" = "yellow")) +
  labs(
    title = "Distribution of Cards by Team",
    x = "Team",
    y = "Number of Cards",
    fill = "Card Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 10)
  ) +
  geom_text(aes(label = Count),
            position = position_stack(vjust = 0.5))

##

# 1. Radar Chart comparing top scorers across multiple metrics
# Example for comparing top 3 scorers
radar_plot_top_scorers <- data_player %>%
  # Filter top 3 scorers
  arrange(desc(Top_scorer)) %>%
  head(3) %>%
  # Select relevant metrics
  select(
    ParticipantName,
    Goals = Top_scorer,
    Assists,
    `xG` = `Expected_goals_(xG)`,
    `xA` = `Expected_assist_(xA)`,
    Rating = FotMob_rating
  ) %>%
  # Prepare data for radar plot
  column_to_rownames("ParticipantName") %>%
  # Add max and min rows required by fmsb
  rbind(
    apply(., 2, max),  # Max values
    apply(., 2, min),  # Min values
    .
  )

# Create the radar plot
par(mar = c(1, 3, 3, 1))  # Adjust margins
radarchart(
  radar_plot_top_scorers,
  pcol = c("red", "blue", "green"),  # Line colors
  pfcol = scales::alpha(c("red", "blue", "green"), 0.3),  # Fill colors
  plwd = 2,  # Line width
  cglcol = "grey",  # Grid color
  cglty = 1,  # Grid line type
  axislabcol = "grey50",  # Axis label color
  caxislabels = seq(0, 1, 0.25),  # Custom axis labels
  title = "Top 3 Scorers Performance Comparison"
)
# Add legend
legend(
  "topleft",
  legend = rownames(radar_plot_top_scorers)[-(1:2)],
  col = c("red", "blue", "green"),
  lwd = 2,
  bty = "n"
)