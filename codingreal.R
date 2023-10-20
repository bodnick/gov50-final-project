library(tidyverse)
library(dplyr)
library(ggplot2)

games <- read_csv("games.csv")

#Comparing wins

wins_table <- games |>
  group_by(SEASON) |>
  summarize(percentage_home_team_wins = mean(HOME_TEAM_WINS),
            percentage_away_team_wins = 1 - mean(HOME_TEAM_WINS)
  )

long_wins_table <- wins_table |>
  pivot_longer(
    cols = c(percentage_home_team_wins, percentage_away_team_wins),
    names_to = "win_type",
    values_to = "percentage"
  )

wins_barplot <- ggplot(long_wins_table, aes(x = SEASON, y = percentage, fill = win_type)) +
  geom_bar(stat="identity", position="stack") +
  labs(title = "The home team consistently wins more than half of games in the NBA",
       y = "Percentage of wins",
       x = "Season") +
  scale_fill_manual(values = c("percentage_home_team_wins" = "#3498db", 
                               "percentage_away_team_wins" = "#e74c3c"),
                    labels = c("Home team", "Visiting team"),
                    name = "Win rate type")
  theme_minimal()

wins_barplot

#Comparing points

points_table <- games |>
  group_by(SEASON) |>
  summarize(avg_score_difference = mean(PTS_home, na.rm = TRUE) - 
              mean(PTS_away, na.rm = TRUE)
  )

points_barplot <- ggplot(points_table, aes(x = SEASON, y = avg_score_difference)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title = "Average Score Difference by Season",
       y = "Average Score Difference",
       x = "Season") +
  theme_minimal()

points_barplot

#Comparing free throw percentages

ft_table <- games |>
  group_by(SEASON) |>
  summarize(avg_ft_difference = (mean(FT_PCT_home, na.rm = TRUE)) - 
              (mean(FT_PCT_away, na.rm = TRUE))
  )

ft_barplot <- ggplot(ft_table, aes(x = SEASON, y = avg_ft_difference)) +
  geom_bar(stat="identity", fill="darkorchid") + 
  geom_hline(yintercept = 0, linetype="dashed", color="black") +  # Adds a horizontal line at 0 for reference
  labs(title = "Average Free Throw Percentage Difference by Season",
       y = "Average FT% Difference",
       x = "Season") +
  theme_minimal()

ft_barplot

# home_wins_barplot <- ggplot(
#     home_wins_table,
#     aes(x = SEASON, y = percentage_home_team_wins)
# ) + 
#   geom_bar(stat="identity", fill="blue") +
#   labs(title = "Percentage of Home Team Wins by Season",
#        y = "Percentage of Wins",
#        x = "Season") +
#   theme_minimal()



