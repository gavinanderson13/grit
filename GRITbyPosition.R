library(dplyr)
library(ggplot2)
library(broom)
library(scales)
library(readr)
library(ggpmisc)  

players_data <- read_csv("player-match-ratings-3-cleaned.csv")

score_map <- c(
  WM  = "WM.score",
  CB  = "CB.score",
  CDM = "CMD.score",
  CMA = "CMA.score",
  ST  = "ST.score",
  FB  = "FB.score",
  GK  = "GK.score"
)

# --- Create Position_Score ---
players_data <- players_data %>%
  rowwise() %>%
  mutate(
    Position_Score = if (!is.na(score_map[position2])) get(score_map[position2]) else NA_real_
  ) %>%
  ungroup() %>%
  filter(!is.na(Position_Score))

# --- Determine match result ---
determine_result <- function(row) {
  if(is.na(row$home_score) || is.na(row$away_score)) return(NA)
  is_home <- row$team_name == row$home_team.home_team_name
  player_score <- if(is_home) row$home_score else row$away_score
  opponent_score <- if(is_home) row$away_score else row$home_score
  if(player_score > opponent_score) return("Win")
  if(player_score < opponent_score) return("Loss")
  return("Draw")
}

players_data <- players_data %>%
  rowwise() %>%
  mutate(result = determine_result(cur_data())) %>%
  ungroup()

# --- Compute Big Game & Resilience separately ---
big_game_scores <- players_data %>%
  group_by(player_name, position2) %>%
  arrange(desc(elorating2)) %>%
  slice_head(n = 5) %>%
  summarise(big_game_score = mean(Position_Score, na.rm = TRUE), .groups = "drop")

resilience_scores <- players_data %>%
  group_by(player_name, position2) %>%
  filter(lag(result) == "Loss") %>%
  summarise(resilience_score = mean(Position_Score, na.rm = TRUE), .groups = "drop")

# --- Compute consistency and avg score ---
player_metrics <- players_data %>%
  group_by(player_name, position2) %>%
  summarise(
    avg_score = mean(Position_Score, na.rm = TRUE),
    consistency_score = if(n() > 1) 1 / sd(Position_Score, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  ) %>%
  left_join(big_game_scores, by = c("player_name", "position2")) %>%
  left_join(resilience_scores, by = c("player_name", "position2"))

# --- Percentiles within position ---
player_metrics <- player_metrics %>%
  group_by(position2) %>%
  mutate(
    big_game_percentile    = percent_rank(big_game_score),
    resilience_percentile  = percent_rank(resilience_score),
    consistency_percentile = percent_rank(consistency_score),
    GRIT_raw        = (big_game_percentile + resilience_percentile + consistency_percentile) / 3,
    GRIT_percentile = percent_rank(GRIT_raw)
  ) %>%
  ungroup()

# --- Model graph for all players ---
model_plot <- ggplot(player_metrics, aes(x = GRIT_percentile, y = avg_score)) +
  geom_point(aes(color = position2), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Overall Regression: Avg Position Score vs GRIT Percentile",
    x = "GRIT Percentile Rank",
    y = "Average Position Score"
  ) +
  theme_minimal()

# --- Faceted plot by position with trend lines, confidence, and equation ---
position_plot <- ggplot(player_metrics, aes(x = GRIT_percentile, y = avg_score)) +
  geom_point(aes(color = position2), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
               formula = y ~ x, parse = TRUE, size = 3) +
  facet_wrap(~position2, scales = "free") +
  labs(
    title = "GRIT Percentile vs Average Position Score by Position",
    x = "GRIT Percentile Rank",
    y = "Average Position Score"
  ) +
  theme_minimal()



# --- Output ---
print(position_plot)

