devtools::install_github("danmorse314/hockeyR")

library(hockeyR)
library(dplyr)
library(tidyverse)
library(gt)
nhl_stats <- get_skater_stats_hr(2023)
unique_cols <- unique(nhl_stats$position)

nhl_stats <- nhl_stats %>%
  filter(position != "D") %>%
  group_by(position)

stats <- nhl_stats[,6:28]

stats = subset(stats, select = -c(1,2,3,6,18,22))

stats <- stats %>%
  mutate(shooting_percent = ifelse(shooting_percent == "NaN", 0.00000000, shooting_percent)) %>%
  mutate(faceoff_win_percent = ifelse(faceoff_win_percent == "NaN", 0.00000000, faceoff_win_percent))

pca <- prcomp(stats, scale = TRUE)
weights <- pca$rotation[,1]
weighted_avg <- rowSums(stats * weights)
nhl_stats$metric <- weighted_avg

metrics <- nhl_stats %>%
  select(player, team_abbr, position, metric)

metrics_c <- metrics %>% filter(position == "C" | position == "F")
metrics_lw <- metrics %>% filter(position == "LW" | position == "F" | position == "W")
metrics_rw <- metrics %>% filter(position == "RW" | position == "F" | position == "W")

metrics_c <- metrics_c %>%
  mutate(position = "C")

metrics_lw <- metrics_lw %>%
  mutate(position = "LW")

metrics_rw <- metrics_rw %>%
  mutate(position = "RW")

metrics_c[,4] <- as.data.frame(apply(metrics_c[,4], 2, function(x) rank(x) / length(x) * 100))
metrics_lw[,4] <- as.data.frame(apply(metrics_lw[,4], 2, function(x) rank(x) / length(x) * 100))
metrics_rw[,4] <- as.data.frame(apply(metrics_rw[,4], 2, function(x) rank(x) / length(x) * 100))

metrics <- rbind(metrics_c, metrics_lw, metrics_rw)

metrics <- metrics %>%
 group_by(player, team_abbr) %>%
 filter(metric == max(metric))

max_players <- metrics %>%
  group_by(team_abbr, position) %>%
  filter(metric == max(metric))

max_teams <- max_players %>%
  group_by(team_abbr) %>%
  summarize(avg_metric = mean(metric)) %>%
  arrange(-avg_metric)

final <- max_players %>%
  semi_join(max_teams, by = "team_abbr") %>%
  pivot_wider(
    id_cols = team_abbr,
    names_from = position,
    values_from = c(player, metric),
  )

final <- inner_join(final, max_teams, by = "team_abbr")

teams <- get_team_records()

final <- inner_join(final, teams, by = "team_abbr")

final <- final %>%
  arrange(-avg_metric) %>%
  select(team_name, avg_metric, player_C, metric_C, player_LW, metric_LW, player_RW, metric_RW)

final = subset(final, select = -1)

table <- final %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(team_name, avg_metric, player_C, metric_C, player_LW, metric_LW, player_RW, metric_RW)
  ) %>%
  data_color(
    columns = c(avg_metric, metric_C, metric_LW, metric_RW),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    team_name = md("**Team**"),
    avg_metric = md("**Average Percentile**"),
    player_C = md("**C Player**"),
    metric_C = md("**C Percentile**"),
    player_LW = md("**LW Player**"),
    metric_LW = md("**LW Percentile**"),
    player_RW = md("**RW Player**"),
    metric_RW = md("**RW Percentile**")
  ) %>%
  tab_header(
    title = md("**Best Forward Lines**"),
    subtitle = "Based on 2022-23 Season With Combination of Metrics With Principal Component Analysis"
  )
gtsave(table, "table.png")