library(tidyverse)
library(nflfastR)
library(ggplot2)
library(ggrepel)
library(ggimage)
library(gt)
library(ggpubr)
nflreadr::.clear_cache()
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
rushers_18 <- load_pbp(2018) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            team_abbr = last(posteam),
            avg_epa = mean(epa),
            avg_yards = mean(yards_gained),
            median_yards = median(yards_gained),
            mode_yards = mode(yards_gained)) %>%
  mutate(average_yards_over_median = avg_yards - median_yards) %>%
  mutate(median_yards_over_mode = median_yards - mode_yards) %>%
  mutate(average_yards_over_mode = avg_yards - mode_yards) %>%
  filter(rushes > 100 & rushes < 3000)
rushers_18 <- rushers_18 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
rushers_19 <- load_pbp(2019) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            team_abbr = last(posteam),
            avg_epa = mean(epa),
            avg_yards = mean(yards_gained),
            median_yards = median(yards_gained),
            mode_yards = mode(yards_gained)) %>%
  mutate(average_yards_over_median = avg_yards - median_yards) %>%
  mutate(median_yards_over_mode = median_yards - mode_yards) %>%
  mutate(average_yards_over_mode = avg_yards - mode_yards) %>%
  filter(rushes > 100 & rushes < 3000)
rushers_19 <- rushers_19 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
rushers_20 <- load_pbp(2020) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            team_abbr = last(posteam),
            avg_epa = mean(epa),
            avg_yards = mean(yards_gained),
            median_yards = median(yards_gained),
            mode_yards = mode(yards_gained)) %>%
  mutate(average_yards_over_median = avg_yards - median_yards) %>%
  mutate(median_yards_over_mode = median_yards - mode_yards) %>%
  mutate(average_yards_over_mode = avg_yards - mode_yards) %>%
  filter(rushes > 100 & rushes < 3000)
rushers_20 <- rushers_20 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
rushers_21 <- load_pbp(2021) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            team_abbr = last(posteam),
            avg_epa = mean(epa),
            avg_yards = mean(yards_gained),
            median_yards = median(yards_gained),
            mode_yards = mode(yards_gained)) %>%
  mutate(average_yards_over_median = avg_yards - median_yards) %>%
  mutate(median_yards_over_mode = median_yards - mode_yards) %>%
  mutate(average_yards_over_mode = avg_yards - mode_yards) %>%
  filter(rushes > 100 & rushes < 3000)
rushers_21 <- rushers_21 %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
rushers <- load_pbp(2018:2021) %>%
  group_by(rusher_player_name) %>%
  filter(!is.na(epa)) %>%
  summarise(rushes = n(),
            team_abbr = last(posteam),
            avg_epa = mean(epa),
            avg_yards = mean(yards_gained),
            median_yards = median(yards_gained),
            mode_yards = mode(yards_gained)) %>%
  mutate(average_yards_over_median = avg_yards - median_yards) %>%
  mutate(median_yards_over_mode = median_yards - mode_yards) %>%
  mutate(average_yards_over_mode = avg_yards - mode_yards) %>%
  filter(rushes > 500 & rushes < 15000)
rushers <- rushers %>%
  left_join(teams_colors_logos, by = c("team_abbr" = "team_abbr"))
rushers_18 %>%
  ggplot(aes(x= avg_yards, y= median_yards, label = rusher_player_name)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_text(hjust = 0, vjust = 0) +
  labs(x = "Yards/Attempt",
       y = "Median Yards/Attempt",
       title = "2018 Average and Median Yards") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
rushers_18 %>%
  ggplot(aes(x= rushes , y= average_yards_over_median, label = rusher_player_name)) +
  geom_text(hjust = 0, vjust = 0) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(rushers_18$average_yards_over_median)) +
  geom_vline(xintercept = mean(rushers_18$rushes)) +
  labs(x = "Rushing Attempts",
       y = "Avg. Yards over Median",
       title = "2018 Rushers Yards/Attempt vs Median Yards",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) +
  annotate("text", x = 275, y = 2.6, alpha = .5, label = "Explosive Rushers") +
  annotate("text", x = 275, y = 0.4, alpha = .5, label = "non-Explosive Rushers")
rushers_19 %>%
  ggplot(aes(x= avg_yards, y= median_yards, label = rusher_player_name)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_text(hjust = 0, vjust = 0) +
  labs(x = "Yards/Attempt",
       y = "Median Yards/Attempt",
       title = "2019 Average and Median Yards") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
rushers_19 %>%
  ggplot(aes(x= rushes , y= average_yards_over_median, label = rusher_player_name)) +
  geom_text(hjust = 0.55, vjust = -.75) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(rushers_19$average_yards_over_median)) +
  geom_vline(xintercept = mean(rushers_19$rushes)) +
  labs(x = "Rushing Attempts",
       y = "Avg. Yards over Median",
       title = "2019 Rushers Yards/Attempt vs Median Yards",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) +
  annotate("text", x = 300, y = 2.5, alpha = 0.5, label = "Explosive Rushers") +
  annotate("text", x = 350, y= .25, alpha = 0.5, label = "non-Explosive Rushers")
rushers_20 %>%
  ggplot(aes(x= avg_yards, y= median_yards, label = rusher_player_name)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_text(hjust = 0, vjust = 0) +
  labs(x = "Yards/Attempt",
       y = "Median Yards/Attempt",
       title = "2020 Average and Median Yards") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
rushers_20 %>%
  ggplot(aes(x= rushes , y= average_yards_over_median, label = rusher_player_name)) +
  geom_text(hjust = 0, vjust = 0) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(rushers_20$average_yards_over_median)) +
  geom_vline(xintercept = mean(rushers_20$rushes)) +
  labs(x = "Rushing Attempts",
       y = "Avg. Yards over Median",
       title = "2020 Rushers Yards/Attempt vs Median Yards",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) +
  annotate("text", x = 300, y = 2.25, alpha = 0.5, label = "Explosive Rushers") +
  annotate("text", x = 300, y = 0.5, alpha = 0.5, label = "non-Explosive Rushers")
rushers_21 %>%
  ggplot(aes(x= avg_yards, y= median_yards, label = rusher_player_name)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_text(hjust = 0, vjust = 0) +
  labs(x = "Yards/Attempt",
       y = "Median Yards/Attempt",
       title = "2021 Average and Median Yards") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
rushers_21 %>%
  ggplot(aes(x= rushes , y= average_yards_over_median, label = rusher_player_name)) +
  geom_text(hjust = 0, vjust = 0) +
  geom_image(aes(image = team_logo_espn)) +
  geom_hline(yintercept = mean(rushers_21$average_yards_over_median)) +
  geom_vline(xintercept = mean(rushers_21$rushes)) +
  labs(x = "Rushing Attempts",
       y = "Avg. Yards over Median",
       title = "2021 Rushers Yards/Attempt vs Median Yards",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) +
  annotate("text", x = 300, y = 3, alpha = 0.5, label = "Explosive Rushers") +
  annotate("text", x = 300, y = .5, alpha = 0.5, label = "non-Explosive Rushers")
rushers %>%
  ggplot(aes(x= avg_yards, y= median_yards, label = rusher_player_name)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_text(hjust = 0, vjust = 0) +
  labs(x = "Yards/Attempt",
       y = "Median Yards/Attempt",
       title = "2018-2021 Average and Median Yards") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5))
rushers %>%
  ggplot(aes(x = rushes, y = average_yards_over_median, label = rusher_player_name)) +
  geom_image(aes(image = team_logo_espn)) +
  geom_text(hjust = 0.5, vjust = -1) +
  geom_hline(yintercept = mean(rushers$average_yards_over_median)) +
  geom_vline(xintercept = mean(rushers$rushes)) +
  labs(x = "Rushing Attempts",
       y = "Avg. Yards over Median",
       title = "2018-2021 Rushers Yards/Attempt vs Median Yards",
       caption = "Jarrett Markman | Sports Analytics") +
  theme(plot.title = element_text(size = 12, face = 'bold', hjust = 0.5)) +
  annotate("text", x = 1000, y = 2, alpha = 0.5, label = "Explosive Rushers") +
  annotate("text", x = 1000, y = 0.75, alpha = 0.5, label = "non-Explosive Rushers")
# because the median is 3 for every rusher besides lamar jackson, over time yards/carry will dictate where players appears for avg. yards over median