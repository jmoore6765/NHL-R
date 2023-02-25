library(hockeyR)
library(tidyverse)

# Load the play by play data from the 2021:2022 season
pbph = hockeyR::load_pbp(2022)

# Calculate the expected goals per game and the goals over expeted goals per game
xgPerGame = calculate_individual(pbph, type = "R", game_strength = "all") |>
  group_by(player_name) |> 
  filter(goals >= 30) |> 
  mutate(xg_pergame = ixg / gp) |> 
  mutate(gax_pergame = (goals / gp) - (ixg / gp)) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name"))

# Plot expected goals per game on the x axis and goals over expected goals per game on the y axis
xgPerGame |> 
  na.omit() |> 
  ggplot(aes(x = xg_pergame, y = gax_pergame)) +
  geom_point(aes(fill = team_color1, color = team_color2, size = gp), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = player_name)) +
  theme_bw() +
  geom_hline(yintercept = mean(xgPerGame$gax_pergame), linetype = "dashed") +
  geom_vline(xintercept = mean(xgPerGame$xg_pergame), linetype = "dashed") +
  labs(x = "Expected Goals per Game",
       y = "Goals Over Expected Goals per Game",
       title = "Goals Over Expected and xG",
       subtitle = "Minimum of 30 goals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))


