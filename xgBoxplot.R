# library(hockeyR)
# library(tidyverse)
library(gt)
library(gtExtras)
library(ggbeeswarm)

# library(ggthemes)
library(ggimage)
library(ggrepel)
# library(ggbeeswarm)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 16),
      axis.title.x = element_text(size=18),
      axis.title.y = element_text(size=18),
      axis.text = element_text(size = 14),
      strip.text = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

pbp22 = hockeyR::load_pbp(2022)

topPlayers = pbp22 |> 
  filter(!is.na(xg), event_team_abbr == "TOR") |> 
  group_by(event_player_1_name) |> 
  summarise(totalXG = sum(xg)) |> 
  arrange(desc(totalXG)) |> 
  head(3)

topPlayerNames = topPlayers$event_player_1_name

pbp22 |> 
  filter(xg >= 0.01) |> 
  filter(xg <= 0.5) |> 
  filter(!is.na(xg), event_player_1_name %in% topPlayerNames) |> 
  ggplot(aes(x = event_player_1_name, y = xg, fill = xg)) +
  geom_quasirandom(pch = 21, size = 4) +
  scale_fill_viridis_c() +
  theme_reach() +
  geom_hline(yintercept = 0, color = "black", alpha=1.0) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(x = "Player Name", y = "XG", title = "Goal Probability of Top Three Toronto Player Shots")
  
  
  
