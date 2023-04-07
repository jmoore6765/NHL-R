library(hockeyR)
library(tidyverse)

pbp_full = hockeyR::load_pbp(2023:2023)

goe_filtered = calculate_individual(pbp_full, type = "R", game_strength = "all") |>
  group_by(player_id) |> 
  filter(mean(goals) >= 10) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name")) |>
  select(player_id, player_name, team_logo_espn, gp, goals, ixg, gax, team,
         team_color1, team_color2) 

write.csv(goe_filtered, file = "goe_filtered.csv", row.names = FALSE)
