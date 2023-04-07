library(hockeyR)
library(tidyverse)
library(gt)
library(gtExtras)

# Load the play by play data from the 2021:2022 season
pbph = hockeyR::load_pbp(2022)

# Looking into total expected goals and total goals over expected goals in players with over 30 goals
goals_over_expected = calculate_individual(pbph, type = "R", game_strength = "all") |>
  group_by(player_id) |> 
  filter(goals >= 30) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name"))

# Plot expected goals per game on the x axis and goals over expected goals per game on the y axis
goals_over_expected |> 
  na.omit() |> 
  ggplot(aes(x = ixg, y = gax)) +
  geom_point(aes(fill = team_color1, color = team_color2, size = gp), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = player_name)) +
  theme_bw() +
  geom_hline(yintercept = mean(goals_over_expected$gax), linetype = "dashed") +
  geom_vline(xintercept = mean(goals_over_expected$ixg), linetype = "dashed") +
  labs(x = "Expected Goals",
       y = "Goals Over Expected Goals",
       title = "Goals Over Expected and Expected Goals",
       subtitle = "Minimum 30 goals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))


# Creating a table with players ranked based on their goals over expected
goals_over_expected = cbind(goals_over_expected, gax_rank = rank(-goals_over_expected$gax))

goals_over_expected |> 
  arrange(gax_rank) |>
  select(gax_rank, player_name, team_logo_espn, gp, goals, ixg, gax) |>
  mutate(ixg = round(ixg, 1), 
         gax = round(gax, 1)) |> 
  gt() |> 
  tab_row_group(label = "", rows = 1:nrow(goals_over_expected)) |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_logo_espn) |> 
  cols_label(gax_rank = "Rank",
             player_name = "Name", 
             team_logo_espn = "Team",
             goals = "Goals",
             ixg = md("Expected <br> Goals"),
             gp = md("Games <br> Played"), 
             gax = md("Goals Over <br> Expected")) |>
  gt_theme_espn() |> 
  gt_color_rows(gax, 
                palette = c("#E03232", "orange", "#117124"), 
                pal_type = "continuous")


# Standardize the data based on games played and look into the "per game" metrics
# Calculate the expected goals per game and the goals over expeted goals per game
goals_over_expected_pg = calculate_individual(pbph, type = "R", game_strength = "all") |>
  group_by(player_id) |> 
  filter(goals >= 30) |> 
  mutate(xg_pg = ixg / gp) |> 
  mutate(gax_pg = (goals / gp) - (ixg / gp)) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name"))

# Plot expected goals per game on the x axis and goals over expected goals per game on the y axis
goals_over_expected_pg |> 
  na.omit() |> 
  ggplot(aes(x = xg_pg, y = gax_pg)) +
  geom_point(aes(fill = team_color1, color = team_color2, size = gp), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = player_name)) +
  theme_bw() +
  geom_hline(yintercept = mean(goals_over_expected_pg$gax_pg), linetype = "dashed") +
  geom_vline(xintercept = mean(goals_over_expected_pg$xg_pg), linetype = "dashed") +
  labs(x = "Expected Goals per Game",
       y = "Goals Over Expected Goals per Game",
       title = "Goals Over Expected and Expected Goals",
       subtitle = "Minimum of 30 goals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

# Creating a table with players ranked based on their goals over expected per game
goals_over_expected_pg = cbind(goals_over_expected_pg, gax_rank_pg = rank(-goals_over_expected_pg$gax_pg))


goals_over_expected_pg |> 
  arrange(gax_rank_pg) |>
  select(gax_rank_pg, player_name, team_logo_espn, gp, xg_pg, gax_pg) |> 
  mutate(xg_pg = round(xg_pg, 2),
         gax_pg = round(gax_pg, 2)) |>
  gt() |> 
  tab_row_group(group = "", rows = 1:nrow(goals_over_expected_pg)) |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_logo_espn) |> 
  cols_label(gax_rank_pg = "Rank",
             player_name = "Name", 
             team_logo_espn = "Team",
             xg_pg = "XG",
             gax_pg = "GAX",
             gp = md("Games <br> Played")) |>
  gt_theme_espn() |> 
  gt_color_rows(gax_pg, 
                palette = c("#E03232", "orange", "#117124"), 
                pal_type = "continuous")

# Load the play by play data from the 2023 season
pbph_2 = hockeyR::load_pbp(2023)

# Looking into total expected goals and total goals over expected goals in players with over 30 goals
goals_over_expected = calculate_individual(pbph_2, type = "R", game_strength = "all") |>
  group_by(player_id) |> 
  filter(goals >= 30) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name"))

# Plot expected goals per game on the x axis and goals over expected goals per game on the y axis
goals_over_expected |> 
  na.omit() |> 
  ggplot(aes(x = ixg, y = gax)) +
  geom_point(aes(fill = team_color1, color = team_color2, size = gp), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = player_name)) +
  theme_bw() +
  geom_hline(yintercept = mean(goals_over_expected$gax), linetype = "dashed") +
  geom_vline(xintercept = mean(goals_over_expected$ixg), linetype = "dashed") +
  labs(x = "Expected Goals",
       y = "Goals Over Expected Goals",
       title = "Goals Over Expected and Expected Goals",
       subtitle = "Minimum of 30 goals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))


# to do 
# Fit a model using 2018 to 2022 data, goals as dependent variable 

# Plot of number of goals over expected on the y axis and number of goals on the x axis
# This data will be from 2018 to 2022

# Have a table with the expect goals for 2023 



# Creating a table with players ranked based on their goals over expected
goals_over_expected = cbind(goals_over_expected, gax_rank = rank(-goals_over_expected$gax))

goals_over_expected |> 
  arrange(gax_rank) |>
  select(gax_rank, player_name, team_logo_espn, gp, goals, ixg, gax) |>
  mutate(ixg = round(ixg, 1), 
         gax = round(gax, 1)) |> 
  gt() |> 
  tab_row_group(group = "", rows = 1:nrow(goals_over_expected)) |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_logo_espn) |> 
  cols_label(gax_rank = "Rank",
             player_name = "Name", 
             team_logo_espn = "Team",
             goals = "Goals",
             ixg = md("Expected <br> Goals"),
             gp = md("Games <br> Played"), 
             gax = md("Goals Over <br> Expected")) |>
  gt_theme_espn() |> 
  gt_color_rows(gax, 
                palette = c("#E03232", "orange", "#117124"), 
                pal_type = "continuous")


# Standardize the data based on games played and look into the "per game" metrics
# Calculate the expected goals per game and the goals over expeted goals per game
goals_over_expected_pg = calculate_individual(pbph_2, type = "R", game_strength = "all") |>
  group_by(player_id) |> 
  filter(goals >= 30) |> 
  mutate(xg_pg = ixg / gp) |> 
  mutate(gax_pg = (goals / gp) - (ixg / gp)) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name"))

# Plot expected goals per game on the x axis and goals over expected goals per game on the y axis
goals_over_expected_pg |> 
  na.omit() |> 
  ggplot(aes(x = xg_pg, y = gax_pg)) +
  geom_point(aes(fill = team_color1, color = team_color2, size = gp), 
             shape = 21, alpha = 0.9) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  ggrepel::geom_text_repel(aes(label = player_name)) +
  theme_bw() +
  geom_hline(yintercept = mean(goals_over_expected_pg$gax_pg), linetype = "dashed") +
  geom_vline(xintercept = mean(goals_over_expected_pg$xg_pg), linetype = "dashed") +
  labs(x = "Expected Goals per Game",
       y = "Goals Over Expected Goals per Game",
       title = "Goals Over Expected and Expected Goals",
       subtitle = "Minimum of 30 goals") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 16, hjust = 0.5))

# Creating a table with players ranked based on their goals over expected per game
goals_over_expected_pg = cbind(goals_over_expected_pg, gax_rank_pg = rank(-goals_over_expected_pg$gax_pg))


goals_over_expected_pg |> 
  arrange(gax_rank_pg) |>
  select(gax_rank_pg, player_name, team_logo_espn, gp, xg_pg, gax_pg) |> 
  mutate(xg_pg = round(xg_pg, 2),
         gax_pg = round(gax_pg, 2)) |>
  gt() |> 
  tab_row_group(group = "", rows = 1:nrow(goals_over_expected_pg)) |> 
  cols_align(align = "center") |> 
  gtExtras::gt_img_rows(team_logo_espn) |> 
  cols_label(gax_rank_pg = "Rank",
             player_name = "Name", 
             team_logo_espn = "Team",
             xg_pg = "XG",
             gax_pg = "GAX",
             gp = md("Games <br> Played")) |>
  gt_theme_espn() |> 
  gt_color_rows(gax_pg, 
                palette = c("#E03232", "orange", "#117124"), 
                pal_type = "continuous")








