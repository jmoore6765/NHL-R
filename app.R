library(shiny)
library(hockeyR)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)

# Load the play by play data from the 2021:2022 season
pbp_full = hockeyR::load_pbp(2021:2023)

geo = calculate_individual(pbp_full, type = "R", game_strength = "all") |>
  group_by(player_id) |> 
  filter(mean(goals) >= 10) |> 
  left_join(team_logos_colors, by = c("team" = "full_team_name")) |>
  select(player_id, player_name, team_logo_espn, gp, goals, ixg, gax, team,
         team_color1, team_color2) 

# Define UI
ui <- fluidPage(
  
  # Add padding between the plot and the table
  tags$style(type = "text/css", ".output-wrapper { padding: 20px; }"),
  
  div(
    style = "text-align: center;", # Center the content within the div
    titlePanel("Goals Over Expected and Expected Goals"),
    
    # Wrap the plot output in a div and add some bottom margin
    div(
      plotOutput("my_plot"),
      style = "margin-bottom: 20px;"
    ),
    
    # Wrap the table output in a div and add some top margin
    div(
      gt_output("my_table"),
      style = "margin-top: 20px;"
    )
  )
)

server <- function(input, output) {
  
  # Render the scatter plot
  output$my_plot <- renderPlot({
    goe |>
      na.omit() |>
      ggplot(aes(x = ixg, y = gax)) +
      geom_point(aes(fill = team_color1, color = team_color2, size = gp),
                 shape = 21, alpha = 0.9) +
      scale_color_identity(aesthetics = c("fill", "color")) +
      ggrepel::geom_text_repel(aes(label = player_name)) +
      theme_bw() +
      geom_hline(yintercept = mean(goe$gax), linetype = "dashed") +
      geom_vline(xintercept = mean(goe$ixg), linetype = "dashed") +
      labs(x = "Expected Goals",
           y = "Goals Over Expected Goals",
           title = "",
           subtitle = "Minimum 30 goals") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 16, hjust = 0.5))
  })
  
  # Render the table
  output$my_table <- render_gt({
    goe |> 
      cbind(gax_rank = rank(-goe$gax)) |> 
      arrange(gax_rank) |>
      select(gax_rank, player_name, team_logo_espn, gp, goals, ixg, gax) |>
      mutate(ixg = round(ixg, 1), 
             gax = round(gax, 1)) |> 
      gt() |> 
      tab_row_group(label = "", rows = 1:nrow(goe)) |> 
      cols_align(align = "center") |> 
      gt_img_rows(team_logo_espn) |> 
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
  })
}

# Run the app
shinyApp(ui = ui, server = server)
