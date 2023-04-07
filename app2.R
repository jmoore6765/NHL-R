library(shiny)
library(hockeyR)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)

# Define UI
ui <- fluidPage(
  sliderInput(inputId = "date_range", 
              label = "Select date range:",
              min = 2021, max = 2023, value = c(2021, 2023), step = 1, sep = ""),
  plotOutput(outputId = "scatter_plot"),
  gt_output(outputId = "table")
)

# Define server
server <- function(input, output) {
  
  # Filter data based on date range selected by user
  pbp_filtered <- reactive({
    pbp_full |> 
      filter(season >= input$date_range[1] & season <= input$date_range[2])
  })
  
  # Calculate individual statistics
  goe <- reactive({
    calculate_individual(pbp_filtered(), type = "R", game_strength = "all") |> 
      group_by(player_id) |> 
      filter(goals >= (input$date_range[2] - input$date_range[1]) * 10) |> 
      left_join(team_logos_colors, by = c("team" = "full_team_name")) |> 
      select(player_id, player_name, team_logo_espn, gp, goals, ixg, gax, team,
             team_color1, team_color2) 
  })
  
  output$scatter_plot <- renderPlot({
    goe() |>
      na.omit() |>
      ggplot(aes(x = ixg, y = gax)) +
      geom_point(aes(fill = team_color1, color = team_color2, size = gp),
                 shape = 21, alpha = 0.9) +
      scale_color_identity(aesthetics = c("fill", "color")) +
      ggrepel::geom_text_repel(aes(label = player_name)) +
      theme_bw() +
      geom_hline(yintercept = mean(goe()$gax), linetype = "dashed") +
      geom_vline(xintercept = mean(goe()$ixg), linetype = "dashed") +
      labs(x = "Expected Goals",
           y = "Goals Over Expected Goals",
           title = "Goals Over Expected and Expected Goals",
           subtitle = "Minimum 30 goals") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 16, hjust = 0.5))
  })
  
  output$table <- render_gt({
    goe() |>
      cbind(gax_rank = rank(-goe()$gax)) |>
      arrange(gax_rank) |>
      select(gax_rank, player_name, team_logo_espn, gp, goals, ixg, gax) |>
      mutate(ixg = round(ixg, 1),
             gax = round(gax, 1)) |>
      gt() |>
      tab_row_group(label = "", rows = 1:nrow(goe())) |>
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

