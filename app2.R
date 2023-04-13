library(shiny)
library(hockeyR)
library(tidyverse)
library(gt)
library(gtExtras)
library(ggplot2)

pbpFull = hockeyR::load_pbp(2022:2023)

ui = fluidPage(
  
  titlePanel("Goals Over Expected"),
  
  mainPanel(
    navbarPage(" ", 
               tabPanel("Tab 1",
                        sliderInput(inputId = "dateRange", label = "Date Range", min = 2022, max = 2023, value = c(2022, 2023), step = 1, sep = ""),
                        mainPanel(
                          plotOutput(outputId = "scatterPlotGOE", 
                                     width = "1000px", 
                                     height = "500px"),
                          gt_output(outputId = "gtTableGOE")
                          ),
                        ),
               tabPanel("Tab 2",
                        sliderInput(inputId = "dateRange", label = "Date Range", min = 2022, max = 2023, value = c(2022, 2023), step = 1, sep = ""),
                        mainPanel(
                          ),
                        )
               )
    )
)

server = function(input, output) {
  
  pbp_filtered = reactive({
    pbpFull |> 
      filter(season >= input$dateRange[1] & season <= input$dateRange[2])
    
    })
  
  goe = reactive({
    
    calculate_individual(pbp_filtered(), type = "R", game_strength = "all") |> 
      group_by(player_id) |> 
      filter(goals >= (input$dateRange[2] - input$dateRange[1]) * 30) |> 
      left_join(team_logos_colors, by = c("team" = "full_team_name")) |> 
      select(player_id, player_name, team_logo_espn, gp, goals, ixg, gax, team, team_color1, team_color2) 
    
    })
  
  output$scatterPlotGOE = renderPlot({
    
    goe() |>
      na.omit() |>
      ggplot(aes(x = ixg, y = gax)) +
      geom_point(aes(fill = team_color1, color = team_color2, size = goals), shape = 21, alpha = 0.9) +
      scale_color_identity(aesthetics = c("fill", "color")) +
      ggrepel::geom_text_repel(aes(label = player_name)) +
      theme_bw() +
      geom_hline(yintercept = mean(goe()$gax), linetype = "dashed") +
      geom_vline(xintercept = mean(goe()$ixg), linetype = "dashed") +
      labs(x = "Expected Goals",
           y = "Goals Over Expected Goals",
           title = "Goals Over Expected and Expected Goals",
           subtitle = "Minimum 30 goal per season average") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
      theme(plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(size = 16, hjust = 0.5))
    
    })
  
  output$gtTableGOE = render_gt({
    
    goe() |>
      cbind(gax_rank = rank(-goe()$gax)) |>
      arrange(gax_rank) |>
      select(gax_rank, player_name, team_logo_espn, gp, goals, ixg, gax) |>
      mutate(ixg = round(ixg, 1), gax = round(gax, 1)) |>
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
      gt_color_rows(gax, palette = c("#E03232", "orange", "#117124"), pal_type = "continuous")
  }, width = 800)
  
  }

shinyApp(ui = ui, server = server)

