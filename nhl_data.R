library(hockeyR)
library(tidyverse)

pbp_full = hockeyR::load_pbp(2021:2023)

pbp_full = pbp_full |> 
  filter(!is.na(xg))

write.csv(pbp_full, file = "pbp_full.csv", row.names = FALSE)
