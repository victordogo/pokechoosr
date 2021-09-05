## code to prepare `pokedex_min` dataset goes here

library(dplyr)

col_select <- c("pokedex_number", "name", "status",
                "type_1", "type_2", "hp", "attack",
                "defense", "sp_attack", "sp_defense",
                "speed", "against_normal", "against_fire", "against_water",
                "against_electric", "against_grass", "against_ice",
                "against_fight", "against_poison", "against_ground",
                "against_flying", "against_psychic", "against_bug",
                "against_rock", "against_ghost", "against_dragon",
                "against_dark", "against_steel", "against_fairy")

pokedex_min <- readr::read_csv("data-raw/pokedex.csv") |>
  select(-...1) |> # First column doesnt contain variables
  select(tidyselect::all_of(col_select)) |>
  rowwise() |>
  mutate(sum_against = sum(c_across(against_normal:against_fairy)),.keep="unused")


usethis::use_data(pokedex_min, overwrite = TRUE, internal=TRUE)
