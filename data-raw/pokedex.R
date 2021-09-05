## code to prepare `pokedex` dataset goes here

pokedex <- readr::read_csv("data-raw/pokedex.csv") |>
  dplyr::select(-...1) # First column doesnt contain variables


usethis::use_data(pokedex, overwrite = TRUE)
