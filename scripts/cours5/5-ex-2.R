# Exercice 2 cours 5

library(tidyverse)

d <- read_table("../../donnees/pratique_2.txt")

data <- tibble(mot = d)

data

data |>
  count(mot) |>
  mutate(
    proportion = n / sum(n),
    frq_million = proportion * 1000000
  ) |>
  arrange(desc(n))
