# Exercices for class 5

library(tidyverse)

data <- read_csv("../donnees/pratique_1.csv")

data <- data |>
  mutate(longueur = str_length(token))

data <- data |>
  filter(pos != "DET")

data |> count(pos)

data |>
  summarize(moy = mean(longueur))

out <- data |>
  summarize(long_moy = mean(longueur), .by = pos) |>
  arrange(desc(long_moy))

data
out

write_csv(out, "../donnees/output/5-exercice-1.csv")
