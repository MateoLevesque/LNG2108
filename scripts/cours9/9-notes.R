library(tidyverse)
library(tidytext)

load("../donnees/monte_cristo.RData")

mc_net <- mc |>
  str_subset(".+") |>
  str_to_lower() |>
  str_trim() |>
  str_replace_all("\\s+", " ") |>
  str_remove_all("\\d+|_")

mc_tibble <- tibble(
  texte = mc_net
)

mc_net |>
  str_extract_all("\\b\\w+tion\\b")

mc_tok <- mc_tibble |>
  unnest_tokens(mot, texte)

freq <- mc_tok |>
  count(mot, sort = TRUE)

ent <- function(freq) {
  p <- freq / sum(freq)
  p <- p[p > 0]
  -sum(p * log2(p))
}

ent(freq$n)
