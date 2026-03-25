library(tidyverse)

load("../../donnees/proust.RData")

pt <- tibble(
  text = str_replace_all(proust, "'|’", " "),
  partie = cumsum(str_detect(proust, "^(PREMIÈRE|DEUXIÈME|TROISIÈME)"))
)
pt

pt_net <- pt |>
  filter(text != "")
pt_net
