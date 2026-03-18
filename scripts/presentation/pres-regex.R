#### SETUP ####
library(tidyverse)

mots <- "Linguistique de corpus."

# Standardisation et nettoyage
mots <- mots |>
  str_to_lower() |>
  str_replace_all("[:punct:]", "")
mots

# k
mots <- mots |>
  str_replace_all("que|c", "k")
mots

# ɔ
mots <- mots |>
  str_replace_all("or", "ɔr")
mots

# ɥi
mots <- mots |>
  str_replace_all("ui", "ɥi")
mots

# u devient y
mots <- mots |>
  str_replace_all("u", "y")
mots

# ɛ̃
mots <- mots |>
  str_replace_all("in", "ɛ̃")
mots

# ʁ
mots <- mots |>
  str_replace_all("r", "ʁ")
mots

# ə
mots <- mots |>
  str_replace_all("e", "ə")
mots
