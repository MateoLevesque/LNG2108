library(tidyverse)
library(tidytext)
library(stopwords)

sw <- stopwords("fr", source = "stopwords-iso")

load("../donnees/proust.RData")

proust_tib <- tibble(texte = proust)

proust_tib <- proust_tib |>
  mutate(
    texte = str_replace_all(texte, "'|’", " ")
  )

proust_tib

tokens <- proust_tib |>
  unnest_tokens(mot, texte)

# Affichage des 20 mots les plus fréquent
tokens |>
  filter(!mot %in% sw) |>
  count(mot) |>
  slice_max(n, n = 20) |>
  ggplot(aes(x = reorder(mot, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(x = "tokens", y = "Fréquence", title = "Top 20 mots de contenu - Proust")

## Exercice Regex

prdf <- tibble(texte = proust)
