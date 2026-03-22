library(tidyverse)
library(tidytext)

# On doit importer le fichier RData pour balzac
load("../../exposes/balzac.RData")

# Nettoyage de la colonne text
balzac_nett <- balzac |>
  mutate(
    text = text |>
      str_to_lower() |>
      str_trim() |>
      str_replace_all("\\s+", " ") |>
      str_remove_all("\\d") |>
      str_replace_all("'", " ")
  ) |>
  filter(str_detect(text, ".+"))

balzac_nett

tokens <- balzac_nett |>
  unnest_tokens(mot, texte_nett)

types <- tokens |>
  distinct(mot)

# TÂCHE 2

n_tokens <- nrow(tokens)
# [1] 4533294
n_types <- nrow(types)
# [1] 70804
n_oeuvres <- n_distinct(balzac$title)
# [1] 21

etendue_temps <- balzac |> distinct(year)
# # A tibble: 4 × 1
#    year
#   <dbl>
# 1  1833
# 2  1832
# 3  1830
# 4  1842

ttr_total <- n_types / n_tokens
# [1] 0.01561866

ttr_par_oeuvres <- inner_join(
  tokens |> count(title, name = "n_tokens"),
  tokens |> distinct(title, mot) |> count(title, name = "n_types"),
  by = "title"
) |>
  mutate(ttr = n_types / n_tokens) |>
  select(title, ttr)
#    title                                                                     ttr
#    <chr>                                                                   <dbl>
#  1 Contes bruns                                                           0.0485
#  2 Eugénie Grandet                                                        0.128
#  3 La Comédie humaine - Volume 01                                         0.0811
#  4 La Comédie humaine - Volume 02                                         0.0837
#  5 La Comédie humaine - Volume 03                                         0.0806
#  6 La Comédie humaine - Volume 04                                         0.0767
#  7 La Comédie humaine - Volume 05. Scènes de la vie de Province - Tome 01 0.0803
#  8 La Comédie humaine - Volume 06. Scènes de la vie de Province - Tome 02 0.0808
#  9 La Comédie humaine - Volume 07. Scènes de la vie de Province - Tome 03 0.0870
# 10 La Comédie humaine - Volume 08. Scènes de la vie de Province - Tome 04 0.0771

# TÂCHE 3
