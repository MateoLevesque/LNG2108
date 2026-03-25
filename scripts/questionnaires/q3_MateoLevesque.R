library(tidyverse)
library(tidytext)
library(stopwords)

# TÂCHE 1

# Répondre basé sur le travail d'équipe.


# TÂCHE 2

# Chargement du fichier RData pour balzac
load("../../exposes/balzac.RData")

# Nettoyage de la colonne `text`
balzac_nett <- balzac |>
  mutate(
    text = text |>
      str_to_lower() |> # conversion des majuscules en minuscules
      str_trim() |> # suppression des espaces et tabulations superflux
      str_replace_all("\\s+", " ") |> # simplification des espaces
      str_remove_all("\\d") |> # suppression des chiffres
      str_replace_all("'", " ") # remplacement des apostrophes par des espaces
  ) |>
  filter(str_detect(text, ".+")) # suppression des rangées vides

# pour visualiser le tibble nettoyé, utilisez la commande suivante
balzac_nett

# Tokénisation de la colonne text en mots
tokens <- balzac_nett |>
  unnest_tokens(mot, text)
# Voici un aperçu du tibble tokenisé
#  gutenberg_id title           author             year mot
#  11049        Eugénie Grandet Balzac, Honoré de  1833 eugénie
#  11049        Eugénie Grandet Balzac, Honoré de  1833 grandet
#  11049        Eugénie Grandet Balzac, Honoré de  1833 scènes
#  11049        Eugénie Grandet Balzac, Honoré de  1833 de
#  11049        Eugénie Grandet Balzac, Honoré de  1833 la
#  11049        Eugénie Grandet Balzac, Honoré de  1833 vie
#  11049        Eugénie Grandet Balzac, Honoré de  1833 de
#  11049        Eugénie Grandet Balzac, Honoré de  1833 province
#  11049        Eugénie Grandet Balzac, Honoré de  1833 par
#  11049        Eugénie Grandet Balzac, Honoré de  1833 honoré

# Détermination des types
types <- tokens |>
  distinct(mot)
#    mot
#  1 eugénie
#  2 grandet
#  3 scènes
#  4 de
#  5 la
#  6 vie
#  7 province
#  8 par
#  9 honoré
# 10 balzac

# Statistiques de base

n_tokens <- nrow(tokens)
# Il y a 4533294 tokens

n_types <- nrow(types)
# Il y a 70804 types

n_oeuvres <- n_distinct(balzac$title)
# Il y a 21 oeuvres

etendue_temps <- balzac |>
  distinct(year) |>
  summarise(
    premiere_annee_publication = min(year),
    derniere_annee_publication = max(year)
  )
# A tibble: 1 × 2
# premiere_annee_publication    derniere_annee_publication
# 1830                          1842

# calcul du TTR total
ttr_total <- n_types / n_tokens
# Le TTR total est de 0.01561866

# calcul du TTR par oeuvres
ttr_par_oeuvres <- inner_join(
  tokens |> count(title, name = "n_tokens"),
  tokens |> distinct(title, mot) |> count(title, name = "n_types"),
  by = "title"
) |>
  mutate(ttr = n_types / n_tokens) |>
  select(title, ttr)

# pour visualiser le TTR par oeuvre en entier, exécutez la ligne suivante
ttr_par_oeuvres |>
  print(n = 21)


# TÂCHE 3

# Ajout des stopwords dans la variable `sw`
sw <- stopwords("fr", source = "stopwords-iso")

# Retrait des mots à faible valeur référentielle dans le corpus tokenisé
sans_sw <- tokens |>
  filter(!mot %in% sw)

# Identification des 20 mots les plus fréquents
sans_sw |>
  count(mot, sort = TRUE) |>
  slice_max(n, n = 20)
#    mot          n
#  1 monsieur 10928
#  2 femme    10265
#  3 madame    9950
#  4 homme     9788
#  5 faire     7282
#  6 vie       6312
#  7 jeune     5272
#  8 fille     5055
#  9 père      4964
# 10 moment    4655
# 11 mère      4609
# 12 amour     4449
# 13 jamais    4340
# 14 francs    4212
# 15 yeux      4188
# 16 voir      4181
# 17 répondit  4130
# 18 temps     4077
# 19 monde     3924
# 20 grand     3922

# TODO: Interprétation 2-3 ph

# TÂCHE 4

# Visualisation du la loi de Zipf
tokens |>
  count(mot, name = "freq", sort = TRUE) |>
  mutate(
    rang = row_number()
  ) |>
  ggplot(aes(x = rang, y = freq)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# TODO: Loi de zipf ?

# TÂCHE 5

# 1) J'ai choisi de cibler les mots finissant par -ième
# voici donc le patron que j'utiliserai.
patron <- "\\b.+ième\\b"

# 2)
freq_relative <- inner_join(
  tokens |>
    count(title, name = "n_tokens"),
  tokens |>
    filter(str_detect(mot, patron)) |>
    count(title, name = "n_occ"),
  by = "title"
) |>
  mutate(
    freq_rel = n_occ / n_tokens
  ) |>
  select(title, freq_rel)
#    title                                                                  freq_rel
#  1 Contes bruns                                                           0.000139
#  2 Eugénie Grandet                                                        0.000149
#  3 La Comédie humaine - Volume 01                                         0.000252
#  4 La Comédie humaine - Volume 02                                         0.000198
#  5 La Comédie humaine - Volume 03                                         0.000147
#  6 La Comédie humaine - Volume 04                                         0.000296
#  7 La Comédie humaine - Volume 05. Scènes de la vie de Province - Tome 01 0.000263
#  8 La Comédie humaine - Volume 06. Scènes de la vie de Province - Tome 02 0.000302
#  9 La Comédie humaine - Volume 07. Scènes de la vie de Province - Tome 03 0.000194
# 10 La Comédie humaine - Volume 08. Scènes de la vie de Province - Tome 04 0.000325

# 3)

# max
freq_relative |>
  arrange(desc(freq_rel)) |>
  slice(1)
#   title                        freq_rel
# 1 La Maison du Chat-qui-pelote 0.000409

# min
freq_relative |>
  arrange(freq_rel) |>
  slice(1)
#   title        freq_rel
# 1 Contes bruns 0.000139

# TÂCHE 6

entropie <- function(freq) {
  p <- freq / sum(freq)
  p <- p[p > 0]
  -sum(p * log2(p))
}

calcul_entropie <- sans_sw |>
  group_by(title) |>
  count(mot) |>
  summarise(
    entropie = entropie(n)
  )

# max
calcul_entropie |>
  arrange(desc(entropie)) |>
  slice(1)
#   title                                                   entropie
#   La Comédie humaine - Volume 16. Études philosophiques …     12.7

# min
calcul_entropie |>
  arrange(entropie) |>
  slice(1)
#   title                        entropie
#   La Maison du Chat-qui-pelote     11.1

# plot

ttr_ent <- inner_join(
  calcul_entropie,
  ttr_par_oeuvres,
  by = "title"
)

ttr_ent |>
  mutate(ttrx100 = ttr * 100) |>
  pivot_longer(
    cols = c(entropie, ttrx100),
    names_to = "var", values_to = "val"
  ) |>
  ggplot(aes(x = title, y = val, fill = var)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = ~ str_trunc(.x, width = 35)) +
  labs(x = "Titres", y = "Valeurs", fill = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
