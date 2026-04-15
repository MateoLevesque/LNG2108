# Importation des librairies
library(tidyverse)
library(tidytext)
library(stopwords)
library(udpipe)

# Chargement du fichier RData pour Balzac
load("../../exposes/balzac.RData")

# Ajout des stopwords dans la variable `sw`
sw <- stopwords("fr", source = "stopwords-iso")

# Déclaration de la fonction `entropie` servant à calculer l'entropie
entropie <- function(freq) {
  p <- freq / sum(freq)
  p <- p[p > 0]
  -sum(p * log2(p))
}

# # Model udpipe
# udpipe_download_model(language = "french")
# modele <- udpipe_load_model("french-gsd-ud-2.5-191206.udpipe")
#
# annotation <- udpipe_annotate(modele, balzac$text)
# annotation |> as.data.frame() |>
#   as_tibble() |>
#   select(sentence, tokens, lemma, upos)
# annotation

# Tokenisation de la colonne text en mots
tokens <- balzac |>
  mutate(
    text = text |> str_replace_all("'", " ")
  ) |>
  unnest_tokens(mot, text)

# Détermination des types
types <- tokens |>
  distinct(mot)

# Statistiques de base
n_tokens <- nrow(tokens)
# [1] 4537416

n_types <- nrow(types)
# [1] 71623

n_oeuvres <- n_distinct(balzac$title)
# [1] 21

etendue_temps <- balzac |>
  distinct(year) |>
  summarise(
    premiere_annee_publication = min(year),
    derniere_annee_publication = max(year)
  )
#   premiere_annee_publication derniere_annee_publication
# 1                       1830                       1842

balzac |>
  group_by(title) |>
  arrange(year)


# Calcul du TTR total
ttr_total <- n_types / n_tokens
# [1] 0.01578498

# Calcul du TTR par œuvres
ttr_par_oeuvres <- inner_join(
  tokens |> count(title, name = "n_tokens"),
  tokens |> distinct(title, mot) |> count(title, name = "n_types"),
  by = "title"
) |>
  mutate(ttr = n_types / n_tokens) |>
  select(title, ttr)
# # A tibble: 21 × 2
#    title                                                                     ttr
#    <chr>                                                                   <dbl>
#  1 Contes bruns                                                           0.0486
#  2 Eugénie Grandet                                                        0.129
#  3 La Comédie humaine - Volume 01                                         0.0817
#  4 La Comédie humaine - Volume 02                                         0.0841
#  5 La Comédie humaine - Volume 03                                         0.0810
#  6 La Comédie humaine - Volume 04                                         0.0771
#  7 La Comédie humaine - Volume 05. Scènes de la vie de Province - Tome 01 0.0807
#  8 La Comédie humaine - Volume 06. Scènes de la vie de Province - Tome 02 0.0815
#  9 La Comédie humaine - Volume 07. Scènes de la vie de Province - Tome 03 0.0875
# 10 La Comédie humaine - Volume 08. Scènes de la vie de Province - Tome 04 0.0778

# Retrait des mots à faible valeur référentielle dans le corpus tokenisé
sans_sw <- tokens |>
  filter(!mot %in% sw)

sans_sw

# Identification des 20 mots les plus fréquents
sans_sw |>
  count(mot, sort = TRUE) |>
  slice_max(n, n = 10)


# Calcul de l'entropie par œuvre
calcul_entropie <- sans_sw |>
  arrange(year) |>
  group_by(title) |>
  count(mot) |>
  summarise(
    entropie = entropie(n)
  )

calcul_entropie

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

# Préparation du tableau
ttr_ent <- inner_join(
  calcul_entropie,
  ttr_par_oeuvres,
  by = "title"
)

# Construction du graphique
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


# count word length

sans_sw |>
  mutate(length = str_count(mot)) |>
  arrange(desc(length)) |>
  select(mot, length) |>
  filter(length < 20) |>
  filter(!str_detect(mot, "_")) |>
  distinct(mot)


# Sortir categorie avec udpipe
#  visualiser les donnees

# calcul ttr sans les adj et sans les adverbe
# visualiser les donnes

# analyse par oeuvre selon les resultats

# ttr entropie sorted by date

#  meme chose mais pour entropie
