# Importation des librairies
library(tidyverse)
library(tidytext)
library(stopwords)

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
# # A tibble: 1 × 2
#   premiere_annee_publication derniere_annee_publication
#                        <dbl>                      <dbl>
# 1                       1830                       1842

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

# Identification des 20 mots les plus fréquents
sans_sw |>
  count(mot, sort = TRUE) |>
  slice_max(n, n = 20)


# Calcul de l'entropie par œuvre
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

# L'œuvre avec l'entropie la plus élevée
# est "La Comédie humaine - Volume 16. Études philosophiques …"
# avec 12,7 et la plus basse est "La Maison du Chat-qui-pelote"
# avec 11,1.

# Représentation graphique de la comparaison entre l'entropie et le TTR.

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
  )
ggplot(aes(x = title, y = val, fill = var)) +
  geom_col(position = "dodge") +
  scale_x_discrete(labels = ~ str_trunc(.x, width = 35)) +
  labs(x = "Titres", y = "Valeurs", fill = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Les deux mesures ne semblent pas suivre une tendance similaire.
# On peut facilement voir que l'œuvre avec le TTR (TTR * 100) le plus
# élevé est aussi l'œuvre avec l'entropie la plus faible.
# Donc, on ne peut pas conclure que les deux valeurs vont dans le même sens.
