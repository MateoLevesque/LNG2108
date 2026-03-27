# Importation des librairies
library(tidyverse)
library(tidytext)
library(stopwords)


# TÂCHE 1

# Pour l'exposé oral, nous avons décidé de séparer notre équipe
# en 2 sous-équipes. Je travaillerai avec Benjamin Floirat pour
# analyser le sujet numéro 3. Je me chargerai principalement
# d'élaborer et d'expliquer le code que nous aurons rédigé
# pour analyser et répondre au sujet. J'apporterai également
# mon soutien pour l'interprétation des résultats.


# TÂCHE 2

# Chargement du fichier RData pour Balzac
load("../../exposes/balzac.RData")

# Tokenisation de la colonne text en mots
tokens <- balzac |>
  mutate(
    text = text |> str_replace_all("'", " ")
  ) |>
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
# Il y a 4 537 416 tokens

n_types <- nrow(types)
# Il y a 71 623 types

n_oeuvres <- n_distinct(balzac$title)
# Il y a 21 œuvres

etendue_temps <- balzac |>
  distinct(year) |>
  summarise(
    premiere_annee_publication = min(year),
    derniere_annee_publication = max(year)
  )
# premiere_annee_publication    derniere_annee_publication
# 1830                          1842

# Calcul du TTR total
ttr_total <- n_types / n_tokens
# Le TTR total est de 0,01578498

# Calcul du TTR par œuvres
ttr_par_oeuvres <- inner_join(
  tokens |> count(title, name = "n_tokens"),
  tokens |> distinct(title, mot) |> count(title, name = "n_types"),
  by = "title"
) |>
  mutate(ttr = n_types / n_tokens) |>
  select(title, ttr)

# Pour visualiser le TTR par œuvre en entier, exécutez la ligne suivante
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

# Ces mots nous montrent que l'auteur semble aborder des thèmes
# comme la famille avec des mots comme *jeune, mère, père, fille*
# et les relations avec des mots comme *monsieur, madame, femme, homme, vie*.
# Ce que ces champs lexicaux prouvent, c'est que l'auteur semble proche
# de l'humain et des relations interpersonnelles.


# TÂCHE 4

# Visualisation de la loi de Zipf
tokens |>
  count(mot, name = "freq", sort = TRUE) |>
  mutate(
    rang = row_number()
  ) |>
  ggplot(aes(x = rang, y = freq)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# Oui, la loi de Zipf s'applique au corpus de Balzac.
# En regardant le graphique généré par la commande précédente,
# on peut voir que les extrémités sont plus diversifiées que
# le centre. L'extrémité haute nous montre des données uniques
# séparées par des espaces relativement écartés, tandis que
# l'extrémité basse nous montre une grande quantité de données
# qui ont des valeurs similaires.


# TÂCHE 5

# 1) J'ai choisi de cibler les déterminants numéraux ordinaux.
# Pour ce faire, je ferai mon analyse sur les mots finissant par -ième.

# Création du patron avec les expressions régulières
patron <- "\\b.+ième\\b"

# 2) Calcul de la fréquence relative par œuvres
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
#    title                                                          freq_rel
#  1 Contes bruns                                                   0.000139
#  2 Eugénie Grandet                                                0.000149
#  3 La Comédie humaine - Volume 01                                 0.000252
#  4 La Comédie humaine - Volume 02                                 0.000198
#  5 La Comédie humaine - Volume 03                                 0.000147
#  6 La Comédie humaine - Volume 04                                 0.000295
#  7 La Comédie humaine - Volume 05. Scènes de la vie de Province   0.000263
#  8 La Comédie humaine - Volume 06. Scènes de la vie de Province   0.000301
#  9 La Comédie humaine - Volume 07. Scènes de la vie de Province   0.000194
# 10 La Comédie humaine - Volume 08. Scènes de la vie de Province   0.000325

# 3) Présentation des extrêmes

freq_relative |>
  arrange(desc(freq_rel)) |>
  slice(1)
#   title                        freq_rel
# 1 La Maison du Chat-qui-pelote 0.000409

freq_relative |>
  arrange(freq_rel) |>
  slice(1)
#   title        freq_rel
# 1 Contes bruns 0.000139

# Donc, l'œuvre qui présente la plus haute fréquence relative
# est "La Maison du Chat-qui-pelote" avec 0,000409 et la plus basse
# est "Contes bruns" avec 0,000139


# TÂCHE 6

# Déclaration de la fonction `entropie` servant à calculer l'entropie
entropie <- function(freq) {
  p <- freq / sum(freq)
  p <- p[p > 0]
  -sum(p * log2(p))
}

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
  ) |>
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
