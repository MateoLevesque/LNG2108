# Réponses de Matéo Lévesque au questionnaire 2
# du cours LNG-2108 Linguistique de corpus.

# ATTENTION !
# Ce fichier doit être placé à la racine du dépôt du cours
# pour que la lecture des fichiers se fasse correctement.
# Si vous voulez utiliser ce fichier ailleurs, vous devrez
# changer les lignes 72 et 212 pour indiquer le bon chemin de fichier.

# Importation des bibliothèques nécessaires
library(tidyverse)
library(tidytext)
library(tm)

# QUESTION 1

# Création du vecteur
notes <- c(12, NA, 15, 8, NA, 17)

# Calcul de la moyenne en excluant les NA
mean(notes, na.rm = TRUE)
# [1] 13

# La moyenne est de 13, comme on peut voir à la ligne 22.


# QUESTION 2

# Réponse a)
# `read_csv()` utilise la virgule comme séparateur.
# `read_csv2()` utilise le point-virgule.

# QUESTION 3

# Réponse a)
# Chaque variable = une colonne.
# Chaque observation = une ligne.

# QUESTION 4

# Code fourni dans le questionnaire
df <- tibble(
  mot = c("espagnol", "le", "parler", "français", "un", "anglaise"),
  classe = c("NOUN", "DET", "VERB", "NOUN", "DET", "ADJ")
)

# Calcul de la longueur moyenne des noms
df |>
  mutate(longueur = str_count(mot)) |> # `mutate()` ajoute ici une colonne à `df`.
  filter(classe == "NOUN") |> # `filter()` élimine les lignes où la `classe` n'est pas "NOUN".
  summarize(moyenne = mean(longueur)) # `summarize()` affiche le résultat dans un nouveau tibble.
# # A tibble: 1 × 1
#   moyenne
#     <dbl>
# 1       8

# Comme on peut le voir à la ligne 55,
# la longueur moyenne des noms est de 8 caractères.

# QUESTION 5

# Voici le code que j'aurais exécuté si le fichier "donnees.csv"
# était placé dans le dossier `donnees` dans le dépôt git du cours :
# donnees <- read_csv2("./donnees.csv", locale = locale(encoding = "Latin1"))

# QUESTION 6

# ATTENTION ! Ici, il est important d'importer la bibliothèque
# `tidytext` avec `library(tidytext)` (voir ligne 12).

# Lecture du fichier "monte_cristo.txt"
mtcristo <- read_lines("./donnees/monte_cristo.txt")

# Transformation du texte brut en tibble
q6 <- tibble(texte = mtcristo)

# Calcul du nombre total de tokens dans le texte
q6 |>
  unnest_tokens(mot, texte) |>
  nrow()
# [1] 475222

# Comme on peut le voir à la ligne 81, il y a 475222 tokens dans le texte.

# QUESTION 7

# Réponse b)
# Elle convertit tout en minuscules

# QUESTION 8

# Création de la variable qui contient la phrase
pq8 <- "Le corpus contient des textes de différentes
              langues et différentes époques."

# Transformation de la phrase en tibble
q8 <- tibble(texte = pq8)

# Tokenisation en bigrammes et calcul du nombre total de bigrammes
q8 |>
  unnest_tokens(bigramme, texte, token = "ngrams", n = 2) |>
  nrow()
# [1] 10

# Comme on peut le voir à la ligne 103, je compte 10 bigrammes dans la phrase.
# Il semble y avoir toujours un bigramme de moins que le nombre de mots,
# puisque, lorsqu'on compte les bigrammes, on compte les combinaisons de deux
# mots consécutifs. Donc, le dernier mot d'une chaîne de caractères n'ayant
# pas de mot suivant ne sera pas compté comme un bigramme.

# QUESTION 9

# En français, la ponctuation peut parfois jouer un rôle important dans
# l'orthographe. Par exemple, les apostrophes sont problématiques dans la
# tokenisation, comme dans le token "l'avion" si on décide d'enlever
# les apostrophes. On a aussi des problèmes avec des mots comme "aujourd'hui".
# Un autre problème serait la gestion des mots composés. Comment devrions-
# nous tokeniser des mots comme "pomme de terre", "couvre-lits" ou "sac à dos" ?

# QUESTION 10

# Réponse b)
# La fréquence relative permet de comparer des corpus de tailles différentes

# QUESTION 11

# Transformation du texte brut en tibble.
# (prend en compte que le texte a déjà été lu au préalable)
q11 <- tibble(texte = mtcristo)

# Calcul de la fréquence relative et de la fréquence relative
# par million de mots + ajout de leurs colonnes respectives +
# tri des 10 tokens les plus fréquents.
q11 |>
  unnest_tokens(mot, texte) |>
  count(mot, sort = TRUE) |>
  mutate(
    frq_rel = n / sum(n),
    frq_par_million = (n / sum(n)) * 1e6
  ) |>
  slice_max(n, n = 10)
# # A tibble: 10 × 4
#    mot       n frq_rel frq_par_million
#    <chr> <int>   <dbl>           <dbl>
#  1 de    18658  0.0393          39262.
#  2 le    12082  0.0254          25424.
#  3 et    11452  0.0241          24098.
#  4 la    11073  0.0233          23301.
#  5 à      9880  0.0208          20790.
#  6 vous   8267  0.0174          17396.
#  7 que    8034  0.0169          16906.
#  8 il     6994  0.0147          14717.
#  9 je     6018  0.0127          12664.
# 10 un     5786  0.0122          12175.

# Vous pourrez voir la réponse à la question aux lignes 145 à 154.

# QUESTION 12

# Transformation du texte brut en tibble.
# (prend en compte que le texte a déjà été lu au préalable)
q12 <- tibble(texte = mtcristo)

# Tokenisation du texte
tokens <- q12 |>
  unnest_tokens(mot, texte)

# Calcul du nombre total de tokens.
n_tokens <- tokens |>
  nrow()
# [1] 475222

# Calcul du nombre de tokens uniques (types).
n_types <- tokens |>
  count(mot) |>
  nrow()
# [1] 24056

# Calcul du TTR (Type Token Ratio)
ttr <- n_types / n_tokens
# [1] 0.05062055

# Comme on peut le voir à la ligne 181,
# le TTR pour le texte est d'environ 0,051.

# QUESTION 13

# Réponse d)
# 2 000

# QUESTION 14

# Transformation du texte brut en tibble.
# (prend en compte que le texte a déjà été lu au préalable)
q14 <- tibble(texte = mtcristo)

# calcul du nombre de tokens uniques +
# ajout de la colonne qui indique le numéro de la ligne pour chaque valeur +
# affichage avec `geom_point()`.
q14 |>
  unnest_tokens(mot, texte) |>
  count(mot, sort = TRUE) |>
  mutate(ligne = row_number()) |>
  ggplot(aes(log10(ligne), log10(n))) +
  geom_point()

# Pour voir la réponse, vous devez exécuter le pipeline ci-haut.

# QUESTION 15

# Lecture du texte "monte_cristo.txt"
mtcristo <- read_lines("./donnees/monte_cristo.txt")

# Transformation du texte brut en tibble.
q15 <- tibble(texte = mtcristo)

# Tokenisation
tokens <- q15 |>
  unnest_tokens(mot, texte)

# Sélection des 5 mots les plus fréquents.
top5 <- tokens |>
  count(mot) |>
  slice_max(n, n = 5)
# # A tibble: 5 × 2
#   mot       n
#   <chr> <int>
# 1 de    18658
# 2 le    12082
# 3 et    11452
# 4 la    11073
# 5 à      9880

# Calcul du TTR (Type Token Ratio).
n_tokens <- tokens |> nrow()

n_types <- tokens |>
  count(mot) |>
  nrow()

ttr <- n_types / n_tokens
# [1] 0.05062055

# Comme nous l'avons vu à la question 12, le TTR est d'environ 0,051.

# Création de la variable qui contient les stopwords du français
# Il est aussi important d'importer la bibliothèque `tm`. (voir ligne 13)
stopwords <- stopwords(kind = "fr")

# Sélection des tokens ne figurant pas dans la liste des stopwords.
sans_sw <- tokens |>
  filter(!mot %in% stopwords)

# Sélection des 5 mots les plus fréquents parmi la nouvelle liste de tokens.
new_top5 <- sans_sw |>
  count(mot) |>
  slice_max(n, n = 5)
# # A tibble: 5 × 2
#   mot       n
#   <chr> <int>
# 1 dit    4079
# 2 comme  2065
# 3 plus   2001
# 4 c'est  1979
# 5 bien   1966

# On peut voir le résultat aux lignes 261 à 265.

# Tokenisation en bigrammes +
#
top5_bigramme <- q15 |>
  unnest_tokens(bigramme, texte, token = "ngrams", n = 2) |>
  # na.omit() |>
  count(bigramme) |>
  slice_max(n, n = 5)
# # A tibble: 5 × 2
#   bigramme         n
#   <chr>        <int>
# 1 <NA>         21404
# 2 de la         2063
# 3 monte cristo  1231
# 4 à la          1181
# 5 le comte      1119

# Ici, on remarque à la ligne 279 que le bigramme le plus fréquent
# a la valeur NA. Si l'on voulait obtenir un résultat sans NA, on pourrait
# ajouter `na.omit()` dans le pipeline comme à la ligne 273.
