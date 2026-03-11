# Séance sur les expression régulières

library(tidyverse)

txt <- "La souris mange le fromage. Le chat mange la souris.
        Le chiens mange le chat. L'ours mange le chiens."

list <- c("chat", "chut", "chot", "chit", "cht")

str_detect(txt, "chat") # is pattern present?
# [1] TRUE

str_extract_all(txt, "chat") # extract all pattern.
# [[1]]
# [1] "chat" "chat"

str_count(txt, "chat") # count pattern occurrence.
# [1] 2

str_detect(list, "ch.t")
# [1]  TRUE  TRUE  TRUE  TRUE FALSE

num <- "j'ai 47 ans"

str_count(num, "[0-9]+")
# [1] 1


# Pratique 1

p1 <- "Marie a 25 ans. Pierre a 30 ans. Sophie a 22 ans."

noms <- p1 |>
  str_extract_all("[A-Z][a-z]+")
# [[1]]
# [1] "Marie"  "Pierre" "Sophie"

nombres <- p1 |>
  str_extract_all("[0-9]+")
# [[1]]
# [1] "25" "30" "22"

no_num <- p1 |>
  str_replace_all("[0-9]", "X")
# [1] "Marie a XX ans. Pierre a XX ans. Sophie a XX ans."

ans <- p1 |>
  str_count("ans")
# [1] 3


test <- "tst.chat"
test |>
  str_detect("\\bch")
# [1] FALSE

str_extract_all("tranquillement", "ment\\b")
# [[1]]
# character(0)
#
