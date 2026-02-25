library(tidyverse)
library(tidytext)
library(udpipe)
library(tm)

raw <- read_lines("../../donnees/monte_cristo.txt")

df <- tibble(text = raw)

token <- df |>
  unnest_tokens(
    output = word,
    input = text
  )

token |> nrow()

token <- token |>
  mutate(len = str_count(word))

token |>
  slice_max(len)



bigram <- df |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  count(bigram, sort = TRUE)

bigram
