library(tidyverse)
library(tidytext)

# load RData files
load("balzac.RData")

ent <- function(text) {
  freq <- text |>
    unnest_tokens(word, ) |>
    count(word, sort = TRUE)
  p <- freq / sum(freq)
  p <- p[p > 0]
  -sum(p * log2(p))
}

files <- c("balzac.RData", "sand.RData", "zola.RData", "hugo.RData", "maupassant.RData")

# This function takes a long time before being done.
# Run only if necessary.
# The output of the result is at line 38.
result <- map_dfr(files, function(f) {
  message("Processing: ", f)
  varname <- load(f)
  text <- get(varname)

  # Ensure text is a character vector and in a tibble
  tokens <- tibble(text = as.character(text)) |>
    tidytext::unnest_tokens(word, text)

  tibble(
    file = f,
    n_tokens = nrow(tokens),
    entropy = ent(text)
  )
})

result
