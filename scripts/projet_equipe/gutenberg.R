# Import libraries
library(tidyverse)
library(gutenbergr)

balzac_ids <- gutenberg_metadata |>
  filter(author == "Balzac, Honoré de") |>
  pull(gutenberg_id)

sand_ids <- gutenberg_metadata |>
  filter(author == "Sand, George") |>
  pull(gutenberg_id)

zola_ids <- gutenberg_metadata |>
  filter(author == "Zola, Émile") |>
  pull(gutenberg_id)

hugo_ids <- gutenberg_metadata |>
  filter(author == "Hugo, Victor") |>
  pull(gutenberg_id)

maupassant_ids <- gutenberg_metadata |>
  filter(author == "Maupassant, Guy de") |>
  pull(gutenberg_id)

balzac_books <- gutenberg_download(balzac_ids)
sand_books <- gutenberg_download(sand_ids)
zola_books <- gutenberg_download(zola_ids)
hugo_books <- gutenberg_download(hugo_ids)
maupassant_books <- gutenberg_download(maupassant_ids)
