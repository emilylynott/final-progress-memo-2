## load packages ----
library(tidyverse)

## tidying ----
alcohol_gho_tidy <- alcohol_gho |>
  pivot_wider(
    names_from = "dimension", 
    values_from = "subgroup") |>
  mutate(indicator_name = factor(indicator_name))

names <- alcohol_gho_tidy |>
  distinct(indicator_name)