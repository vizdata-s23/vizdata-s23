# load packages –---------------------------------------------------------------

library(tidyverse)

# load data –-------------------------------------------------------------------

durham_raw <- read_csv(
  here::here("slides/16/data", "durham-2020-raw.csv"),
  na = c("", "9")
  ) |>
  janitor::clean_names()

# subset and clean data –-------------------------------------------------------

durham_subset <- durham_raw |>
  select(
    id, 
    contains("_3_"), 
    contains("_12_"),
    contains("_18_"),
    contains("_24_"),
    have_you_or_someone_in_your_househol_26:block_lat
    )

# update names and levels as needed –-------------------------------------------

durham <- durham_subset |>
  rename(primary_language = what_is_the_primary_language_used_in_34) |>
  mutate(
    primary_language = case_match(
      primary_language,
      1 ~ "English",
      2 ~ "Spanish",
      3 ~ "Other"
    )
  )

# update all names –------------------------------------------------------------

#tibble(
#  original = names(durham_subset),
#  updated = NA
#) |>
#  write_csv(here::here("slides/16/data", "rename-lookup-original.csv"))

# write data -------------------------------------------------------------------

write_csv(durham, here::here("slides/16/data", "durham-2020.csv"))
