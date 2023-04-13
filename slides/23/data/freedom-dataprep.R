# load packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)

# load data --------------------------------------------------------------------

freedom <- read_excel(
  here::here("slides/23", "data/Country_and_Territory_Ratings_and_Statuses_FIW_1973-2023.xlsx"),
  sheet = "Country Ratings, Statuses ",
  skip = 1
) %>%
  select(`Year(s) Under Review`, `1990`:`...151`)

# create names -----------------------------------------------------------------

names_freedom <- c(
  "country",
  map(1990:2022, function(x) paste(c("pr", "cl", "status"), x, sep = "_")) %>% unlist()
)

# repair names -----------------------------------------------------------------

names(freedom) <- names_freedom

# remove first row that used to be colnames ------------------------------------

freedom <- freedom %>%
  slice(-1)

# write data -------------------------------------------------------------------

write_csv(freedom, here::here("slides/23", "data/freedom.csv"))
