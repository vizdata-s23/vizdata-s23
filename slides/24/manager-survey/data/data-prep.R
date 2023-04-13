# Load packages –---------------------------------------------------------------

library(tidyverse)

# Load data --------------------------------------------------------------------

manager_survey_raw <- read_csv(
  "manager-survey/data/survey.csv",
  na = c("", "NA"),
  show_col_types = FALSE
)

# Prep data --------------------------------------------------------------------

manager_survey <- manager_survey_raw |>
  filter(
    !is.na(industry),
    !is.na(highest_level_of_education_completed),
    currency == "USD"
  ) |>
  mutate(
    industry_other = fct_lump_min(industry, min = 100),
    country = countrycode(country, origin = "country.name", destination = "cldr.name.en"),
    highest_level_of_education_completed = fct_relevel(
      highest_level_of_education_completed,
      "High School",
      "Some college",
      "College degree",
      "Master's degree",
      "Professional degree (MD, JD, etc.)",
      "PhD"
    ),
    highest_level_of_education_completed = fct_recode(
      highest_level_of_education_completed,
      "Professional degree" = "Professional degree (MD, JD, etc.)"
    )
  )

# Write data -------------------------------------------------------------------

write_rds(manager_survey, "manager-survey/data/manager-survey.rds")
