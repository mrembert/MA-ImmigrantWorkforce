library(tidycensus)
library(tidyverse)
library(srvyr)


# Looking up PUMS variables

lf_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("employment", var_label, ignore.case = TRUE))

## employment status - ESR
## 1 - Civilian employed, at work
## 2 - Civilian employed, not at work
## 3 - Unemployed


nativity_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("nativity", var_label, ignore.case = TRUE))

## Country of origin - NATIVITY
## Foreign Born - 2

edu_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("education", var_label, ignore.case = TRUE))

## Educational Attainment - SCHL
## 21 - Bachelor's degree
## 22 - Master's degree
## 23 - Professional degree
## 24 - Doctorate

english_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("english", var_label, ignore.case = TRUE))

## Ability to speak English - ENG
## b - Speaks English Only
## 1 - Very Well
## 2 - Well
## 3 - Not Well
## 4 - Not at all


# Pulling in PUMS data ----------------

ma_pums <- get_pums(
  variables = c("ESR", "NATIVITY", "SCHL", "ENG", "AGEP"),
  state = "MA",
  survey = "acs1",
  year = 2021,
  recode = TRUE,
  rep_weights = "person"
) |> 
  to_survey()


# Calculating English proficiency for foreign born workers ---------------


df_english <- ma_pums |>
  mutate(ENG = ifelse(ENG == "b", "0", ENG)) |>
  mutate(ENG = as.numeric(ENG)) |> 
  filter(ESR %in% c("1", "2", "3"), # is in labor force
         NATIVITY == 2
  ) |> 
  group_by(ENG_label) |> 
  summarize(
    english_pct = survey_mean(vartype = "ci"),
    english_n = survey_total(vartype = "ci")
  )


# Calculating English proficiency for foreign born workers with a bach degree or higher---------------


df_english <- ma_pums |>
  mutate(ENG = ifelse(ENG == "b", "0", ENG)) |>
  mutate(ENG = as.numeric(ENG)) |> 
  filter(ESR %in% c("1", "2", "3"), # is in labor force
         NATIVITY == 2, # is foreign born
         SCHL %in% c("21","22","23","24") # bach degree or higher
  ) |> 
  group_by(ENG_label) |> 
  summarize(
    english_pct = survey_mean(vartype = "ci"),
    english_n = survey_total(vartype = "ci")
  )

