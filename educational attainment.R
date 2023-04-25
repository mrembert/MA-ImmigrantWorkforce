library(tidycensus)
library(tidyverse)


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


# Pulling in PUMS data

ma_pums <- get_pums(
  variables = c("ESR", "NATIVITY", "SCHL"),
  state = "MA",
  survey = "acs1",
  year = 2021,
  recode = TRUE,
  rep_weights = TRUE
)

# Calculating share of workers with bachelor's degree or higher

df <- ma_pums |> 
  mutate(ba_above = SCHL %in% c("21","22","23","24")) |> 
  filter(ESR %in% c("1","2","3")) |> 
  group_by(NATIVITY_label) |> 
  summarize(
    total_labor_force = sum(PWGTP),
    ba_above = sum(PWGTP[ba_above == TRUE]),
    ba_above_pct = ba_above/total_labor_force
  )




