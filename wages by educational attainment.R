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

wages_vars <- pums_variables %>% 
  filter(year == 2021, survey == "acs1") %>%
  filter(level == "person", grepl("wage", var_label, ignore.case = TRUE))

## Wages and salary income past 12 months - WAGP 



# Pulling in PUMS data ----------------

ma_pums <- get_pums(
  variables = c("ESR", "NATIVITY", "SCHL", "WAGP", "AGEP", "FSCHP"),
  state = "MA",
  survey = "acs1",
  year = 2021,
  recode = TRUE,
  rep_weights = "person"
) |> 
  to_survey()


# Calculating median wages and salaries for workers with a bachelor's degree or higher ---------------


df_bach_plus <- ma_pums |> 
  filter(ESR %in% c("1", "2", "3"), # is in labor force
         SCHL %in% c("21", "22", "23", "24"), # Bachelor's degree or higher and above
         FSCHP == 0 # Not in school
         ) |> 
  group_by(NATIVITY_label) |> 
  summarize(
    median_wage = survey_median(WAGP, vartype = "ci")
  )
  

# Calculating median wages and salaries for workers with ONLY a bachelor's degree ---------------

df_bach_only <- ma_pums |> 
  filter(ESR %in% c("1", "2", "3"), # is in labor force
         SCHL %in% c("21"), # Bachelor's degree only
         FSCHP == 0 # Not in school
         ) |> 
  group_by(NATIVITY_label) |> 
  summarize(
    median_wage = survey_median(WAGP, vartype = "ci")
  )

# Calculating median wages and salaries for workers with an advanced degree ---------------

df_advanced_degree <- ma_pums |> 
  filter(ESR %in% c("1", "2", "3"), # is in labor force
         SCHL %in% c("22", "23", "24"), # Advanced degree
         FSCHP == 0 # Not in school
         ) |> 
  group_by(NATIVITY_label) |> 
  summarize(
    median_wage = survey_median(WAGP, vartype = "ci")
  )




