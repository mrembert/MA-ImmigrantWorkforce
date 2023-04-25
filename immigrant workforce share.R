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

# Pulling in PUMS data

ma_pums <- get_pums(
  variables = c("ESR", "NATIVITY"),
  state = "MA",
  survey = "acs1",
  year = 2021,
  recode = TRUE
)

# Calculating shares

df <- ma_pums |> 
  mutate(labor_force = ifelse(ESR <=3, 1,0)) |>
  filter(labor_force == 1) |> 
  summarize(
    total_labor_force = sum(PWGTP),
    foreign_labor_force = sum(PWGTP[NATIVITY==2]),
    foreign_share = foreign_labor_force/total_labor_force
            )
 



