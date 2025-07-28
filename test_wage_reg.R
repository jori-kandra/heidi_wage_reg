library(realtalk)
library(epiextractr)
library(tidyverse)
library(epidatatools)
library(MetricsWeighted)
library(fixest)

### PARAMETERS ####
# define wage years
current_year <- 2024
var_list <- c("year", "month", "selfemp", "selfinc", "age", "wage", "female", "wbho", "educ",
                "statefips", "married", "a_earnhour", "a_weekpay")

### DATA ####
#### Standard restrictions ####
data <- load_org(1979:current_year, all_of(c(var_list, "orgwgt"))) %>% 
  # Age and selfemp restrictions
  filter(selfemp == 0, age >= 16, !is.na(wage),
         case_when(selfinc == 0 & !is.na(selfinc) ~ TRUE, # filter selfinc == 0 for year >= 1989
                   # keep any year that doesn't have selfinc (selfinc is NA)
                   is.na(selfinc) ~ TRUE, 
                   # exclude all other cases
                   TRUE ~ FALSE))

#### Master wage dataset ####
wage_master <- data %>%
  mutate(lnwage = log(wage),
         age2 = age^2,
         statefips = as_factor(statefips),
         # group states by union density
         #note: Table 1 (https://www.epi.org/307632/pre/5a3eaa53032b9469772ae5ac01b894dae0978c216faf4ef58df724ec06a0c047/)
         state_groups = case_when(
                          statefips %in% c("HI", "NY", "WA", "AK", "CA", 
                                           "NJ", "CT", "OR", "RI", "VT", 
                                           "MI", "MN", "MA", "IL", "OH", 
                                           "NV", "PA") ~ "High union density",
                          statefips %in% c("AL", "DC", "DE", "IA", "IN",
                                           "KS", "KY", "MD", "ME", "MO", 
                                           "MS", "MT", "NE", "NH", "NM", 
                                           "UT", "WV") ~ "Medium union density",
                          statefips %in% c("AR", "AZ", "CO", "FL", "GA",
                                           "ID", "LA", "NC", "ND", "OK", 
                                           "SC", "SD", "TN", "TX", "VA", 
                                           "WI", "WY") ~ "Low union density")) %>%
  # adjust weight variables
  mutate(wgt = case_when(
           year >= 1979 ~ orgwgt/12),
         all = "all") |> 
  # year pooling
  mutate(year = case_when(2022 <= year & year <= 2024 ~ "2022–2024",
                          TRUE ~ NA))

#### Final regression df ####
#note: ungroup and drop invalid data
df <- ungroup(wage_master) %>% filter(wgt > 0, !is.na(lnwage))

# check sample size of state groups 
df |> filter(!is.na(year), a_earnhour != 1, a_weekpay != 1, wbho == 2) |> 
  crosstab(state_groups, year)

### REGRESSION VARIABLES ####
#### Set treatment variables ####
categorical_vars <- c("i(educ, ref = '2')", "i(female, ref = '0')", "i(wbho, ref = '1')")
continuous_vars = c(treatment_vars, "age", "age2")
treatment_vars = paste(treatment_vars, collapse=" + ")

#### Set fixed variables ####
fe_vars = paste(c("married", "statefips"), collapse=" + ")

#### Regression formula
regression_formula <- as.formula(paste(
  "lnwage ~", rhs_vars, "|", fe_vars
))

### OUTPUT ####
#### National Black-white wage gap ####
#note: remove imputed wages
national_bw_gap <- df |> filter(!is.na(year), a_earnhour != 1, a_weekpay != 1) |> 
  # group by overall
  nest(.by = c(year, all)) |> 
  rowwise(-data) %>%  
  # run model
  mutate(model = list(feols(
    regression_formula,
    data = data,
    weights = ~ wgt))) %>%
  # extract nested data
  reframe(broom::tidy(model)) %>%
  # select BW wage gap
  mutate(indicator_id = case_when(
    term == "wbho::2" ~ "hourly_wage_gap_black_white")) |> 
  filter(!is.na(indicator_id)) |> 
  select(year, indicator_id, value = estimate) |> 
  # exponentiate value
  mutate(value = exp(value) - 1) |> 
  # pull value as object
  pull(value)


#### State groups Black-white wage gap ####
#note: remove imputed wages
results = df |> filter(!is.na(year), a_earnhour != 1, a_weekpay != 1)|>  
  # group by state
  nest(.by = c(year, all, state_groups)) |> 
  rowwise(-data) %>%  
  mutate(model = list(feols(
    regression_formula,
    data = data,
    weights = ~ wgt))) %>%
  reframe(broom::tidy(model)) %>%
  mutate(indicator_id = case_when(
    term == "wbho::2" ~ "hourly_wage_gap_black_white")) |> 
  filter(!is.na(indicator_id)) |> 
  select(year, state_groups, indicator_id, value = estimate) |> 
  mutate(value = exp(value) - 1) |> 
  pivot_wider(id_cols = state_groups, names_from = year, values_from = value) |> 
  add_row(state_groups = "National", `2022–2024` = national_bw_gap) |> 
  arrange(match(state_groups, c("National", "Low union density", 
                                "Medium union density", "High union density")))
  

