library(realtalk)
library(epiextractr)
library(tidyverse)
library(epidatatools)
library(MetricsWeighted)
library(fixest)

# define wage years
current_year <- 2024
var_list <- c("year", "month", "selfemp", "selfinc", "age", "wage", "female", "wbho", "educ",
                "statefips", "married", "a_earnhour", "a_weekpay")

#### DATA IMPORTATION ####
# standard restrictions
data <- load_org(1979:current_year, all_of(c(var_list, "orgwgt"))) %>% 
  # Age and selfemp restrictions
  filter(selfemp == 0, age >= 16, !is.na(wage),
         case_when(selfinc == 0 & !is.na(selfinc) ~ TRUE, # filter selfinc == 0 for year >= 1989
                   # keep any year that doesn't have selfinc (selfinc is NA)
                   is.na(selfinc) ~ TRUE, 
                   # exclude all other cases
                   TRUE ~ FALSE))

#### MASTER WAGE DATASET ####
# master wage dataset
wage_master <- data %>%
  mutate(lnwage = log(wage),
         age2 = age^2,
         statefips = as_factor(statefips),
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
  # flatten labels and adjust weight variables
  mutate(# annual weight adjustment
         wgt = case_when(
           # may data wgt ~ finalwgt
           year >= 1979 ~ orgwgt/12),
          all = "all")

# race/ethnicity
#note: regression by wbho looks at MF wage-gap
treatment_vars <- c("i(educ, ref = '2')", "i(female, ref = '0')", "i(wbho, ref = '1')")
  
df <- ungroup(wage_master) %>% filter(wgt > 0, !is.na(lnwage))


# return null if demographic outside scope of function
treatment_vars = c(treatment_vars, "age", "age2")
rhs_vars = paste(treatment_vars, collapse=" + ")

fe_vars = paste(c("married", "statefips"), collapse=" + ")

regression_formula <- as.formula(paste(
  "lnwage ~", rhs_vars, "|", fe_vars
))

results = df %>% mutate(year = case_when(2022 <= year & year <= 2024 ~ "2022–2024",
                                        TRUE ~ NA)) |> 
                 filter(!is.na(year), a_earnhour != 1, a_weekpay != 1)|>  
  nest(.by = c(year, all, state_groups)) |> 
  rowwise(-data) %>%  
  mutate(model = list(feols(
    regression_formula,
    data = data,
    weights = ~ wgt
  ))) 

fe_vars =  c("married")

regression_formula <- as.formula(paste(
  "lnwage ~", rhs_vars, "|", fe_vars
))

results = df %>% filter(year %in% c(1979:1983, 2020:2024), 
                        a_earnhour != 1, a_weekpay != 1)|>  
  nest(.by = c(year, all)) |> 
  rowwise(-data) %>%  
  mutate(model = list(feols(
    regression_formula,
    data = data,
    weights = ~ wgt
  )))  


test <- results %>%
  reframe(broom::tidy(model)) %>%
  mutate(indicator_id = case_when(
    term == "wbho::2" ~ "hourly_wage_gap_black_white")) |> 
  filter(!is.na(indicator_id)) |> 
  select(year, statefips, indicator_id, value = estimate) |> 
  mutate(value = exp(value) - 1,
         statefips = as_factor(statefips)) |> 
  pivot_wider(id_cols = statefips, names_from = year, values_from = value) |> 
  arrange(statefips) |> 
  add_row(statefips = "National", `1979–1981` = -0.079, `2022–2024` = -.116) |> 
  write_csv('./union_heidi_bw_wage_gap_3yr.csv')

test <- results %>%
  reframe(broom::tidy(model)) %>%
  mutate(indicator_id = case_when(
    term == "wbho::2" ~ "hourly_wage_gap_black_white")) |> 
  filter(!is.na(indicator_id)) |> 
  mutate(value = exp(estimate) - 1) |> 
         #statefips = as_factor(statefips)) |> 
    select(year, indicator_id, value) |> 
  pivot_wider(id_cols = statefips, names_from = year, values_from = value) |> 
  arrange(statefips) |> 
  add_row(statefips = "National", `1979–1983` = -0.079, `2020–2024` = -.116) |> 
  write_csv('./union_heidi_bw_wage_gap_5yr.csv')
