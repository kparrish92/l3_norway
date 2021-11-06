# This script runs Bayesian Ordinal regressions per language and saves the output
# under data/models

source(here("scripts", "00_libs.R"))

# load data

norway_data <- read.csv(here("data", "nor.csv"))

polish_data <- read.csv(here("data", "pol.csv"))

english_data <- read.csv(here("data", "eng.csv"))


fit_sc1 <- brm(
  formula = rating ~ 1 + sentence_type_nor + (1 | participant_no), 
  data = norway_data, 
  family = cumulative("probit"))

fit_sc1 %>% 
  write_rds(here("data", "models", "refl_norway_mod.rds"))


fit_sc2 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type_pol + (1 | participant_no), 
  data = polish_data, 
  family = cumulative("probit"))

fit_sc2 %>% 
  write_rds(here("data", "models", "refl_polish_mod.rds"))

fit_sc3 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type_eng + (1 | participant_no), 
  data = english_data, 
  family = cumulative("probit"))

fit_sc3 %>% 
  write_rds(here("data", "models", "refl_english_mod.rds"))

