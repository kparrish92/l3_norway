# source libs 
source(here("scripts", "word_order", "00_libs.R"))

norway_data <- read.csv(here("data", "word_order", "nor.csv"))

polish_data <- read.csv(here("data", "word_order", "pol.csv"))

english_data <- read.csv(here("data", "word_order", "eng.csv"))


fit_sc1 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type + (1 | participant_no), 
  data = norway_data, 
  family = cumulative("probit"))

summary(fit_sc1)

fit_sc1 %>% 
  write_rds(here("data", "models", "wo_norway_mod.rds"))

fit_sc2 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type + (1 | participant_no), 
  data = polish_data, 
  family = cumulative("probit"))

summary(fit_sc2)


fit_sc2 %>% 
  write_rds(here("data", "models", "wo_polish_mod.rds"))

conditional_effects(fit_sc2, "sentence_type", categorical = TRUE)

fit_sc3 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type + (1 | participant_no), 
  data = english_data, 
  family = cumulative("probit"))


fit_sc3 %>% 
  write_rds(here("data", "models", "wo_english_mod.rds"))

summary(fit_sc3)
