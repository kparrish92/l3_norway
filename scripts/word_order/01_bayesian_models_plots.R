library(tidyverse)
library(here)
library(brms)
# norway analysis 

norway_data <- read.csv(here("data", "word_order", "nor.csv"))

polish_data <- read.csv(here("data", "word_order", "pol.csv"))

english_data <- read.csv(here("data", "word_order", "eng.csv"))


fit_sc1 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type, 
  data = norway_data, 
  family = cumulative("probit"))

summary(fit_sc1)
conditional_effects(fit_sc1, "sentence_type", categorical = TRUE)

fit_sc2 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type, 
  data = polish_data, 
  family = cumulative("probit"))

summary(fit_sc2)
conditional_effects(fit_sc2, "sentence_type", categorical = TRUE)

fit_sc3 <- brm(
  formula = as.integer(rating) ~ 1 + sentence_type, 
  data = english_data, 
  family = cumulative("probit"))

summary(fit_sc3)
conditional_effects(fit_sc3, "sentence_type", categorical = TRUE)
