# Polish analysis 

pol <- read.csv(here("data", "tidy", "polish_tidy.csv")) 

polish_data <- read.csv(here("data", "pol.csv"))

polish_data$rating <- as.factor(polish_data$rating)

polish_data$sentence_type_pol
# Find best fitting Polish model 
m0 <- polr(rating ~ 1, 
           data = polish_data, Hess=TRUE)
m1 <- polr(rating ~ sentence_type_pol, 
           data = polish_data, Hess=TRUE) # this one 


anova(m0, m1)

## probability per rating when sentence type is acceptable mono refl 
new_data1 <- data.frame("sentence_type_pol"= "acceptable_mono_refl")
probs1 <- round(predict(m1,new_data1,type = "p"), 3)

## probability per rating when sentence type is acceptable_di_refl
new_data2 <- data.frame("sentence_type_pol"= "acceptable_di_refl")
probs2 <- round(predict(m1,new_data2,type = "p"), 3)

## probability per rating when sentence type is acceptable mono refl 
new_data3 <- data.frame("sentence_type_pol"= "unacceptable_mono_poss")
probs3 <- round(predict(m1,new_data3,type = "p"), 3)

## probability per rating when sentence type is unacceptable_di_poss
new_data4 <- data.frame("sentence_type_pol"= "unacceptable_di_poss")
probs4 <- round(predict(m1,new_data4,type = "p"), 3)


# save output 
pol_probs1 <- data.frame("probability" = 
                           c(probs1),
                         "condition" = "acceptable_mono_refl",
                         "step" = c(1:5)) 

pol_probs2 <- data.frame("probability" = 
                           c(probs2),
                         "condition" = "acceptable_di_refl",
                         "step" = c(1:5)) 

pol_probs3 <- data.frame("probability" = 
                           c(probs3),
                         "condition" = "unacceptable_mono_poss",
                         "step" = c(1:5)) 

pol_probs4 <- data.frame("probability" = 
                           c(probs4),
                         "condition" = "unacceptable_di_poss",
                         "step" = c(1:5)) 

pol_probs <- rbind(pol_probs1,
                   pol_probs2,
                   pol_probs3,
                   pol_probs4) %>% 
  mutate(language = "pol")

pol_probs %>% 
  write.csv(here("data", "tidy", "pol_probs.csv"))