# English analysis 

english_data <- read.csv(here("data", "eng.csv"))

english_data$rating <- as.factor(english_data$rating)

english_data$sentence_type_eng
# Find best fitting english model 
m0 <- polr(rating ~ 1, 
           data = english_data, Hess=TRUE)
m1 <- polr(rating ~ sentence_type_eng, 
           data = english_data, Hess=TRUE) # this one 


anova(m0, m1)

## probability per rating when sentence type is acceptable mono refl 
new_data1 <- data.frame("sentence_type_eng"= "acceptable_mono_poss")
probs1 <- round(predict(m1,new_data1,type = "p"), 3)

## probability per rating when sentence type is acceptable_di_refl
new_data2 <- data.frame("sentence_type_eng"= "acceptable_di_poss")
probs2 <- round(predict(m1,new_data2,type = "p"), 3)

## probability per rating when sentence type is acceptable mono refl 
new_data3 <- data.frame("sentence_type_eng"= "unacceptable_di_own")
probs3 <- round(predict(m1,new_data3,type = "p"), 3)

## probability per rating when sentence type is unacceptable_di_poss
new_data4 <- data.frame("sentence_type_eng"= "unacceptable_mono_own")
probs4 <- round(predict(m1,new_data4,type = "p"), 3)


# save output 
eng_probs1 <- data.frame("probability" = 
                           c(probs1),
                         "condition" = "acceptable_mono_poss",
                         "step" = c(1:5)) 

eng_probs2 <- data.frame("probability" = 
                           c(probs2),
                         "condition" = "acceptable_di_poss", 
                         "step" = c(1:5)) 

eng_probs3 <- data.frame("probability" = 
                           c(probs3),
                         "condition" = "unacceptable_di_own",
                         "step" = c(1:5))

eng_probs4 <- data.frame("probability" = 
                           c(probs4),
                         "condition" = "unacceptable_mono_own",
                         "step" = c(1:5)) 

eng_probs <- rbind(eng_probs1,
                   eng_probs2,
                   eng_probs3,
                   eng_probs4) %>% 
  mutate(language = "eng")

eng_probs %>% 
  write.csv(here("data", "tidy", "eng_probs.csv"))