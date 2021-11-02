# norway analysis 

norway_data <- read.csv(here("data", "nor.csv"))
norway_data$rating <- as.factor(norway_data$rating)

# Find best fitting norway model 
m0 <- polr(rating ~ 1, 
           data = norway_data, Hess=TRUE)
m1 <- polr(rating ~ sentence_type_nor, 
           data = norway_data, Hess=TRUE) 

summary(m1)

m0 <- clmm(rating ~ 1 + (1 | participant_no), 
           data = norway_data, Hess=TRUE)
m1 <- clmm(rating ~ sentence_type_nor + (1 | participant_no),  
           data = norway_data, Hess=TRUE) 

summary(m1)

anova(m0, m1)

## probability per rating when sentence type is acceptable mono refl 
new_data1 <- data.frame("sentence_type_nor"= "acceptable_mono_refl")
probs1 <- round(predict(m1,new_data1,type = "p"), 3)

## probability per rating when sentence type is acceptable_di_refl
new_data2 <- data.frame("sentence_type_nor"= "acceptable_di_refl")
probs2 <- round(predict(m1,new_data2,type = "p"), 3)

## probability per rating when sentence type is acceptable mono refl 
new_data3 <- data.frame("sentence_type_nor"= "unacceptable_mono_poss")
probs3 <- round(predict(m1,new_data3,type = "p"), 3)

## probability per rating when sentence type is unacceptable_di_poss
new_data4 <- data.frame("sentence_type_nor"= "unacceptable_di_poss")
probs4 <- round(predict(m1,new_data4,type = "p"), 3)

# save output 
nor_probs1 <- data.frame("probability" = 
                          c(probs1),
                        "condition" = "acceptable_mono_refl",
                        "step" = c(1:5)) 

nor_probs2 <- data.frame("probability" = 
                          c(probs2),
                        "condition" = "acceptable_di_refl",
                        "step" = c(1:5)) 

nor_probs3 <- data.frame("probability" = 
                           c(probs3),
                         "condition" = "unacceptable_mono_poss",
                         "step" = c(1:5)) 

nor_probs4 <- data.frame("probability" = 
                           c(probs4),
                         "condition" = "unacceptable_di_poss",
                         "step" = c(1:5)) 

nor_probs <- rbind(nor_probs1,
                   nor_probs2,
                   nor_probs3,
                   nor_probs4) %>% 
  mutate(language = "Nor")

nor_probs %>% 
  write.csv(here("data", "tidy", "nor_probs.csv"))
