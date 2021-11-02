require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

# within subjects 


eng <- read.csv(here("data", "tidy", "eng_tidy.csv")) %>% 
  rename(sentence_type_code = sentence_type_eng_code)

pol <- read.csv(here("data", "tidy", "polish_tidy.csv")) %>% 
  rename(sentence_type_code = sentence_type_pol_code)

nor <- read.csv(here("data", "tidy", "nor_tidy.csv")) %>% 
  rename(sentence_type_code = sentence_type_nor_code)

all_data <- rbind(eng, pol, nor)



# how many unique participants 
length(unique(all_data$participant_no))

# all data acc/unacc
all_data %>% 
  group_by(group, acceptable) %>% 
  summarize(n = n(), mean = mean(rating), sd_rating = sd(rating))

# monotransitve ratings 
all_data %>% 
  filter(tran == "mono") %>% 
  group_by(group, acceptable) %>% 
  summarize(n = n(), mean = mean(rating), sd_rating = sd(rating))

all_data %>% 
  filter(tran == "di") %>% 
  group_by(group, acceptable) %>% 
  summarize(n = n(), mean = mean(rating), sd_rating = sd(rating))


all_data %>% 
  filter(tran == "mono") %>% 
  group_by(group, acceptable) %>% 
  summarize(n = n(), mean = mean(rating), sd_rating = sd(rating)) %>% 

# all data together 
rating ~ language + tran + poss + (1 | participant_no) + (1 | sentence_code)      

null <- lmer(rating ~  (1 | participant_no), data = all_data)
mod1 <- lmer(rating ~ group + (1 | participant_no), data = all_data)

anova(null, mod1)
# 2 models

# progressive vs reflexive 

# word order 


all_data$rating <- as.factor(all_data$rating)

eng$rating <- as.factor(eng$rating)



# Find best fitting English model 
m0 <- polr(rating ~ 1, data = eng, Hess=TRUE)
m1 <- polr(rating ~ poss, data = eng, Hess=TRUE) # this one 
m2 <- polr(rating ~ poss + tran, data = eng, Hess=TRUE)# no main effect 

anova(m0, m1, m2)

# find prob per step 

## probability per rating when poss = poss 
new_data1 <- data.frame("poss"= "poss")
probs1 <- round(predict(m1,new_data,type = "p"), 3)

## probability per rating when poss = own
new_data2 <- data.frame("poss"= "own")
probs2 <- round(predict(m1,new_data2,type = "p"), 3)



new_data3 <- data.frame("poss"= "own", "tran" = "mono")
probs3 <- round(predict(m1,new_data,type = "p"), 3)

new_data4 <- data.frame("poss"= "own", "tran" = "di")
probs4 <- round(predict(m1,new_data,type = "p"), 3)

sum(probs)
