# This is the script used to visualize the raw data in the report.rmd file. 

### conditions
# Acceptable_mono_refl = acceptable monotransitive reflexive
# Unacceptable_mono_refl = unacceptable monotransitive possessive
# Acceptable_di_refl = acceptable ditransitive reflexive
# Unacceptable_di_poss = unacceptable ditransitive possessive

library(here)
library(readxl)
library(tidyverse)

# read and tidy data for each language
polish_data <- read.csv(here("data", "pol.csv")) %>% 
  separate(sentence_type_pol, into = c("acceptable", "type"), 
           sep = "_") %>% 
  mutate(group = "Polish")

english_data <- read.csv(here("data", "eng.csv")) %>% 
  separate(sentence_type_eng, into = c("acceptable", "tran", "poss"), 
           sep = "_") %>% 
  mutate(group = "English")

norway_data <- read.csv(here("data", "nor.csv")) %>% 
  separate(sentence_type_nor, into = c("acceptable", "tran", "poss"), 
           sep = "_") %>% 
  mutate(group = "Norweigan")


# remove NA (text) and transform rating to numeric
polish_data <- polish_data %>%
  filter(!rating == "NA")
polish_data$rating <- as.numeric(polish_data$rating)

polish_data %>% 
  write.csv(here("data", "tidy", "polish_tidy.csv"))

english_data <- english_data %>%
  filter(!rating == "NA")
english_data$rating <- as.numeric(english_data$rating)

english_data %>% 
  write.csv(here("data", "tidy", "eng_tidy.csv"))

norway_data <- norway_data %>%
  filter(!rating == "NA")
norway_data$rating <- as.numeric(norway_data$rating)

norway_data %>% 
  write.csv(here("data", "tidy", "nor_tidy.csv"))

# plots 
p1 <- ggplot(polish_data, aes(
  x = acceptable,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = polish_data,
    aes(
      x = acceptable,
      y = rating,
      colour = tran,
      shape = poss
    ),
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .8,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("Polish ratings") + 
  ggsave(path = here("plots"), filename =  "pol_full.png")

         

p2 <- ggplot(english_data, aes(
  x = acceptable,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = english_data,
    aes(
      x = acceptable,
      y = rating,
      colour = tran,
      shape = poss),
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .8,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("English ratings") + 
  ggsave(path = here("plots"), filename =  "eng_full.png")

         

p3 <- ggplot(norway_data, aes(
  x = acceptable,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = norway_data,
    aes(
      x = acceptable,
      y = rating,
      colour = tran,
      shape = poss
    ),
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("Norwegian ratings") + 
  ggsave(path = here("plots"), filename = "nor_full.png")

         

gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

## make charts giving numbers of ratings per condition in each lang.

# number of ratings of acceptable stim in Polish 
polish_data %>% 
  filter(acceptable == "acceptable") %>% 
  group_by(rating) %>% 
  summarise(n = n()) %>% 
  
polish_data_a <- polish_data %>% 
  filter(acceptable == "acceptable") 

ggplot(polish_data_a, aes(
  x = tran,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = polish_data_a,
    aes(
      x = tran,
      y = rating,
      colour = tran,
      shape = poss
    ),
    show.legend = FALSE,
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("Polish acceptable stimuli") + 
  ggsave(path = here("plots"), filename =  "pol_acc.png")



polish_data %>% 
  filter(acceptable == "unacceptable") %>% 
  group_by(rating) %>% 
  summarise(n = n())


polish_data_u <- polish_data %>% 
  filter(acceptable == "unacceptable") 


ggplot(polish_data_u, aes(
  x = tran,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = polish_data_u,
    aes(
      x = tran,
      y = rating,
      colour = tran,
      shape = poss
    ),
    show.legend = FALSE,
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("Polish unacceptable stimuli") + 
  ggsave(path = here("plots"), filename =  "pol_un.png")



# number of ratings of acceptable stim in english 
english_data %>% 
  filter(acceptable == "acceptable") %>% 
  group_by(rating) %>% 
  summarise(n = n()) 

  english_data_a <- english_data %>% 
  filter(acceptable == "acceptable") 

ggplot(english_data_a, aes(
  x = tran,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = english_data_a,
    aes(
      x = tran,
      y = rating,
      colour = tran,
      shape = poss
    ),
    show.legend = FALSE,
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("english acceptable stimuli") + 
  ggsave(path = here("plots"), filename =  "eng_acc.png")



english_data %>% 
  filter(acceptable == "unacceptable") %>% 
  group_by(rating) %>% 
  summarise(n = n())


english_data_u <- english_data %>% 
  filter(acceptable == "unacceptable") 


ggplot(english_data_u, aes(
  x = tran,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = english_data_u,
    aes(
      x = tran,
      y = rating,
      colour = tran,
      shape = poss
    ),
    show.legend = FALSE,
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("english unacceptable stimuli") + 
  ggsave(path = here("plots"), filename =  "eng_un.png")



# number of ratings of acceptable stim in norway 
norway_data %>% 
  filter(acceptable == "acceptable") %>% 
  group_by(rating) %>% 
  summarise(n = n()) 

norway_data_a <- norway_data %>% 
  filter(acceptable == "acceptable") 

ggplot(norway_data_a, aes(
  x = tran,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = norway_data_a,
    aes(
      x = tran,
      y = rating,
      colour = tran,
      shape = poss
    ),
    show.legend = FALSE,
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("Norweigen acceptable stimuli") + 
ggsave(path = here("plots"), filename =  "nor_acc.png")



norway_data %>% 
  filter(acceptable == "unacceptable") %>% 
  group_by(rating) %>% 
  summarise(n = n())


norway_data_u <- norway_data %>% 
  filter(acceptable == "unacceptable") 


ggplot(norway_data_u, aes(
  x = tran,
  y = rating,
  fill = poss
)) +
  geom_point(
    data = norway_data_u,
    aes(
      x = tran,
      y = rating,
      colour = tran,
      shape = poss
    ),
    show.legend = FALSE,
    position = position_jitterdodge(
      jitter.width = .08,
      jitter.height = 0.16,
      dodge.width = .2
    ),
    size = 2,
    alpha = .6,
    shape = 20,
    inherit.aes = FALSE
  ) + ggtitle("norweigen unacceptable stimuli") + 
  ggsave(path = here("plots"), filename =  "nor_un.png")



