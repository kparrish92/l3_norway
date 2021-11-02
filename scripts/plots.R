
nor <- read.csv(here("data", "tidy", "nor_probs.csv"))
pol <- read.csv(here("data", "tidy", "pol_probs.csv"))
eng <- read.csv(here("data", "tidy", "eng_probs.csv"))

prob_df <- rbind(nor,pol,eng)


e <- prob_df %>% 
  filter(language == "eng") %>% 
  ggplot(aes(x = step, y = probability, color = condition)) + 
  geom_point() + geom_line() + 
  ggtitle("English rating probabilities per condition") +
  ylim(0, .8)

p <- prob_df %>% 
  filter(language == "pol") %>% 
  ggplot(aes(x = step, y = probability, color = condition)) + 
  geom_point() + geom_line() + 
  ggtitle("Polish rating probabilities per condition") +
  ylim(0, .8)

n <- prob_df %>% 
  filter(language == "Nor") %>% 
  ggplot(aes(x = step, y = probability, color = condition)) + 
  geom_point() + geom_line() + 
  ggtitle("Norwegien rating probabilities per condition") +
  ylim(0, .8)



prob_df %>% 
  filter(condition == "acceptable_mono_refl") %>% 
  ggplot(aes(x = step, y = probability, color = language)) + 
  geom_point() + geom_line()

prob_df %>% 
  filter(condition == "acceptable_di_refl") %>% 
  ggplot(aes(x = step, y = probability, color = language)) + 
  geom_point() + geom_line()


prob_df %>% 
  filter(condition == "unacceptable_di_poss") %>% 
  ggplot(aes(x = step, y = probability, color = language)) + 
  geom_point() + geom_line()

prob_df %>% 
  filter(condition == "unacceptable_mono_poss") %>% 
  ggplot(aes(x = step, y = probability, color = language)) + 
  geom_point() + geom_line()



prob_df %>% 
  filter(language == "pol" | language == "Nor") %>% 
  filter(condition == "unacceptable_mono_poss") %>% 
  ggplot(aes(x = step, y = probability, 
             color = language)) + 
  geom_point() + geom_line() + 
  ggtitle("Polish rating probabilities per condition")


ggarrange(e, p, n)
