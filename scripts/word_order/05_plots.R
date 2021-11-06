# source libs 
source(here("scripts", "word_order", "00_libs.R"))

# load models 
eng_mod <- read_rds(here("data", "models", "wo_english_mod.rds"))
pol_mod <- read_rds(here("data", "models", "wo_polish_mod.rds"))
nor_mod <- read_rds(here("data", "models", "wo_norway_mod.rds"))


# take conditional effects from the models output and make it them a df for plotting 
eng_prob <- conditional_effects(eng_mod, "sentence_type", categorical = TRUE)
english_probs <- data.frame(eng_prob[["sentence_type:cats__"]]) %>% 
  mutate(language = "English")

nor_probs <- conditional_effects(nor_mod, "sentence_type", categorical = TRUE)
nor_probs_df <- data.frame(nor_probs[["sentence_type:cats__"]])  %>% 
  mutate(language = "Nor")

pol_probs <- conditional_effects(pol_mod, "sentence_type", categorical = TRUE)
pol_probs_df <- data.frame(pol_probs[["sentence_type:cats__"]])  %>% 
  mutate(language = "Polish")

# combine the dfs from the conditional effects objects 
all_df <- rbind(english_probs, pol_probs_df, nor_probs_df)

# plot per condition
all_df %>% 
  filter(sentence_type == "main_post_verb") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__, alpha = .8)) +
  geom_point(position = position_jitterdodge()) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), pos = position_jitterdodge())


all_df %>% 
  filter(sentence_type == "main_post_verb") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Main post-verb condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "word_order", "main_post.png"), dpi = 900)


all_df %>% 
  filter(sentence_type == "main_pre_verb") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Main pre-verb condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "word_order", "main_pre.png"), dpi = 900)


all_df %>% 
  filter(sentence_type == "relative_post_verb") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Relative post-verb condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "word_order", "rel_post.png"), dpi = 900)

  
  
all_df %>% 
  filter(sentence_type == "relative_pre_verb") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Relative pre-verb condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "word_order", "rel_pre.png"), dpi = 900)


  