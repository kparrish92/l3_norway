
source(here("scripts", "00_libs.R"))

nor <- read.csv(here("data", "tidy", "nor_probs.csv"))
pol <- read.csv(here("data", "tidy", "pol_probs.csv"))
eng <- read.csv(here("data", "tidy", "eng_probs.csv"))

# The cond_eff plots were manually saved from the conditional effects function in brms

# load models 
eng_mod <- read_rds(here("data", "models", "refl_english_mod.rds"))
pol_mod <- read_rds(here("data", "models", "refl_polish_mod.rds"))
nor_mod <- read_rds(here("data", "models", "refl_norway_mod.rds"))


# take conditional effects from the models output and make it them a df for plotting 
eng_prob <- conditional_effects(eng_mod, "sentence_type_eng", categorical = TRUE)
english_probs <- data.frame(eng_prob[["sentence_type_eng:cats__"]]) %>% 
  mutate(language = "English") %>% 
  rename("sentence_type" = "sentence_type_eng") %>% 
  mutate(sentence_type = case_when(
    sentence_type == "unacceptable_di_own" ~ "di_refl",
    sentence_type == "unacceptable_mono_own" ~ "mono_refl",
    sentence_type == "acceptable_di_poss" ~ "di_poss",
    sentence_type == "acceptable_mono_poss" ~ "mono_poss"
  ))

english_probs = subset(english_probs, select = -c(as.integer.rating.))

nor_probs <- conditional_effects(nor_mod, "sentence_type_nor", categorical = TRUE)
nor_probs_df <- data.frame(nor_probs[["sentence_type_nor:cats__"]])  %>% 
  mutate(language = "Nor") %>% 
  rename("sentence_type" = "sentence_type_nor")

pol_probs <- conditional_effects(pol_mod, "sentence_type_pol", categorical = TRUE)
pol_probs_df <- data.frame(pol_probs[["sentence_type_pol:cats__"]])  %>% 
  mutate(language = "Polish") %>% 
  rename("sentence_type" = "sentence_type_pol")


pol_probs_df = subset(pol_probs_df, select = -c(as.integer.rating.))

# combine the dfs from the conditional effects objects 
all_df <- rbind(english_probs, pol_probs_df, nor_probs_df)

pos <- position_dodge(.2)
lim <- aes(ymin=lower__, ymax=upper__)



all_df <- all_df %>% 
  mutate(sentence_type = str_remove(sentence_type, "acceptable_")) %>% 
  mutate(sentence_type = str_remove(sentence_type, "un"))


# plot per condition

all_df %>% 
  filter(sentence_type == "di_refl") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Ditransitive-possessive/own condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "di_refl.png"), dpi = 900)


all_df %>% 
  filter(sentence_type == "mono_refl") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Monotrasitive reflexive/own per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "mono_refl.png"), dpi = 900)


all_df %>% 
  filter(sentence_type == "di_poss") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Ditransitive possessive condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "di_poss.png"), dpi = 900)


all_df %>% 
  filter(sentence_type == "mono_poss") %>% 
  ggplot(aes(x = language, y = estimate__, color = effect2__)) + 
  geom_point(size = 3.5, position=pos) + 
  geom_errorbar(lim, position=pos, width=.1) +
  labs(title = "Monotransitive possessive condition per language\n", 
       x = "Language", 
       y = "Probability", 
       color = "Rating\n") +
  geom_hline(yintercept = .2, linetype = "dashed", alpha = .4) +
  ggsave(here("plots", "mono_poss.png"), dpi = 900)

