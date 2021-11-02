library(ordinal)
library(here)
library(tidyverse)

english_data <- read.csv(here("data", "word_order", "eng.csv"))

clmm.graph <- clmm(as.factor(rating) ~ sentence_type + (1 | participant_no),  
                   data = english_data, Hess=TRUE) 

vlines <- c(0, # intercept, 0 = sentence_type_noracceptable_di_refl
            clmm.graph$beta[[1]], # sentence_type_noracceptable_mono_refl
            clmm.graph$beta[[2]], # sentence_type_norunacceptable_di_poss 
            clmm.graph$beta[[3]])# sentence_type_norunacceptable_mono_poss
xaxis <- seq(min(vlines-.5), max(vlines+.5), length.out = 100) # create 100 steps
yaxis <- rep(c(0,1),50) # fill in 0s and 1s for y-axis
colors <- c("one" = "orangered",
            "two" = "orange2",
            "three" = "yellow",
            "four" = "green",
            "five" = "turquoise") # establish the colour-rating level correspondence
tibble(xaxis,yaxis) %>% # baseline tibble for plot dimensions
  mutate(one=  plogis(clmm.graph$Theta[1] - xaxis), # add column for rating levels (must be adapted for larger scales)
         two=  plogis(clmm.graph$Theta[2] - xaxis) - plogis(clmm.graph$Theta[1] - xaxis),
         three=plogis(clmm.graph$Theta[3] - xaxis) - plogis(clmm.graph$Theta[2] - xaxis),
         four= plogis(clmm.graph$Theta[4] - xaxis) - plogis(clmm.graph$Theta[3] - xaxis),
         five=1 - (plogis(clmm.graph$Theta[2] - xaxis))) %>%
  gather(rating,probability,3:7) %>% # make long data (change right-hand number to suit the ordinal levels)
  mutate(rating=factor(rating,levels=c("one","two","three","four","five"))) %>% # make factor and relevel
  ggplot(aes(x=xaxis,y=yaxis)) + # set up ggplot
  geom_hline(yintercept=0,lty="dotted") + # add lower horizontal line
  geom_hline(yintercept=1,lty="dotted") + # add upper horizontal line
  geom_line(aes(y=probability,colour=rating),lwd=1,alpha=.8) + # add predicted curves
  annotate("segment", # type of annotation = line segments
           x=vlines, y=0, xend=vlines, yend=1, # add estimates
           lty="solid", alpha=.75) + # visual properties of vertical lines
  annotate("text", # type of annotation = text
           x=vlines,y=.75, # location of labels
           label=c("main_post_verb",
                   "main_pre_verb",
                   "relative_post_verb",
                   "relative_pre_verb"), # label names aligned with vlines[1:4]
           angle=90,vjust=-0.2) + # visual properties of text labels
  scale_x_continuous(breaks=c(min(xaxis-.5),max(xaxis+.5))) + # expand x axis horizontally
  scale_y_continuous(breaks=c(0,.25,.5,.75,1)) + # expand y axis with consistent breaks
  ylab("Probability") + xlab("") + ggtitle("English Predicted curves") + # label plot properties
  scale_colour_manual(values = colors) + # apply colours manually
  theme_bw() + # improve visibility with white background
  ggsave(here("plots", "word_order", "english_prob_curve_word_order.png"), dpi = 900)