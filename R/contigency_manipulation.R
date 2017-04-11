library(dplyr)
library(ggplot2)
library(tidyr)

# read in contingency table files
sport_con <- read.csv('~/Documents/research/neander_trauma/Data/contingency.csv')
neander_con <- read.csv('~/Documents/research/neander_trauma/Data/neander_contingency.csv')


sport_con %>%
  mutate(sum = head_neck + shoulder_arm + hand + pelvis + leg + foot +trunk) %>% 
  mutate(head_neck = head_neck/sum, 
         shoulder_arm = shoulder_arm/sum,
         hand = hand/sum,
         pelvis = pelvis/sum,
         leg = leg/sum,
         foot = foot/sum,
         trunk = trunk/sum) %>%
  select(-sum) %>%
  write.csv(.,file = "~/Documents/research/neander_trauma/Data/contingency_norm.csv")
