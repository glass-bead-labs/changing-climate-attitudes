library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)

dat1 = read.csv("cco_mech_core_intervention_notext.csv")
dat2 = read.csv("cco_mech_delayed_test_notext.csv")



# Is dat1 the Experiment 6 MTurk group?
table(dat1$party) # you guys only have 8 republicans in the whole sample
barplot(table(dat1$conserv), xlab = "conservatism") # sample leans left
table(dat1$conserv, dat1$party)

# 
dat1 %>% 
  select(starts_with("gw")) %>% 
  head

dat1 %>% 
  select(starts_with("gw")) %>% 
  cor(use = "pairwise.complete") %>% 
  round(3)

dat1 %>% 
  gather(key = "question", value = "value", starts_with("gw")) %>% 
  separate(question, into = c("question", "time"), sep = -4) %>% 
  ggplot(aes(x = value, fill = party)) +
  geom_bar() +
  facet_grid(question~time) +
  scale_x_discrete(limits = 1:9)

dat1 %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  gather(key = "question", value = "value", starts_with("gw")) %>% 
  separate(question, into = c("question", "time"), sep = -4) %>% 
  filter(question %in% c("gw1_2_", "gw2_1_", "gw2_2_")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = subj_id)) +
  facet_grid(party~question)

dat1 %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  gather(key = "question", value = "value", starts_with("gw")) %>% 
  separate(question, into = c("question", "time"), sep = -4) %>% 
  ggplot(aes(x = time, y = value, col = party)) +
  geom_point(position = position_jitter(width = .2, height = .5)) +
  geom_line(aes(group = subj_id)) +
  facet_wrap(~question)
  