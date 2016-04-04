library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(BayesFactor)
library(lme4)

# Experiment 2 ----
# I would like to check out this no-pretest control group, but not sure which col that is
exp2.1 = read.csv("UCp_mech_core_intervention_notext.csv")
exp2.1$sample = "UC-Berkeley"
exp2.2 = read.csv("UT_mech_core_intervention_notext.csv")
exp2.2$sample = "UT-Brownsville"
exp2.2 = separate(exp2.2, survey_number, c('f11', 'survey_number'), sep = "-")
exp2.2$survey_number = as.numeric(as.character(exp2.2$survey_number))
# We're gonna combine these data frames so let's make sure survey_number is distinct
intersect(exp2.1$survey_number, exp2.2$survey_number) # not distinct
exp2.2$survey_number = exp2.2$survey_number + 1000
intersect(exp2.1$survey_number, exp2.2$survey_number) # distinct

# Looks like only n_s == s has pre/post (altho sometimes pre-only?)
# n_s == n looks like the post-only control
table(exp2.1$n_s, exp2.1$survey_number)
table(exp2.2$n_s, exp2.2$survey_number)

# so for now let's restrict to pre/post like they report (altho pre-only comparison could be interesting)
exp2.1 = exp2.1 %>% select(X:reread)
exp2.2 = exp2.2 %>% select(X:reread)
exp2 = bind_rows(exp2.1, exp2.2)
exp2$pre_post = factor(exp2$pre_post, levels = c("pre", "post"))

table(exp2$conservative)
barplot(table(exp2$conservative))
table(exp2$party) # 8 republicans out of 175

exp2.flat = exp2 %>%
  gather(key = "question", value = "value", starts_with("gw"))
 
exp2.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  #separate(question, into = c("question", "time"), sep = -4) %>% 
  #filter(question %in% c("gw1_2_", "gw2_1_", "gw2_2_")) %>% 
  ggplot(aes(x = pre_post, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = survey_number)) +
  facet_grid(party~question)

# Bayes factor model?
# sum questions to make single outcome
# NOTE: MIGHT NOT WANT TO DO THIS. CHECK ITEM CODES AND MANUSCRIPT.
exp2 = exp2 %>% 
  mutate(gw_composite = gw1_2 + gw2_1 + gw2_2 + gw2_3 + gw2_4)
# Code and discard those who dropped out before post
exp2.bf = exp2 %>% 
  filter(n_s == "s") %>% 
  select(survey_number, gw_composite, pre_post, conservative) %>% 
  filter(complete.cases(.)) %>% 
  mutate(survey_number = as.factor(survey_number))
tab = table(exp2.bf$survey_number)
tab[tab == 1]
tab[tab == 1] %>% names
exp2.bf = exp2.bf %>% 
  filter(!survey_number %in% names(tab[tab==1]))

# Could plot distribution of change scores given T1 score
# or distribution of change scores given conservative

# run BayesFactor models
exp2.m0 = lmBF(gw_composite ~ survey_number, data = exp2.bf)
exp2.m1 = lmBF(gw_composite ~ pre_post + survey_number, data = exp2.bf)
exp2.m2 = lmBF(gw_composite ~ conservative + survey_number, data = exp2.bf)
exp2.m3 = lmBF(gw_composite ~ conservative + pre_post + survey_number, data = exp2.bf)
exp2.m4 = lmBF(gw_composite ~ conservative * pre_post + survey_number, data = exp2.bf)
# Error "Dependent variable must be numeric"
# And yet:
class(exp2.bf$gw_composite)
lm(gw_composite ~ survey_number, data = exp2.bf) %>% summary
lmer(gw_composite ~ pre_post * conservative + (1|survey_number), data = exp2.bf) %>% summary


# Experiment 3 ----
exp3.1 = read.csv("UCo_mech_core_intervention_notext.csv") 
exp3.2 = read.csv("UCo_mech_full_intervention_notext.csv") 
# Pruning columns
exp3.1 = exp3.1 %>% 
  select(-starts_with("k1time"), -starts_with("k2time"), -starts_with("k3time"),
         -starts_with("codes."), -starts_with("coder."))
exp3.2 = exp3.2 %>% 
  select(-starts_with("k1time"), -starts_with("k2time"), -starts_with("k3time"),
         -starts_with("codes."), -starts_with("coder."),
         -starts_with("Q"))
exp3.1$sample = "core"
exp3.2$sample = "full"

exp3 = bind_rows(exp3.1, exp3.2)

table(exp3$social.cons, useNA='always')
barplot(table(exp3$social.cons, useNA = 'always'))
table(exp3$econ.cons, useNA='always')
barplot(table(exp3$econ.cons, useNA = 'always'))
table(exp3$party, useNA = 'always')
table(exp3$party.pre, useNA = 'always')



# Experiment 4 ----
exp4.1 = read.csv("cco_mech_core_intervention_notext.csv")
exp4.2 = read.csv("cco_mech_delayed_test_notext.csv")

# Still need to fix all this
table(exp4.1$party) # you guys only have 8 republicans in the whole sample
barplot(table(exp4.1$conserv), xlab = "conservatism") # sample leans left
table(exp4.1$party, exp4.1$conserv)

intersect(exp4.1$subj_id, exp4.2$subj_id)
setdiff(exp4.1$subj_id, exp4.2$subj_id)
setdiff(exp4.2$subj_id, exp4.1$subj_id)

exp4 = left_join(exp4.1, exp4.2, by = "subj_id")

exp4 %>% 
  select(conserv, starts_with("gw")) %>% 
  cor(use = 'pairwise') %>% 
  round(3)

exp4.flat = exp4 %>% 
  select(subj_id, party, conserv, starts_with("gw")) %>% 
  gather("question", "value", starts_with("gw")) %>% 
  separate(col = question, into = c("item", "time"), sep = -4) %>% 
  mutate(time = factor(time, levels = c("pre", "pst", "fol"))) 

exp4.flat %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .02, height = .02)) +
  geom_line(aes(group = subj_id)) +
  facet_grid(item ~ party)

exp4.1 %>% 
  gather(key = "question", value = "value", starts_with("gw")) %>% 
  separate(question, into = c("question", "time"), sep = -4) %>% 
  ggplot(aes(x = value, fill = party)) +
  geom_bar() +
  facet_grid(question~time) +
  scale_x_discrete(limits = 1:9)

# This is the good plot
exp4.1 %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  gather(key = "question", value = "value", starts_with("gw")) %>% 
  separate(question, into = c("question", "time"), sep = -4) %>% 
  filter(question %in% c("gw1_2_", "gw2_1_", "gw2_2_")) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point(position = position_jitter(width = .1, height = .02)) +
  geom_line(aes(group = subj_id)) +
  facet_grid(party~question)

# Why does this look like it has more datapoints? What does this show us?
exp4.1 %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  gather(key = "question", value = "value", starts_with("gw")) %>% 
  separate(question, into = c("question", "time"), sep = -4) %>% 
  ggplot(aes(x = time, y = value, col = party)) +
  geom_point(position = position_jitter(width = .2, height = .5)) +
  geom_line(aes(group = subj_id)) +
  facet_wrap(~question)

# Experiment 5 ----
# No political information
exp5 = read.csv("HS_mech+stats_full_intervention.csv")
exp5$time = factor(exp5$time, levels = c("pre", "post", "followup"))

exp5 %>% 
  gather(question, value, GW1:GW4) %>% 
  ggplot(aes(x = time, y = value)) +
  geom_point() +
  geom_line(aes(group = Code)) +
  facet_wrap(~question)