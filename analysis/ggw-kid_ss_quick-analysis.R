# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stats)
library(stringr)
library(langcog)
library(lubridate)

# clear environment
rm(list=ls())
graphics.off()

# --- READING IN DATA ---------------------------------------------------------

# read in raw data
d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot data/pilot_data_2016-03-23.csv")[-1] # get rid of column of obs numbers

# read in counterbalancing info
cb <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/counterbalancing/cb_sequences.csv") %>%
  mutate_each(funs(factor)) %>%
  mutate(trial = as.numeric(as.character(trial)),
         char_order = as.numeric(as.character(char_order)))

# tidy and merge
d_tidy <- d %>%
  mutate(trial = trial - 2) %>% # correct for practice trials
  select(-target, -predicate) %>% # get rid of incomplete info
  full_join(cb %>% select(sequence, block, speaker, trial, 
                          character, type, predicate)) %>%
  mutate(character = factor(ifelse(trial == -1, "icecream",
                                   ifelse(trial == 0, "strawberries",
                                          as.character(character)))),
         predicate = factor(ifelse(trial == -1, 
                                   ifelse(subid %in% c("np01", 
                                                       "np02",  
                                                       "kp01", 
                                                       "kp02"),
                                          "tastes sweet", # minor change
                                          "is very cold"),
                                   ifelse(trial == 0, "are blue",
                                          as.character(predicate)))),
         speaker = factor(ifelse(phase == "practice", "boy", 
                                 as.character(speaker))),
         type = factor(ifelse(character == "icecream", 
                              "practice_serious", 
                              ifelse(character == "strawberries", 
                                     "practice_silly",
                                     as.character(type)))),
         type2 = factor(ifelse(type %in% c("catch_serious", 
                                           "catch_silly"), "catch",
                               ifelse(type %in% c("practice_serious", 
                                                  "practice_silly"), 
                                      "practice",
                                      "test"))),
         dob = parse_date_time(dob, orders = "%m/%d/%y"),
         dot = parse_date_time(dot, orders = "%m/%d/%y"),
         age = (dot - dob)/365)

# --- QUICK PLOTS -------------------------------------------------------------

d1 <- d_tidy %>%
  filter(grepl("kp", subid), # run by kara
         !subid %in% c("kp01", "kp02"), # in the current version
         age >= 4.45, # over age 4.45y
         age <= 5.55) # under age 5.55y

d_means <- multi_boot.data.frame(
  data = d1,
  column = "response",
  summary_groups = c("phase", "character", "type2"),
  statistics_functions = c("ci_lower", "mean", "ci_upper"))

d_means <- d_means %>%
  full_join(count(d1, phase, character, type2)) %>%
  ungroup() %>%
  mutate(character = factor(character,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")))

ggplot(data = d_means %>% filter(phase == "test"), 
       aes(x = type2, y = mean, colour = type2)) +
  facet_grid(~ character) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  ylim(1, 5) +
  labs(title = "Pilot data\n",
       x = "\nTarget character",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

ggplot(data = d1 %>% filter(phase == "test"), 
       aes(x = type2, y = response, colour = type2)) +
  facet_grid(~ character) +
  geom_boxplot(aes(fill = type2), alpha = 0.2) +
  geom_jitter(height = 0.1) +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  ylim(1, 5) +
  labs(title = "Pilot data\n",
       x = "\nTarget character",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

## by predicate

d_means_bypred <- multi_boot.data.frame(
  data = d1,
  column = "response",
  summary_groups = c("phase", "character", "type2", "predicate"),
  statistics_functions = c("ci_lower", "mean", "ci_upper"))

d_means_bypred <- d_means_bypred %>%
  full_join(count(d1, phase, character, type2, predicate)) %>%
  ungroup() %>%
  mutate(character = factor(character,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")),
         pred2 = factor(ifelse(type2 == "catch", "(catch trial)",
                               as.character(predicate))))

library(RColorBrewer)
kara13qual=sort(c(brewer.pal(12, "Set3"), "#194452"))

ggplot(data = d_means_bypred %>% filter(phase == "test"), 
       aes(x = pred2, y = mean, colour = predicate)) +
  facet_wrap(~ character) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5) +
  labs(title = "Pilot data\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

ggplot(data = d_means_bypred %>% filter(phase == "test"), 
       aes(x = character, y = mean, colour = predicate)) +
  facet_wrap(~ pred2) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5) +
  labs(title = "Pilot data\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

# recode 2s and 4s as 3s to simulate 3AFC
d2 <- d1 %>%
  mutate(response = ifelse(response %in% c(2, 3, 4), 3, response))

d_means_bypred_3AFC <- multi_boot.data.frame(
  data = d2,
  column = "response",
  summary_groups = c("phase", "character", "type2", "predicate"),
  statistics_functions = c("ci_lower", "mean", "ci_upper"))

d_means_bypred_3AFC <- d_means_bypred_3AFC %>%
  full_join(count(d2, phase, character, type2, predicate)) %>%
  ungroup() %>%
  mutate(character = factor(character,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")),
         pred2 = factor(ifelse(type2 == "catch", "(catch trial)",
                               as.character(predicate))))

ggplot(data = d_means_bypred_3AFC %>% filter(phase == "test"), 
       aes(x = character, y = mean, colour = predicate)) +
  facet_wrap(~ pred2) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5) +
  labs(title = "Pilot data (3AFC simulation: 2s & 4s become 3s)\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

# drop 2s and 4s to simulate 3AFC
d3 <- d1 %>%
  mutate(response = as.numeric(ifelse(response %in% c(2, 3), NA, response))) %>%
  filter(is.na(response) == F)

ci_lower_na <- function(x){quantile(x, 0.025, na.rm = T)}
ci_upper_na <- function(x){quantile(x, 0.975, na.rm = T)}
mean_na <- function(x){mean(x, na.rm = T)}

d_means_bypred_3AFCb <- multi_boot.data.frame(
  data = d3,
  column = "response",
  summary_groups = c("phase", "character", "type2", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_means_bypred_3AFCb <- d_means_bypred_3AFCb %>%
  full_join(count(d3, phase, character, type2, predicate)) %>%
  ungroup() %>%
  mutate(character = factor(character,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")),
         pred3 = factor(ifelse(type2 == "catch", "(catch trial)",
                               as.character(predicate))))

ggplot(data = d_means_bypred_3AFCb %>% filter(phase == "test"), 
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_wrap(~ pred3) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 14)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5) +
  labs(title = "Pilot data (3AFCb simulation: Drop 2s and 4s)\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")