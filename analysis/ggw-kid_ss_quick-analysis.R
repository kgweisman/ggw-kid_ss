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

# make functions to remove NAs
ci_lower_na <- function(x){quantile(x, 0.025, na.rm = T)}
ci_upper_na <- function(x){quantile(x, 0.975, na.rm = T)}
mean_na <- function(x){mean(x, na.rm = T)}

# --- READING IN DATA: PILOT 1 (March 2016) -----------------------------------

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

# --- QUICK PLOTS: PILOT 1 (March 2016) ---------------------------------------

d1 <- d_tidy %>%
  filter(grepl("kp", subid), # run by kara
         !subid %in% c("kp01", "kp02"), # in the current version
         age >= 4.45, # over age 4.45y
         age <= 5.55) # under age 5.55y

# dyoung <- d_tidy %>%
#   filter(grepl("kp", subid), # run by kara
#          !subid %in% c("kp01", "kp02"), # in the current version
#          age < 4.45) # under age 4.45y
# 
# dold <- d_tidy %>%
#   filter(grepl("kp", subid), # run by kara
#          !subid %in% c("kp01", "kp02"), # in the current version
#          age > 5.55) # under age 4.45y

d_means <- multi_boot.data.frame(
  data = d1,
  column = "response",
  summary_groups = c("phase", "character", "type2"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_means <- d_means %>%
  full_join(count(d1, phase, character, type2)) %>%
  ungroup() %>%
  mutate(character = factor(character,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")))

ggplot(data = d_means %>% filter(phase == "test"), 
       aes(x = type2, y = mean_na, colour = type2)) +
  facet_grid(~ character) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  ylim(1, 5.5) +
  labs(title = "Pilot data\n",
       x = "\nTarget character",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

ggplot(data = d1 %>% filter(phase == "test"), 
       aes(x = type2, y = response, colour = type2)) +
  facet_grid(~ character) +
  geom_boxplot(aes(fill = type2), alpha = 0.2) +
  geom_jitter(height = 0.1) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  ylim(1, 5.5) +
  labs(title = "Pilot data\n",
       x = "\nTarget character",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

## by predicate

d_means_bypred <- multi_boot.data.frame(
  data = d1,
  column = "response",
  summary_groups = c("phase", "character", "type2", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

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

# ggplot(data = d_means_bypred %>% filter(phase == "test"), 
#        aes(x = pred2, y = mean_na, colour = predicate)) +
#   facet_wrap(~ character) +
#   geom_point(stat = "identity", position = position_dodge(1), size = 5) +
#   geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
#                 position = position_dodge(1), width = .2, size = .3) +
#   geom_text(aes(y = mean_na + 0.4, label = n)) +
#   theme_bw() +
#   theme(text = element_text(size = 20)) +
#   scale_color_manual(values = kara13qual) +
#   ylim(1, 5.5) +
#   labs(title = "Pilot data\n",
#        x = "\nPredicate",
#        y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

ggplot(data = d_means_bypred %>% filter(phase == "test"), 
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_wrap(~ pred2) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5.5) +
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
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

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
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_wrap(~ pred2) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5.5) +
  labs(title = "Pilot data (3AFC simulation: 2s & 4s become 3s)\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

# drop 2s and 4s to simulate 3AFC
d3 <- d1 %>%
  mutate(response = as.numeric(ifelse(response %in% c(2, 3), NA, response))) %>%
  filter(is.na(response) == F)

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
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5.5) +
  labs(title = "Pilot data (3AFCb simulation: Drop 2s and 4s)\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

# look only at last few kids
d4 <- d1 %>%
  filter(is.na(response) == F,
         as.character(dot) == "2016-03-23")

d_means_bypred_lastDay <- multi_boot.data.frame(
  data = d4,
  column = "response",
  summary_groups = c("phase", "character", "type2", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_means_bypred_lastDay <- d_means_bypred_lastDay %>%
  full_join(count(d4, phase, character, type2, predicate)) %>%
  ungroup() %>%
  mutate(character = factor(character,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")),
         pred4 = factor(ifelse(type2 == "catch", "(catch trial)",
                               as.character(predicate))))

ggplot(data = d_means_bypred_lastDay %>% filter(phase == "test"), 
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_wrap(~ pred4) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(1, 5.5) +
  labs(title = "Pilot data (last day)\n",
       x = "\nPredicate",
       y = "Mean rating (1 = Totally Serious, 5 = Totally Silly)\n")

# CATCH TRIALS PILOT (pilot2, May 2016) ---------------------------------------

# read in data

# read in raw data
d2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot2 data/pilot2_data_2016-05-18.csv")[-1] # get rid of column of obs numbers

# read in counterbalancing info
cb2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/counterbalancing/cb_sequences_pilot2.csv") %>%
  mutate_each(funs(factor)) %>%
  mutate(trial = as.numeric(as.character(trial)),
         trial = trial - 2) # correct for practice trials

# tidy and merge
d2_tidy <- d2 %>%
  mutate(sequence = factor(sequence),
         trial = trial - 2) %>% # correct for practice trials
  select(-target, -predicate) %>% # get rid of incomplete info
  full_join(cb2 %>% select(sequence, block, trial, 
                           target, predicate, prediction)) %>%
  mutate(dob = parse_date_time(dob, orders = "%m/%d/%y"),
         dot = parse_date_time(dot, orders = "%m/%d/%y"),
         age = (dot - dob)/365,
         response_num = ifelse(response == "serious", 0,
                               ifelse(response == "in between", 1,
                                      ifelse(response == "silly", 2,
                                             NA))))

# plot

d2_means <- multi_boot.data.frame(
  data = d2_tidy,
  column = "response_num",
  summary_groups = c("phase", "prediction", "target", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d2_means <- d2_means %>%
  full_join(count(d2_tidy, phase, target, prediction)) %>%
  ungroup() %>%
  mutate(character = factor(target,
                            levels = c("grownups", "kids", "babies",
                                       "dogs", "bears", "bugs",
                                       "robots", "computers", "cars", "staplers",
                                       "icecream", "strawberries")))

ggplot(data = d2_means %>% filter(phase == "test"), 
       aes(x = target, y = mean_na, colour = predicate)) +
  facet_wrap(~ prediction) +
  geom_point(stat = "identity", position = position_dodge(0.5), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(0.5), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.3, label = n)) +
  geom_text(aes(y = mean_na + 0.1, label = predicate)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  # scale_color_manual(values = kara13qual) +
  ylim(0, 2.5) +
  labs(title = "Pilot2 data\n",
       x = "\nTarget",
       y = "Mean rating (0 = serious, 1 = in between, 2 = silly\n")

# # plot
# 
# d2_means <- multi_boot.data.frame(
#   data = d2_tidy,
#   column = "response_num",
#   summary_groups = c("phase", "prediction", "target", "predicate"),
#   statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))
# 
# d2_means <- d2_means %>%
#   full_join(count(d2_tidy, phase, target, prediction)) %>%
#   ungroup() %>%
#   mutate(target = factor(target,
#                          levels = c("grownups", "kids", "babies",
#                                     "dogs", "bears", "bugs",
#                                     "robots", "computers", "cars", "staplers",
#                                     "icecream", "strawberries")),
#          label = paste0(predicate, "\n", "(n=", n, ")"),
#          importance = factor(
#            ifelse(prediction == "silly" & 
#                     target %in% c("grownups", "kids", "babies",
#                                   "dogs", "bears", "bugs"),
#                   "important",
#                   ifelse(prediction == "serious" & 
#                            target %in% c("staplers", "cars", "computers", "robots"),
#                          "important", "unimportant"))))
# 
# ggplot(data = d2_means %>% filter(phase == "test"),
#        aes(x = target, y = mean_na, colour = importance)) +
#   facet_wrap(~ prediction) +
#   geom_point(stat = "identity", position = position_dodge(width = 1), size = 5) +
#   geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
#                 position = position_dodge(width = 1), width = .2, size = .3) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_hline(yintercept = 1, lty = 3) +
#   geom_hline(yintercept = 2, lty = 3) +
#   # geom_text(aes(y = mean_na + 0.3, label = n)) +
#   # geom_text(aes(y = mean_na + 0.1, label = predicate)) +
#   geom_text_repel(aes(label = label),
#                   # nudge_x = -0.5,
#                   size = 5,
#                   box.padding = unit(1, 'lines'),
#                   point.padding = unit(1, 'lines'),
#                   segment.color = "gray",
#                   segment.size = 0.5,
#                   arrow = arrow(length = unit(0.01, 'npc')),
#                   force = 10) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none") +
#   # scale_color_manual(values = kara13qual) +
#   ylim(-0.25, 2.25) +
#   labs(title = "Pilot2 data\n",
#        x = "\nTarget",
#        y = "Mean rating (0 = serious, 1 = in between, 2 = silly\n")
# 
