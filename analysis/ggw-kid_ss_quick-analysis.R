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
library(ggrepel)

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

d_means <- multi_boot(
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

d_means_bypred <- multi_boot(
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

d_means_bypred_3AFC <- multi_boot(
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

d_means_bypred_3AFCb <- multi_boot(
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

d_means_bypred_lastDay <- multi_boot(
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

# --- CATCH TRIALS PILOT (PILOT 2, May 2016) ----------------------------------

# read in data

# read in raw data
d2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot2 data/pilot2_data_2016-06-10.csv")[-1] # get rid of column of obs numbers

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
         response = factor(
           ifelse(response %in% c("serious", "not at all"), "serious/not at all",
                  ifelse(response %in% c("in between", "a little"), "in between/a little",
                         ifelse(response %in% c("silly", "really"), "silly/really",
                                NA))),
           levels = c("serious/not at all", "in between/a little", "silly/really")),
         response_num = ifelse(response == "serious/not at all", 0,
                               ifelse(response == "in between/a little", 1,
                                      ifelse(response == "silly/really", 2,
                                             NA))),
#          response = factor(response, levels = c("serious", "in between", "silly")),
#          response_num = ifelse(response == "serious", 0,
#                                ifelse(response == "in between", 1,
#                                       ifelse(response == "silly", 2,
#                                              NA))),
         importance = factor(
           ifelse(prediction == "silly" & 
                    target %in% c("grownups", "kids", "babies",
                                  "dogs", "bears", "bugs"),
                  "important",
                  ifelse(prediction == "serious" & 
                           target %in% c("staplers", "cars", "computers", "robots"),
                         "important", "unimportant"))))

# plot

d2_means <- multi_boot(
  data = d2_tidy,
  column = "response_num",
  summary_groups = c("phase", "prediction", "importance", "target", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d2_means <- d2_means %>%
  full_join(count(d2_tidy, phase, target, prediction)) %>%
  ungroup() %>%
  mutate(target = factor(target,
                         levels = c("grownups", "kids", "babies",
                                    "dogs", "bears", "bugs",
                                    "robots", "computers", "cars", "staplers",
                                    "icecream", "strawberries")),
         label = paste0(predicate, "\n", "(n=", n, ")"))

# ggplot(data = d2_means %>% filter(phase == "test"), 
#        aes(x = target, y = mean_na, colour = predicate)) +
#   facet_wrap(~ prediction) +
#   geom_hline(yintercept = 0, lty = 3) +
#   geom_hline(yintercept = 1, lty = 3) +
#   geom_hline(yintercept = 2, lty = 3) +
#   geom_point(stat = "identity", position = position_dodge(0.5), size = 5) +
#   geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
#                 position = position_dodge(0.5), width = .2, size = .3) +
#   geom_text(aes(y = mean_na + 0.2, label = label)) +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "none") +
#   # scale_color_manual(values = kara13qual) +
#   ylim(0, 2.5) +
#   labs(title = "Pilot2 data\n",
#        x = "\nTarget",
#        y = "Mean rating (0 = serious, 1 = in between, 2 = silly\n")


unimp_area <- rbind(serious = c(d2_means %>% 
                                  filter(phase == "test",
                                         importance == "unimportant",
                                         prediction == "serious") %>%
                                  summarise(min_mean = min(mean_na),
                                            min_lower = min(ci_lower_na)) %>%
                                  as.numeric(), 
                                d2_means %>% 
                                  filter(phase == "test",
                                         importance == "unimportant",
                                         prediction == "serious") %>%
                                  summarise(max_mean = max(mean_na),
                                            max_upper = max(ci_upper_na)) %>%
                                  as.numeric()),
                    silly = c(d2_means %>% 
                                filter(phase == "test",
                                       importance == "unimportant",
                                       prediction == "silly") %>%
                                summarise(min_mean = min(mean_na),
                                          min_lower = min(ci_lower_na)) %>%
                                as.numeric(), 
                              d2_means %>% 
                                filter(phase == "test",
                                       importance == "unimportant",
                                       prediction == "silly") %>%
                                summarise(max_mean = max(mean_na),
                                          max_upper = max(ci_upper_na)) %>%
                                as.numeric())) %>%
  data.frame() %>%
  add_rownames(var = "prediction") %>%
  rename(min_mean = X1, min_lower = X2, max_mean = X3, max_upper = X4)

d2_means <- full_join(d2_means, unimp_area)

ggplot(data = d2_means %>% filter(phase == "test"),
       aes(x = target, y = mean_na, colour = importance, group = predicate)) +
  facet_wrap(~ prediction) +
  geom_rect(xmin = -Inf, xmax = Inf, 
            aes(ymin = min_lower, ymax = max_upper),
            fill = "turquoise", alpha = 0.005, size = 0) +
  geom_rect(xmin = -Inf, xmax = Inf, 
            aes(ymin = min_mean, ymax = max_mean),
            fill = "turquoise", alpha = 0.007, size = 0) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_hline(yintercept = 2, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(0.7), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(0.7), width = .2, size = .3) +
  geom_label(aes(y = -1.5, label = paste0(predicate, " (n = ", n, ")")), 
             position = position_dodge(0.7), alpha = 0.5, hjust = 0, size = 5) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  # scale_color_manual(values = kara13qual) +
  scale_y_continuous(limits = c(-1.5, 2), breaks = 0:2) +
  labs(title = "Pilot2 data: Means\n",
       x = "Target",
       y = "\nMean rating (0 = serious/not at all,\n1 = in between/a little, 2 = silly/really\n") +
  coord_flip()

ggplot(data = d2_tidy %>% 
         filter(phase == "test") %>%
         mutate(target_by_predicate = factor(paste(target, predicate))),
       aes(x = target_by_predicate, fill = response, gruop = target,
           label = prediction, alpha = importance)) +
  # facet_grid(. ~ prediction) +
  geom_bar() +
  scale_alpha_discrete(range = c(1, 0.3)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  geom_text(y = 0.5, angle = 90, hjust = 0, size = 6) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(title = "Pilot2 data: Response distribution\n",
       x = "\nPhrase",
       y = "Count of ratings\n")

# --- PILOT 3 (June-July 2016) ------------------------------------------------

# --- READING IN DATA: PILOT 3 (June-July 2016) -------------------------------

# read in raw data
d3 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot3 data/pilot3_data_2016-07-17.csv")[-1] # get rid of column of obs numbers

# read in counterbalancing info
cb3 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/counterbalancing/cb_sequences_pilot3.csv") %>%
  mutate_each(funs(factor)) %>%
  mutate(trial = as.numeric(as.character(trial)),
         sequence = as.numeric(as.character(sequence)))

# tidy and merge
d_tidy3 <- d3 %>%
  mutate(trial = trial - 2) %>% # correct for practice trials
  select(-target, -predicate) %>% # get rid of incomplete info
  full_join(cb3 %>% select(sequence, block, trial, 
                          character, predicate)) %>%
  mutate(character = factor(ifelse(trial == -1, "icecream",
                                   ifelse(trial == 0, "strawberries",
                                          as.character(character))),
                            levels = c("grownup", "kid", "baby",
                                       "dog", "bear", "bug",
                                       "robot", "computer", "car",
                                       "stapler")),
         predicate = factor(ifelse(trial == -1, 
                                   ifelse(subid %in% c("np01", 
                                                       "np02",  
                                                       "kp01", 
                                                       "kp02"),
                                          "tastes sweet", # minor change
                                          "is very cold"),
                                   ifelse(trial == 0, "are blue",
                                          as.character(predicate))),
                            levels = c("hunger", "feelings", "thinking")),
         dob = parse_date_time(dob, orders = "%m/%d/%y"),
         dot = parse_date_time(dot, orders = "%m/%d/%y"),
         age = (dot - dob)/365,
         responseCat = factor(
           ifelse(grepl("normal", response), "normal",
                  ifelse(grepl("little", response), "a little silly",
                         ifelse(grepl("really", response), "really silly",
                                NA))),
           levels = c("normal", "a little silly", "really silly")),
         responseNum = ifelse(grepl("normal", responseCat), 0, 
                              ifelse(grepl("little", responseCat), 1,
                                     ifelse(grepl("really", responseCat), 2,
                                            NA))),
         characterNum = as.numeric(character),
         block = factor(block, 
                        levels = c("1", "2", "3", "practice"),
                        labels = c("block 1", "block 2", "block 3", "practice")))

# --- QUICK PLOTS: PILOT 3 (June-July 2016) -----------------------------------

d_means3 <- multi_boot(
  data = d_tidy3,
  column = "responseNum",
  summary_groups = c("character", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_means3 <- d_means3 %>%
  full_join(count(d_tidy3, character, predicate)) %>%
  ungroup() %>%
  filter(!character %in% c("icecream", "strawberries"),
         !is.na(character)) %>%
  mutate(character = 
           factor(character,
                  levels = c("grownup", "kid", "baby",
                             "dog", "bear", "bug",
                             "robot", "computer", "car", "stapler")),
         predicate = 
           factor(predicate,
                  levels = c("hunger", "feelings", "thinking")))

library(RColorBrewer)
kara13qual=sort(c(brewer.pal(12, "Set3"), "#194452"))

ggplot(data = d_means3, 
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_wrap(~ predicate) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(0, 2) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n")

# version where characters are numbers
d_means3a <- d_means3 %>%
  mutate(characterNum = as.numeric(character))

ggplot(data = d_means3a, 
       aes(x = characterNum, y = mean_na, colour = predicate)) +
  facet_wrap(~ predicate) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(0, 2) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n") +
  geom_smooth(method = "loess")

# by block

d_means3_byblock <- multi_boot(
  data = d_tidy3,
  column = "responseNum",
  summary_groups = c("block", "character", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_means3_byblock <- d_means3_byblock %>%
  full_join(count(d_tidy3, block, character, predicate)) %>%
  ungroup() %>%
  filter(!character %in% c("icecream", "strawberries"),
         !is.na(character)) %>%
  mutate(character = 
           factor(character,
                  levels = c("grownup", "kid", "baby",
                             "dog", "bear", "bug",
                             "robot", "computer", "car", "stapler")),
         predicate = 
           factor(predicate,
                  levels = c("hunger", "feelings", "thinking")))

library(RColorBrewer)
kara13qual=sort(c(brewer.pal(12, "Set3"), "#194452"))

ggplot(data = d_means3_byblock, 
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_grid(block ~ predicate) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(0, 2) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n")

# version where characters are numbers
d_means3_byblocka <- d_means3_byblock %>%
  mutate(characterNum = as.numeric(character))

ggplot(data = d_means3_byblocka, 
       aes(x = characterNum, y = mean_na, colour = predicate)) +
  facet_grid(block ~ predicate) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(0, 2) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n") +
  geom_smooth(method = "loess")

# by trial

d_means3_bytrial <- multi_boot(
  data = d_tidy3 %>% mutate(half = ifelse(trialNum < 16, "half 1", "half 2")),
  column = "responseNum",
  summary_groups = c("half", "character", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_means3_bytrial <- d_means3_bytrial %>%
  full_join(d_tidy3 %>%
              mutate(half = ifelse(trialNum < 16, "half 1", "half 2")) %>%
              count(half, character, predicate)) %>%
  ungroup() %>%
  filter(!character %in% c("icecream", "strawberries"),
         !is.na(character)) %>%
  mutate(character = 
           factor(character,
                  levels = c("grownup", "kid", "baby",
                             "dog", "bear", "bug",
                             "robot", "computer", "car", "stapler")),
         predicate = 
           factor(predicate,
                  levels = c("hunger", "feelings", "thinking")))

library(RColorBrewer)
kara13qual=sort(c(brewer.pal(12, "Set3"), "#194452"))

ggplot(data = d_means3_bytrial, 
       aes(x = character, y = mean_na, colour = predicate)) +
  facet_grid(half ~ predicate) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(0, 2) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n")

# version where characters are numbers
d_means3_bytriala <- d_means3_bytrial %>%
  mutate(characterNum = as.numeric(character))

ggplot(data = d_means3_bytriala, 
       aes(x = characterNum, y = mean_na, colour = predicate)) +
  facet_grid(half ~ predicate) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = mean_na + 0.4, label = n)) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  ylim(0, 2) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n") +
  geom_smooth(method = "loess")

# counts

ggplot(data = filter(d_tidy3, phase == "test"), 
       aes(x = character, fill = responseCat)) +
  facet_grid(. ~ predicate) +
  geom_bar(position = "stack") + # for counts
  # geom_bar(position = "fill") + # for proporitions
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       y = "Count of responses\n",
       # y = "Proportion of responses\n",
       fill = "Response")

ggplot(data = filter(d_tidy3, phase == "test"), 
       aes(x = character, fill = responseCat)) +
  facet_grid(block ~ predicate) +
  # geom_bar(position = "stack") + # for counts
  geom_bar(position = "fill") + # for proporitions
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       # y = "Count of responses\n",
       y = "Proportion of responses\n",
       fill = "Response")

ggplot(data = filter(d_tidy3, phase == "test") %>%
         mutate(half = ifelse(trial <= 15, "first half", "second half")), 
       aes(x = character, fill = responseCat)) +
  facet_grid(half ~ predicate) +
  # geom_bar(position = "stack") + # for counts
  geom_bar(position = "fill") + # for proporitions
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  labs(title = "Pilot data\n",
       x = "\nCharacter",
       # y = "Count of responses\n",
       y = "Proportion of responses\n",
       fill = "Response")

# --- POISSON ANALYSES: PILOT 3 (June-July 2016) ------------------------------

r1 <- glm(responseNum ~ poly(characterNum, 3), 
          data = filter(d_tidy3, phase == "test", is.na(responseNum) == F), 
          family = "poisson")
summary(r1)

r2 <- glm(responseNum ~ poly(characterNum, 3) + predicate, 
          data = filter(d_tidy3, phase == "test", is.na(responseNum) == F), 
          family = "poisson")
summary(r2)

r3 <- glm(responseNum ~ poly(characterNum, 3) * predicate, 
          data = filter(d_tidy3, phase == "test", is.na(responseNum) == F), 
          family = "poisson")
summary(r3)

anova(r1, r2, r3)

r4 <- glm(responseNum ~ poly(characterNum, 3) + 
            poly(characterNum, 3):predicate, 
          data = filter(d_tidy3, phase == "test", is.na(responseNum) == F), 
          family = "poisson")
summary(r4)

# half 1 only

r1_half1 <- glm(responseNum ~ poly(characterNum, 3), 
          data = filter(d_tidy3, phase == "test", trialNum < 16, is.na(responseNum) == F), 
          family = "poisson")
summary(r1_half1)

r2_half1 <- glm(responseNum ~ poly(characterNum, 3) + predicate, 
          data = filter(d_tidy3, phase == "test", trialNum < 16, is.na(responseNum) == F), 
          family = "poisson")
summary(r2_half1)

r3_half1 <- glm(responseNum ~ poly(characterNum, 3) * predicate, 
          data = filter(d_tidy3, phase == "test", trialNum < 16, is.na(responseNum) == F), 
          family = "poisson")
summary(r3_half1)

anova(r1_half1, r2_half1, r3_half1)

r4_half1 <- glm(responseNum ~ poly(characterNum, 3) + 
            poly(characterNum, 3):predicate, 
          data = filter(d_tidy3, phase == "test", trialNum < 16, is.na(responseNum) == F), 
          family = "poisson")
summary(r4_half1)

# --- READING IN DATA: RUN 1 (July-Februrary 2017) -------------------------------

# read in raw data
dr1 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/run1 data/run1_data_2017-02-16.csv")[-1] # get rid of column of obs numbers

# read in counterbalancing info
cb3 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/counterbalancing/cb_sequences_run1.csv") %>%
  mutate_each(funs(factor)) %>%
  mutate(trial = as.numeric(as.character(trial)),
         sequence = as.numeric(as.character(sequence)))

# read in demographics info
demo_r1 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/sillynormal_participant_ages_2016-08-08.csv") %>%
  mutate_each(funs(factor))

# tidy and merge
d_tidyr1 <- dr1 %>%
  select(-target, -predicate, -ethnicity, -gender, -dob, -dot) %>% # get rid of incomplete info
  full_join(cb3) %>%
  full_join(demo_r1) %>% 
  select(-age_formula) %>%
  mutate(character = gsub(" ", "", character)) %>%
  filter(!character %in% c("", "LEAVEBLANK"), !is.na(character)) %>%
  filter(!predicate %in% c("", "LEAVEBLANK"), !is.na(predicate)) %>%
  mutate(response = tolower(response)) %>%
  mutate(character = factor(as.character(character),
                            levels = c("strawberries", "icecream",
                                       "stapler", "guitar", "car",
                                       "computer", "cellphone", "robot",
                                       "bug", "bear", "dog",
                                       "baby", "kid", "grownup")),
         char_cat = factor(as.character(char_cat),
                           levels = c("artifact", "technology", "animal", "human")),
         predicate = factor(
           ifelse(grepl("hung", as.character(predicate)), "hunger",
                  ifelse(grepl("feel", as.character(predicate)), "feelings",
                         ifelse(grepl("think", as.character(predicate)), "thinking",
                                ifelse(grepl("blue", as.character(predicate)), "are blue",
                                             ifelse(grepl("cold", as.character(predicate)), "is very cold", 
                                                          NA))))),
           levels = c("are blue", "is very cold", "hunger", "feelings", "thinking")),
         predicate_wording = factor(
           ifelse(grepl("hung", as.character(predicate)), "can get hungry",
                  ifelse(grepl("feel", as.character(predicate)), "have feelings",
                         ifelse(grepl("think", as.character(predicate)), "can think",
                                ifelse(grepl("blue", as.character(predicate)), "are blue",
                                             ifelse(grepl("cold", as.character(predicate)), "is very cold",
                                                          NA))))),
           levels = c("are blue", "is very cold",
                      "can get hungry", "have feelings", "can think")),
         responseCat = factor(
           ifelse(grepl("normal", as.character(response)), "normal",
                  ifelse(grepl("little", as.character(response)), "a little silly",
                         ifelse(grepl("really", as.character(response)), "really silly",
                                NA))),
           levels = c("normal", "a little silly", "really silly")),
         responseNum = ifelse(grepl("normal", as.character(responseCat)), 0, 
                              ifelse(grepl("little", as.character(responseCat)), 1,
                                     ifelse(grepl("really", as.character(responseCat)), 2,
                                            NA))),
         characterNum = as.numeric(character),
         age = as.numeric(as.character(age)))

# take a look at demographics
d_tidyr1 %>% distinct(subid, .keep_all = T) %>% count(gender)

d_tidyr1 %>% 
  distinct(subid, .keep_all = T) %>% 
  summarise(mean = mean(age),
            sd = sd(age),
            median = median(age),
            min = min(age),
            max = max(age))

qplot(d_tidyr1$age)

# limit to target age range
# HACK 2017-02-16
right_age_kids <- c("o04", "o05", "o06", "a16", "o14", "o18", "o19", "o11", "o15", "o20", "o23", "o12", "o13", "a14", "a15", "o22", "o25", "o33", "o27", "o26", "o35", "o29", "o36", "o30", "a13", "o32", "o37", "o39", "o40", "n01", "n02", "n03", "n04", "n05", "n06", "n07", "n08", "n09", "n10")

d_tidyr1 %>% 
  filter(subid %in% right_age_kids) %>%
  # filter(experimenter == "JNS" | (age >= 4.5 & age <= 5.5)) %>% # JNS = nicky at bing
  distinct(subid, .keep_all = T) %>% 
  count(gender)

d_tidyr1 %>% 
  filter(subid %in% right_age_kids) %>%
  # filter(experimenter == "JNS" | (age >= 4.5 & age <= 5.5)) %>% # JNS = nicky at bing
  distinct(subid, .keep_all = T) %>% 
  summarise(mean = mean(age),
            sd = sd(age),
            median = median(age),
            min = min(age),
            max = max(age))

qplot(age, data = d_tidyr1 %>%
        filter(subid %in% right_age_kids) %>%
        # filter(experimenter == "JNS" | (age >= 4.5 & age <= 5.5)) %>% # JNS = nicky at bing
        distinct(subid, .keep_all = T))

d_tidyr1 %>% 
  filter(subid %in% right_age_kids) %>%
  # filter(experimenter == "JNS" | (age >= 4.5 & age <= 5.5)) %>% # JNS = nicky at bing
  distinct(subid, .keep_all = T) %>% 
  count(ethnicity)

# --- QUICK PLOTS: RUN 1 (July-Februrary 2017) -----------------------------------

d_meansr1 <- multi_boot(
  data = d_tidyr1 %>% filter(subid %in% right_age_kids),
  column = "responseNum",
  summary_groups = c("character", "predicate"),
  statistics_functions = c("ci_lower_na", "mean_na", "ci_upper_na"))

d_meansr1 <- d_meansr1 %>%
  full_join(count(d_tidyr1 %>% filter(subid %in% right_age_kids), character, predicate)) %>%
  ungroup() %>%
  filter(!character %in% c("icecream", "strawberries"),
         !is.na(character)) %>%
  mutate(character = 
           factor(character,
                  levels = c("stapler", "guitar", "car",
                             "computer", "cellphone", "robot",
                             "bug", "bear", "dog",
                             "baby", "kid", "grownup")),
         predicate = 
           factor(predicate,
                  levels = c("hunger", "feelings", "thinking")))

library(RColorBrewer)
kara13qual=sort(c(brewer.pal(12, "Set3"), "#194452"))

ggplot(data = d_meansr1, 
       aes(x = character, y = mean_na)) +
  facet_wrap(~ predicate) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_hline(yintercept = 2, lty = 3) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = -0.2, label = paste0("n=", n))) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(-0.2, 2) +
  labs(title = "Mean responses by mental capacity and character\n",
       x = "\nCharacter",
       y = "Mean rating (0 = Normal, 2 = Really Silly)\n")

# version where characters are numbers
d_meansr1a <- d_meansr1 %>%
  mutate(characterNum = as.numeric(character),
         char_cat = ifelse(character %in% c("grownup", "kid", "baby",
                                            "dog", "bear", "bug"),
                           "animate", 
                           ifelse(character %in% c("robot", "cellphone", "computer",
                                                   "car", "guitar", "stapler"),
                                  "inanimate", NA)))

ggplot(data = d_meansr1a, 
       aes(x = characterNum, y = mean_na, colour = char_cat, group = predicate)) +
  facet_grid(predicate ~ .) +
  geom_hline(yintercept = 0, lty = 3) +
  geom_hline(yintercept = 1, lty = 3) +
  geom_hline(yintercept = 2, lty = 3) +
  geom_smooth(method = "loess", alpha = 0, fullrange = T) +
  geom_point(stat = "identity", position = position_dodge(1), size = 5) +
  geom_errorbar(aes(ymin = ci_lower_na, ymax = ci_upper_na), 
                position = position_dodge(1), width = .2, size = .3) +
  geom_text(aes(y = -0.2, label = paste0("n=", n))) +
  scale_x_continuous(breaks = 1:12, 
                     labels = c("stapler", "guitar", "car",
                                "computer", "cellphone", "robot",
                                "bug", "bear", "dog",
                                "baby", "kid", "grownup")) +
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  ylim(-0.2, 2) +
  labs(colour = "Character category: ",
       x = "\nCharacter",
       y = "Mean rating\n") +
  ggtitle(expression(atop("Mean ratings", 
                          paste("0 = ", italic("Normal"),
                                ", 1 = ", italic("A little silly"),
                                ", 2 = ", italic("Really silly")))))

# counts with really silly on the bottom
ggplot(data = filter(d_tidyr1, phase == "test", subid %in% right_age_kids), 
       aes(x = character, fill = factor(responseCat,
                                        levels = c("normal",
                                                   "a little silly",
                                                   "really silly")))) +
  # aes(x = character, fill = factor(responseCat,
       #                                  levels = c("really silly",
       #                                             "a little silly",
       #                                             "normal")))) +
  facet_grid(. ~ predicate) +
  # geom_bar(position = "stack") + # for counts
  geom_bar(position = "fill") + # for proporitions
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  # scale_fill_hue(h.start = 120) +
  # scale_fill_brewer(type = "div", palette = 9) +
  # scale_fill_manual(values = c("#0571b0", "#92c5de", "#ca0020")) +
  labs(title = "Responses by predicate and character\n(a priori order)\n",
       x = "\nCharacter",
       # y = "Count of responses\n",
       y = "Proportion of responses\n",
       fill = "Response")

# counts by kid order (instead of a priori order)
# kid_order <- d_tidyr1 %>%
#   filter(phase == "test", subid %in% right_age_kids, responseCat == "normal") %>%
#   count(character) %>%
#   arrange(n) %>%
#   tibble::rownames_to_column(var = "kid_order") %>%
#   mutate(kid_order = as.numeric(kid_order))

kid_order <- d_tidyr1 %>%
  filter(phase == "test", subid %in% right_age_kids) %>%
  group_by(character) %>%
  summarise(mean = mean(responseNum, na.rm = T)) %>%
  arrange(desc(mean)) %>%
  tibble::rownames_to_column(var = "kid_order") %>%
  mutate(kid_order = as.numeric(kid_order))

ggplot(data = filter(d_tidyr1, subid %in% right_age_kids, phase == "test") %>%
         full_join(kid_order), 
       aes(x = reorder(character, kid_order), fill = responseCat)) +
  facet_grid(. ~ predicate) +
  # geom_bar(position = "stack") + # for counts
  geom_bar(position = "fill") + # for proporitions
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  labs(title = "Responses by predicate and character\n(child order)\n",
       x = "\nCharacter",
       # y = "Count of responses\n",
       y = "Proportion of responses\n",
       fill = "Response")

# kid order for each predicate separately

# kid_order_bypred <- d_tidyr1 %>%
#   filter(phase == "test", subid %in% right_age_kids) %>%
#   mutate(responseNum = responseNum + 1) %>% # get rid of 0s
#   group_by(predicate, character) %>%
#   summarise(weight = sum(responseNum, na.rm = T),
#             weight2 = mean(responseNum, na.rm = T)) %>%
#   # arrange(predicate, desc(weight)) %>%
#   arrange(predicate, desc(weight2)) %>%
#   mutate(kid_order = 1:12) %>%
#   ungroup()

kid_order_bypred <- d_tidyr1 %>%
  filter(phase == "test", subid %in% right_age_kids) %>%
  group_by(predicate, character) %>%
  summarise(mean = mean(responseNum, na.rm = T)) %>%
  arrange(predicate, desc(mean)) %>%
  mutate(kid_order = 1:12) %>%
  ungroup()

ggplot(data = filter(d_tidyr1, subid %in% right_age_kids, phase == "test") %>%
         full_join(kid_order_bypred), 
       aes(x = kid_order, fill = responseCat)) +
  facet_grid(. ~ predicate) +
  # geom_bar(position = "stack") + # for counts
  geom_bar(position = "fill") + # for proporitions
  theme_bw() +
  theme(text = element_text(size = 20),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = kara13qual) +
  geom_text(aes(label = character, 
                # y = 0), hjust = 0, # for counts 
            y = 1), hjust = 1, # for proportions
angle = 90, size = 6) +
  labs(title = "Responses by predicate and character\n(child order by predicate)\n",
       x = "\nCharacter",
       # y = "Count of responses\n",
       y = "Proportion of responses\n",
       fill = "Response")

# calculating percent normal
d_tidyr1 %>%
  filter(subid %in% right_age_kids, phase == "test") %>%
  group_by(predicate, character, responseCat) %>%
  summarise (n = n()) %>%
  mutate(freq = n / sum(n)) %>% 
  View()

# # --- POISSON ANALYSES: RUN 1 (July-August 2016) ------------------------------
# 
# set contrasts
# d_testr1 <- d_tidyr1 %>%
#   filter(phase == "test", age >= 4.5, age <= 5.5) %>%
#   mutate(character = factor(character,
#                             levels = c("stapler", "guitar", "car",
#                                        "computer", "cellphone", "robot",
#                                        "bug", "bear", "dog",
#                                        "baby", "kid", "grownup")),
#          predicate = factor(predicate,
#                             levels = c("hunger", "feelings", "thinking")),
#          predicate_wording = factor(predicate_wording,
#                                     levels = c("can get hungry",
#                                                "have feelings",
#                                                "can think")))

predicate_dummy <- cbind(F.v.H = c(0, 1, 0),
                         T.v.H = c(0, 0, 1))

predicate_contr <- cbind(FT.v.H = c(-2, 1, 1),
                         F.v.T = c(0, 1, -1))

predicate_effct <- cbind(H.v.UGM = c(1, -1, 0),
                         T.v.UGM = c(0, -1, 1))
# 
# # contrasts(d_testr1$predicate) = predicate_dummy
# contrasts(d_testr1$predicate) = predicate_contr
# # contrasts(d_testr1$predicate) = predicate_effct
# 
# r1 <- glm(responseNum ~ poly(characterNum, 3), 
#           data = filter(d_testr1, phase == "test", is.na(responseNum) == F), 
#           family = "poisson")
# summary(r1)
# 
# r2 <- glm(responseNum ~ poly(characterNum, 3) + predicate, 
#           data = filter(d_testr1, phase == "test", is.na(responseNum) == F), 
#           family = "poisson")
# summary(r2)
# 
# r3 <- glm(responseNum ~ poly(characterNum, 3) * predicate, 
#           data = filter(d_testr1, phase == "test", is.na(responseNum) == F), 
#           family = "poisson")
# summary(r3)
# 
# anova(r1, r2, r3, test = "Chisq")
# 
# r4 <- glm(responseNum ~ poly(characterNum, 3) + 
#             poly(characterNum, 3):predicate, 
#           data = filter(d_testr1, phase == "test", is.na(responseNum) == F), 
#           family = "poisson")
# summary(r4)
# 

# --- REGRESSION ANALYSES: RUN 1 (July-August 2016) ---------------------------

d_testr1_b <- d_tidyr1 %>%
  filter(phase == "test") %>%
  filter(subid %in% right_age_kids) %>%
  # filter(age >= 4.5, age <= 5.5) %>%
  left_join(kid_order_bypred) %>%
  mutate(kid_orderCAT = factor(kid_order),
         character = factor(character,
                            levels = c("stapler", "guitar", "car",
                                       "computer", "cellphone", "robot",
                                       "bug", "bear", "dog",
                                       "baby", "kid", "grownup")),
         predicate = factor(predicate,
                            levels = c("hunger", "feelings", "thinking")),
         predicate_wording = factor(predicate_wording,
                                    levels = c("can get hungry",
                                               "have feelings",
                                               "can think")))
           
# set contrasts

# see above for constructing contrasts
# contrasts(d_testr1_b$predicate) = predicate_dummy
contrasts(d_testr1_b$predicate) = predicate_contr
# contrasts(d_testr1_b$predicate) = predicate_effct

character_dummy <- contr.treatment(12)
character_poly <- contr.poly(12)

# contrasts(d_testr1_b$character) <- character_dummy
# contrasts(d_testr1_b$character) <- character_poly
contrasts(d_testr1_b$character, how.many = 3) <- character_poly

# contrasts(d_testr1_b$kid_orderCAT) <- character_dummy
# contrasts(d_testr1_b$kid_orderCAT) <- character_poly
contrasts(d_testr1_b$kid_orderCAT, how.many = 3) <- character_poly

library(ordinal)

# by character (a priori order)
r1 <- clmm(responseCat ~ character + (1 + predicate | subid),
           data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r1)

r2 <- clmm(responseCat ~ character + predicate + (1 + predicate | subid),
           data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r2)

r3 <- clmm(responseCat ~ character * predicate + (1 + predicate | subid),
           data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r3)

anova(r1, r2, r3)

r4 <- clmm(responseCat ~ character * predicate + (1 | subid),
           data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r4)


# by kid order by predicate
r1b <- clmm(responseCat ~ kid_orderCAT + (1 + predicate | subid),
           data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r1b)

r2b <- clmm(responseCat ~ kid_orderCAT + predicate + (1 + predicate | subid),
           data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F))
summary(r2b)

r3b <- clmm(responseCat ~ kid_orderCAT * predicate + (1 + predicate | subid),
            data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r3b)

anova(r1b, r2b, r3b)

r4b <- clmm(responseCat ~ kid_orderCAT * predicate + (1 | subid),
            data = filter(d_testr1_b, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids))
summary(r4b)

# NUMBER OF SILLY analysis
# by a priori order
d_testr1_c <- d_testr1_b %>%
  mutate(silly_log = ifelse(grepl("silly", responseCat), 1,
                            ifelse(grepl("normal", responseCat), 0,
                                   NA)))

r1c <- glmer(silly_log ~ character + (1 + predicate | subid),
            data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids),
            family = "binomial")
summary(r1c)

r2c <- glmer(silly_log ~ character + predicate + (1 + predicate | subid),
            data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F),
            family = "binomial")
summary(r2c)

r3c <- glmer(silly_log ~ character * predicate + (1 + predicate | subid),
            data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids),
            family = "binomial")
summary(r3c)

anova(r1c, r2c, r3c)

r4c <- glmer(silly_log ~ character * predicate + (1 | subid),
            data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids),
            family = "binomial")
summary(r4c)

# by kid order
d_testr1_c <- d_testr1_b %>%
  mutate(silly_log = ifelse(grepl("silly", responseCat), 1,
                            ifelse(grepl("normal", responseCat), 0,
                                   NA)))

r1d <- glmer(silly_log ~ kid_orderCAT + (1 + predicate | subid),
             data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids),
             family = "binomial")
summary(r1d)

r2d <- glmer(silly_log ~ kid_orderCAT + predicate + (1 + predicate | subid),
             data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F),
             family = "binomial")
summary(r2d)

r3d <- glmer(silly_log ~ kid_orderCAT * predicate + (1 + predicate | subid),
             data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids),
             family = "binomial")
summary(r3d)

anova(r1d, r2d, r3d)

r4d <- glmer(silly_log ~ kid_orderCAT * predicate + (1 | subid),
             data = filter(d_testr1_c, phase == "test", is.na(responseNum) == F, subid %in% right_age_kids),
             family = "binomial")
summary(r4d)

