# --- PRELIMINARIES -----------------------------------------------------------

# libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(stats)
library(stringr)

# clear environment
rm(list=ls())
setwd("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/")

# --- READING IN DATA: PILOT 1 (March 2016)------------------------------------

# Pilot
pilot_files <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot data/individual sessions")
wd <- getwd()

d <- data.frame()
for(i in 1:length(pilot_files)) {
  temp <- read.csv(paste0(wd, "/ggw-kid_ss/pilot data/individual sessions/", 
                          pilot_files[i])) %>%
    mutate_each(funs(factor))
  d <- bind_rows(d, temp) %>%
    mutate_each(funs(factor)) %>%
    mutate(trial = as.numeric(as.character(trial)),
           response = ifelse(response == "NA", NA,
                             as.numeric(as.character(response))))
}

# --- WRITING ANONYMIZED CSV: PILOT 1 (March 2016) ----------------------------

# write to de-identified csv file
write.csv(d, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot data/pilot_data_2016-03-23.csv")

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot data/pilot_data_2016-03-23.csv")[-1] # get rid of column of obs numbers

# --- READING IN DATA: PILOT 2 (May 2016) -------------------------------------

# Pilot
pilot2_files <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot2 data/individual sessions")
wd <- getwd()

d2 <- data.frame()
for(i in 1:length(pilot2_files)) {
  temp <- read.csv(paste0(wd, "/ggw-kid_ss/pilot2 data/individual sessions/", 
                          pilot2_files[i])) %>%
    mutate_each(funs(factor))
  d2 <- bind_rows(d2, temp) %>%
    mutate_each(funs(factor)) %>%
    mutate(trial = as.numeric(as.character(trial)),
           response = factor(ifelse(response == "NA", NA, as.character(response))))
}

# --- WRITING ANONYMIZED CSV: PILOT 2 -----------------------------------------

# write to de-identified csv file
write.csv(d2, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot2 data/pilot2_data_2016-06-10.csv")

d2 <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot2 data/pilot2_data_2016-06-10.csv")[-1] # get rid of column of obs numbers

# --- READING IN DATA: PILOT 3 (June-July 2016) -------------------------------

# Pilot
pilot3_files <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot3 data/individual sessions")
wd <- getwd()

d3 <- data.frame()
for(i in 1:length(pilot3_files)) {
  temp <- read.csv(paste0(wd, "/ggw-kid_ss/pilot3 data/individual sessions/", 
                          pilot3_files[i])) %>%
    mutate_each(funs(factor))
  d3 <- bind_rows(d3, temp) %>%
    mutate_each(funs(factor))
}

d3a <- d3 %>%
  filter(!is.na(sequence)) %>%
  mutate(trialNum = as.numeric(trial))

# --- WRITING ANONYMIZED CSV: PILOT 3 -----------------------------------------

# write to de-identified csv file
write.csv(d3a, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot3 data/pilot3_data_2016-07-17.csv")

d3a <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot3 data/pilot3_data_2016-07-17.csv")[-1] # get rid of column of obs numbers

# --- READING IN DATA: RUN 1 (July-August 2016) -------------------------------

# Pilot
run1_files <- dir("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/run1 data/individual sessions")
wd <- getwd()

dr1 <- data.frame()
for(i in 1:length(run1_files)) {
  temp <- read.csv(paste0(wd, "/ggw-kid_ss/run1 data/individual sessions/", 
                          run1_files[i])) %>%
    mutate_each(funs(factor))
  dr1 <- bind_rows(dr1, temp) %>%
    mutate_each(funs(factor))
}

dr1a <- dr1 %>%
  # filter(!is.na(sequence)) %>%
  mutate(trialNum = as.numeric(trial))

# --- WRITING ANONYMIZED CSV: PILOT 3 -----------------------------------------

# write to de-identified csv file
write.csv(dr1a, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/run1 data/run1_data_2016-07-30.csv")

dr1a <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/run1 data/run1_data_2016-07-30.csv")[-1] # get rid of column of obs numbers
