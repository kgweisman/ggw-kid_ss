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

# --- READING IN DATA ---------------------------------------------------------

# Pilot
pilot_files <- dir("ggw-kid_ss/pilot data/individual sessions")
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

# --- WRITING ANONYMIZED CSV --------------------------------------------------

# write to de-identified csv file
write.csv(d, "/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot data/pilot_data_2016-03-23.csv")

d <- read.csv("/Users/kweisman/Documents/Research (Stanford)/Projects/GGW-kid_ss/ggw-kid_ss/pilot data/pilot_data_2016-03-23.csv")[-1] # get rid of column of obs numbers

