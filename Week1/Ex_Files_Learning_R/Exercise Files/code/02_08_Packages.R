# File:     02_08_Packages.R
# Project:  LearningR

# INSTALL AND LOAD PACKAGES ################################

# pacman must already be installed; then load contributed
# packages (including pacman) with pacman
pacman::p_load(pacman, rio, tidyverse)

# LOAD AND PREPARE DATA ####################################

df <- import("data/StateData.xlsx") %>%
  as_tibble() %>%
  select(state_code, 
    psychRegions,
    instagram:modernDance) %>% 
  mutate(psychRegions = as.factor(psychRegions)) %>%
  # rename(y = psychRegions) %>%
  print()

# FILTER BY CATEGORY #######################################

# States that are "Relaxed and Creative" on psychRegions

# Base R: use square brackets
df$state_code[df$psychRegions == "Relaxed and Creative"]

# Tidyverse: use "filter"
df %>%
  filter(psychRegions == "Relaxed and Creative") %>%
  select(state_code)

# FILTER BY COMPARISON #####################################

# States that are above median on searches for volunteering

# Above value
df %>%
  filter(volunteering > 1) %>%
  select(state_code) %>%
  print()

# Above median
df %>%
  filter(volunteering > median(volunteering)) %>%
  select(state_code) %>%
  print(n = Inf)  # Show all rows (not just 10)

# Compare scores on two variables
df %>%
  filter(instagram > facebook) %>%
  select(state_code) %>%
  print(n = Inf)

# Reverse comparison
df %>%
  filter(facebook > instagram) %>%
  select(state_code) %>%
  print(n = Inf)  # Show all rows (not just 10)

# FILTER BY TWO VARIABLES ##################################

# Filter by two conditions: "or"
df %>%
  filter(instagram > 1 | facebook > 1) %>%
  select(state_code, psychRegions)

# Filter by two conditions: "and"
df %>%
  filter(instagram > 1 & facebook > 1) %>%
  select(state_code, psychRegions)

# Filter by two conditions: "not"
df %>%
  filter(instagram > 1 & 
      !psychRegions == "Temperamental and Uninhibited") %>%
  select(state_code, psychRegions)

# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
