# Load packages
library(tidyverse)
library(readxl)

# Load data
cbb <- read.csv("CollegeBasketballPlayers2009-2021.csv")
nba <- read_excel("DraftedPlayers2009-2021.xlsx")


str(cbb)

# Join CBB and draft data, fix height stupidity
cbb_join <- cbb %>% 
  left_join(select(nba, PLAYER, TEAM), by = c("player_name" = "PLAYER")) %>% 
  mutate(drafted = case_when(is.na(TEAM) ~ "No",
                             TRUE ~ "Yes")) %>%
  mutate(ht2 = ht) %>% 
  separate(ht2, sep = "-", into = c("ht_in", "ht_ft")) %>% 
  mutate(ht_ft = case_when(ht_ft == "May" ~ 5L,
                           ht_ft == "Jun" ~ 6L,
                           ht_ft == "Jul" ~ 7L,
                           TRUE ~ NA_integer_),
         ht_in = as.integer(ht_in),
         ht_total = ht_ft*12 + ht_in) %>% 
  mutate(ht_rev = case_when(ht == "Jun-00" ~ 72L,
                            TRUE ~ ht_total))

# Create scatterplot of pairs of variables to see correlations
cbb_join %>% 
  filter(Min_per > 5) %>%
  # Remove comment from the line below to switch between only drafted players and all CBB
  #filter(drafted == "Yes") %>% 
  ggplot(aes(x = ht_rev, y = adjoe)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlim(c(60, 90)) #+
  #ylim(c(0, 100))

# Linear regression equations relating variables in...

  # Full CBB group
  summary(lm(data = filter(cbb_join, Min_per > 5), formula = adjoe ~ ht_rev))

  # Drafted group
  summary(lm(data = filter(cbb_join, Min_per > 5, drafted == "Yes"), formula = adjoe ~ ht_rev))

