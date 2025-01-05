#Data Cleaning
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)
install.packages("readxl")
library(readxl)

# handle common conflicts
tidymodels_prefer()


#Read in data
atp_matches_raw <- read.csv("data/atp_matches_raw.csv")

#Cleaning of data
atp_matches <- atp_matches_raw |> 
  rename("Tournament" = "tourney_name") |> 
  mutate(surface = as.factor(surface),
         round = as.factor(round),
         winner_hand = as.factor(winner_hand), 
         loser_hand = as.factor(loser_hand),
         winner_entry = as.factor(winner_entry),
         loser_entry = as.factor(loser_entry)) |>
  mutate(service_games = w_SvGms + l_SvGms,
         service_points = w_svpt + l_svpt) |> 
  mutate(tourney_level = fct_recode(tourney_level,
                                    "Other" = "A",
                                    "Grand Slam" = "G",
                                    "Davis Cup" = "D",
                                    "Masters" = "M",
                                    "Tour Finals" = "F")) |> 
  mutate(tourney_y = substring(tourney_date, 1, 4),
         tourney_m = substring(tourney_date, 5, 6),
         tourney_d = substring(tourney_date, 7, 8)) |> 
  select(-"winner_id", -"loser_id", -"tourney_id")

#df_clean <- df[complete.cases(df$Name), ]
atp_matches <- atp_matches[complete.cases(atp_matches$minutes), ]

#Save data
write_rds(atp_matches, file = here("cleaned data/atp_matches.rds"))


