#2: Recipe development

rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)

# handle common conflicts
tidymodels_prefer()

#Load training data
atp_train <- read_rds("cleaned data/atp_train.rds")

#Default recipe
atp_recipe_default <- recipe(minutes ~ surface + tourney_level + winner_ht +
                               winner_age + loser_ht + loser_age + w_ace + w_df +
                               l_ace + l_df + winner_rank_points + loser_rank_points +
                               service_games + service_points + l_1stIn + l_1stWon +
                               l_2ndWon + w_1stIn + w_1stWon + w_2ndWon,
                             data = atp_train) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  step_center(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# Check recipe
atp_recipe_default |> 
  prep() |> 
  bake(new_data = NULL) |> 
  glimpse()

#Interact recipe
atp_recipe_interact <- recipe(minutes ~ surface + tourney_level + winner_ht +
                               winner_age + loser_ht + loser_age + w_ace + w_df +
                               l_ace + l_df + winner_rank_points + loser_rank_points +
                               service_games + service_points + l_1stIn + l_1stWon +
                                l_2ndWon + w_1stIn + w_1stWon + w_2ndWon,
                             data = atp_train) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  step_interact( ~ service_games:service_points ) |> 
  step_center(all_predictors()) |>
  step_normalize(all_numeric_predictors())
  

#Random Forest Recipe
atp_recipe_rf <- recipe(minutes ~ surface + tourney_level + winner_ht +
                          winner_age + loser_ht + loser_age + w_ace + w_df +
                          l_ace + l_df + winner_rank_points + loser_rank_points +
                          service_games + service_points + l_1stIn + l_1stWon +
                          l_2ndWon + w_1stIn + w_1stWon + w_2ndWon,
                        data = atp_train) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |> 
  step_center(all_predictors()) |> 
  step_normalize(all_numeric_predictors())


## Write out recipes
save(atp_recipe_default, file = here("recipes/atp_recipe_default.rda"))
save(atp_recipe_interact, file = here("recipes/atp_recipe_interact.rda"))
save(atp_recipe_rf, file = here("recipes/atp_recipe_rf.rda"))

#Interaction Images
atp_train_na <- atp_train |> drop_na()

# w_seed_rank <- atp_train_na |> ggplot(aes(x = winner_seed, y = winner_rank_points)) + 
#   geom_point() + geom_smooth() +
#   labs(title = "Scatterplot of Winner Seed and Ranking Points")
# w_seed_rank
# 
# l_seed_rank <- atp_train_na |> ggplot(aes(x = loser_seed, y = loser_rank_points)) + 
#   geom_point() + geom_smooth() + 
#   labs(title = "Scatterplot of Loser Seed and Ranking Points")
# l_seed_rank

points_games <- atp_train_na |> ggplot(aes(x = service_points, y = service_games)) + 
  geom_point() + geom_smooth() + 
  labs(title = "Scatterplot of Service Points and Service Games")
points_games

#Save Images
# ggsave(w_seed_rank, file = here("figures/w_seed_rank.png"))
# ggsave(l_seed_rank, file = here("figures/l_seed_rank.png"))
ggsave(points_games, file = here("figures/points_games.png"))


