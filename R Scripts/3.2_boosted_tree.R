#3.2: boosted tree model
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
install.packages("xgboost")
library(xgboost)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# parallel processing
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

#Load relevant data
atp_train <- read_rds("cleaned data/atp_train.rds")
atp_folds <- read_rds("cleaned data/atp_folds.rds")
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
keep_wflow <- control_resamples(save_workflow = TRUE)

atp_train <- atp_train |> select(-winner_seed, -winner_entry, -loser_seed, -loser_entry)

#Load recipes
load("recipes/atp_recipe_default.rda")
load("recipes/atp_recipe_interact.rda")
load("recipes/atp_recipe_rf.rda")

# model specifications ----
bt_spec <- boost_tree(
  min_n = tune(),
  mtry = tune(),
  learn_rate = tune()
) |> 
  set_mode("regression") |> 
  set_engine("xgboost")

# define workflows ----
bt_wflow <- workflow() |> 
  add_model(bt_spec) |> 
  add_recipe(atp_recipe_rf)

# hyperparameter tuning values ----
# Check rangers for hyperparameters
hardhat::extract_parameter_set_dials(bt_spec)

# Change hyperparameter ranges
bt_params <- hardhat::extract_parameter_set_dials(bt_spec) |> 
  update(mtry = mtry(c(1,10)), learn_rate = learn_rate(c(-5, -0.2)))

# Build tuning grid
bt_grid <- grid_regular(bt_params, levels = 5)

# fit workflows/models ----
set.seed(111)
bt_tuned <- bt_wflow |> 
  tune_grid(atp_folds, grid = bt_grid, control = control_grid(save_workflow = TRUE))


# write out results (fitted/trained workflows) ----
save(bt_tuned, file = here("results/bt_tuned.rda"))



