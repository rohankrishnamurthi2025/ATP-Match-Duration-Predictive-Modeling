#3e: k-nearest neighbor regression model
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)
library(doMC)

# handle common conflicts
tidymodels_prefer()

#Load relevant data
atp_train <- read_rds("cleaned data/atp_train.rds")
atp_folds <- read_rds("cleaned data/atp_folds.rds")
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
keep_wflow <- control_resamples(save_workflow = TRUE)

#Complete Cases
atp_train_c <- atp_train |> select(-winner_seed, -winner_entry, -loser_seed, -loser_entry)
atp_train_c <- atp_train_c[complete.cases(atp_train_c), ]
atp_folds <- vfold_cv(atp_train_c, v = 5, repeats = 3,
                      strata = minutes)


#Load recipes
load("recipes/atp_recipe_default.rda")
load("recipes/atp_recipe_interact.rda")
load("recipes/atp_recipe_rf.rda")

# parallel processing
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# model specifications ----
knn_spec <- nearest_neighbor(neighbors = tune()) %>% 
  set_engine("kknn") |> 
  set_mode("regression")

# define workflows ----
knn_wflow_default <- workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(atp_recipe_default)

knn_wflow_interact <- workflow() |> 
  add_model(knn_spec) |> 
  add_recipe(atp_recipe_interact)

# hyperparameter tuning values ----
# Check rangers for hyperparameters
hardhat::extract_parameter_set_dials(knn_spec)

# Change hyperparameter ranges
knn_params <- hardhat::extract_parameter_set_dials(knn_spec)

# Build tuning grid
knn_grid <- grid_regular(knn_params, levels = 5)

# fit workflows/models ----
set.seed(111)
knn_tuned_default <- knn_wflow_default |> 
  tune_grid(atp_folds, grid = knn_grid, control = control_grid(save_workflow = TRUE))

knn_tuned_interact <- knn_wflow_interact |> 
  tune_grid(atp_folds, grid = knn_grid, control = control_grid(save_workflow = TRUE))


# write out results (fitted/trained workflows) ----
save(knn_tuned_default, file = here('results/knn_tuned_default.rda'))
save(knn_tuned_interact, file = here('results/knn_tuned_interact.rda'))



