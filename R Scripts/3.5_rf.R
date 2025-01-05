#3f: random forest model
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)

# handle common conflicts
tidymodels_prefer()

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
# create a cluster object and then register:
cl <- makePSOCKcluster(num_cores - 1)
registerDoParallel(cl)

#Load relevant data
atp_train <- read_rds("cleaned data/atp_train.rds")
atp_folds <- read_rds("cleaned data/atp_folds.rds")
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
keep_wflow <- control_resamples(save_workflow = TRUE)

#Load recipes
load("recipes/atp_recipe_default.rda")
load("recipes/atp_recipe_interact.rda")
load("recipes/atp_recipe_rf.rda")


#Complete Cases
atp_train_c <- atp_train |> select(-winner_seed, -winner_entry, -loser_seed, -loser_entry)
atp_train_c <- atp_train_c[complete.cases(atp_train_c), ]
atp_train_c <- atp_train_c[1:3000, ]
atp_folds <- vfold_cv(atp_train_c, v = 5, repeats = 3,
                      strata = minutes)

#Defining of model specifications and workflow

# model specifications ----
rf_spec <- rand_forest(
  trees = tune(), min_n = tune(), mtry = tune()) |> 
  set_mode("regression") |> 
  set_engine("ranger")

# define workflows ----
rf_wflow <- workflow() |> 
  add_model(rf_spec) |> 
  add_recipe(atp_recipe_rf)

# hyperparameter tuning values ----
#check rangers for hyperparameters
hardhat::extract_parameter_set_dials(rf_spec)

# change hyperparameter ranges
rf_params <- hardhat::extract_parameter_set_dials(rf_spec) |> 
  update(mtry = mtry(c(1, 10)))

# build tuning grid
rf_grid <- grid_regular(rf_params, levels = 5)

#25 grid points: 5 by 5 (25 models)

# fit workflows/models ----
set.seed(111)

rf_tuned <- rf_wflow |> 
  tune_grid(atp_folds, grid = rf_grid, control = control_grid(save_workflow = TRUE))

# write out results (fitted/trained workflows) ----
save(rf_tuned, file = here('results/rf_tuned.rda'))

