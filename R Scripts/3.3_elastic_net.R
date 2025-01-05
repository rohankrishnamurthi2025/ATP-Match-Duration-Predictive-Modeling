#3.3: Elastic Net model
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)
library(doParallel)

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

# parallel processing ----
num_cores <- parallel::detectCores(logical = TRUE)
# create a cluster object and then register:
cl <- makePSOCKcluster(num_cores - 1)
registerDoParallel(cl)


#Defining of model specifications
en_spec <-  linear_reg(
  penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet") |>  
  set_mode("regression")

#Define workflows
en_wflow_default <- workflow() |> 
  add_model(en_spec) |> 
  add_recipe(atp_recipe_default)

en_wflow_interact <- workflow() |> 
  add_model(en_spec) |> 
  add_recipe(atp_recipe_interact)

# hyperparameter tuning values ----
en_params <- hardhat::extract_parameter_set_dials(en_spec)
 
# Build tuning grid
en_grid <- grid_regular(en_params, levels = 5)

# fit workflows/models ----
set.seed(111)

en_tuned_default <- en_wflow_default |> 
  tune_grid(atp_folds, grid = en_grid, 
            control = control_grid(save_workflow = TRUE))

en_tuned_interact <- en_wflow_interact |> 
  tune_grid(atp_folds, grid = en_grid, 
            control = control_grid(save_workflow = TRUE))

# stopping parallel processing
stopCluster(cl)


# write out results (fitted/trained workflows) ----
save(en_tuned_default, file = here("results/en_tuned_default.rda"))
save(en_tuned_interact, file = here("results/en_tuned_interact.rda"))


