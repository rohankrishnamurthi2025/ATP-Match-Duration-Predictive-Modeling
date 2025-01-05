#3a: fit of null model
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

#Null model specifications
null_model <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("regression") |> 
  translate()

# define workflow, default recipe
null_wflow_default <- workflow() |> 
  add_model(null_model) |> 
  add_recipe(atp_recipe_default)

#fit workflows/models
null_fit_default <-
  null_wflow_default |> 
  fit_resamples(resamples = atp_folds, control = keep_pred)

# define workflow, interact recipe
null_wflow_interact <- workflow() |> 
  add_model(null_model) |> 
  add_recipe(atp_recipe_interact)

#fit workflows/models
null_fit_interact <-
  null_wflow_interact |> 
  fit_resamples(resamples = atp_folds, control = keep_pred)

#null_fit_train <- fit(null_wflow, atp_train)

# write out fitted workflows
save(null_fit_default, file = here("results/null_fit_default.rda"))
save(null_fit_interact, file = here("results/null_fit_interact.rda"))
#save(null_fit_train, file = here("results/null_fit_train.rda"))
