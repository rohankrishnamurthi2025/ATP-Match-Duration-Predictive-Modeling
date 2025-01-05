#3b: fit of linear regression model
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)

# handle common conflicts
tidymodels_prefer()

#Load relevant data
atp_train <- read_rds("cleaned data/atp_train.rds")
atp_folds <- read_rds("cleaned data/atp_folds.rds")
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
keep_wflow <- control_resamples(save_workflow = TRUE)

#Load recipes
load("recipes/atp_recipe_default.rda")
load("recipes/atp_recipe_interact.rda")

#Defining of model specifications and workflow
lm_spec <- linear_reg() %>% 
  set_engine("lm") |> 
  set_mode("regression")

lm_wflow_default <- workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(atp_recipe_default)

lm_wflow_interact <- workflow() |> 
  add_model(lm_spec) |> 
  add_recipe(atp_recipe_interact)

#fit workflows/models
lm_fit_default <-
  lm_wflow_default |> 
  fit_resamples(resamples = atp_folds, control = keep_pred)

lm_fit_interact <- 
  lm_wflow_interact |> 
  fit_resamples(resamples = atp_folds, control = keep_pred)


#lm_fit_train <- fit(lm_wflow, wta_2023_train)

# write out fitted results
save(lm_fit_default, file = here("results/lm_fit_default.rda"))
save(lm_fit_interact, file = here("results/lm_fit_interact.rda"))


#save(lm_fit_train, file = here("results/lm_fit_train.rda"))


