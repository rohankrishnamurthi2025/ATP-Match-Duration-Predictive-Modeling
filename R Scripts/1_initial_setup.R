# Initial data checks, data splitting, & data folding
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)

# handle common conflicts
tidymodels_prefer()

#Load Files
atp_matches <- read_rds("cleaned data/atp_matches.rds")

##Initial split of data
##Set seed for random split
set.seed(111)
atp_split <- atp_matches |> 
  initial_split(prop = 0.8, strata = minutes)

atp_train <- atp_split |> training()
atp_test <- atp_split |> testing()

##Write out split, train, and test data
write_rds(atp_split, file = here("cleaned data/atp_split.rds"))
write_rds(atp_train, file = here("cleaned data/atp_train.rds"))
write_rds(atp_test, file = here("cleaned data/atp_test.rds"))

##Use V-fold cross-validation (v = 5 folds, 3 repeats)
set.seed(111)
atp_folds <- vfold_cv(atp_train, v = 5, repeats = 3,
                      strata = minutes)

# set up controls for fitted resamples ----
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)
keep_wflow <- control_resamples(save_workflow = TRUE)

##Write out folds
write_rds(atp_folds, file = here("cleaned data/atp_folds.rds"))
write_rds(keep_pred, file = here("cleaned data/keep_pred.rds"))
write_rds(keep_wflow, file = here("cleaned data/keep_wflow.rds"))


