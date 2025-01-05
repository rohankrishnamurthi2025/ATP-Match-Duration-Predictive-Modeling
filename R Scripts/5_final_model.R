#5: analysis of final model
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)
library(kableExtra)

# handle common conflicts
tidymodels_prefer()


#REDO
#Load Data
atp_train <- read_rds("cleaned data/atp_train.rds")
atp_test <- read_rds("cleaned data/atp_test.rds")

atp_train_c <- atp_train |> select(-winner_seed, -winner_entry, -loser_seed, -loser_entry)
atp_train_c <- atp_train_c[complete.cases(atp_train_c), ]

atp_test_c <- atp_test |> select(-winner_seed, -winner_entry, -loser_seed, -loser_entry)
atp_test_c <- atp_test_c[complete.cases(atp_test_c), ]

atp_folds <- vfold_cv(atp_train_c, v = 5, repeats = 3,
                      strata = minutes)
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

#Load Models and recipes
load(here("results/en_tuned_default.rda"))
load(here("results/en_tuned_interact.rda"))
load("recipes/atp_recipe_default.rda")
load("recipes/atp_recipe_interact.rda")

#Train Elastic Net Model, Interact Recipe, with best parameters

#Defining of model specifications
en_spec <-  linear_reg(
  penalty = 0, mixture = 0.2875) |> 
  set_engine("glmnet") |>  
  set_mode("regression")

#Define workflows, model, fitting

en_wflow_final <- workflow() |> 
  add_model(en_spec) |> 
  add_recipe(atp_recipe_interact)

en_interact_final <-
  en_wflow_final |> 
  fit_resamples(resamples = atp_folds, control = keep_pred)

final_wflow <- en_interact_final |> 
  extract_workflow(en_interact_final) |> 
  finalize_workflow(select_best(en_interact_final, metric = "rmse"))

#Save model
#save(final_wflow, file = here("results/final_model.rda"))

# train final model ----
# set seed
set.seed(123)
final_fit <- fit(final_wflow, atp_train_c)

#Finalize Predictions
final_predictions <- atp_test_c %>% 
  select(minutes) %>% 
  bind_cols(predict(final_fit, atp_test_c))

final_predictions <- final_predictions |> 
  mutate(difference = abs(minutes - .pred))

final_predictions
nrow(final_predictions)
#Assess model performance
get_metrics <- metric_set(rmse, rsq, mape, mae)
pred_metrics <- as.data.frame(get_metrics(final_predictions, truth = minutes, estimate = .pred)) 
pred_metrics


#Plot Results
minutes_plot <- final_predictions |> ggplot(aes(x = minutes, y = .pred)) +
  geom_abline(lty = 2, color = "blue") + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Duration (Minutes)", x = "Actual Duration (Minutes)")

minutes_plot

#Save Results
save(final_predictions, file = "tables/final_predictions.rda")
save(pred_metrics, file = "tables/pred_metrics.rda")
ggsave(minutes_plot, file = here("figures/minutes_plot.png"))



