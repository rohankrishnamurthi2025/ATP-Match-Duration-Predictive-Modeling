#4: analysis of models
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
atp_test <- read_csv("cleaned data/atp_test.rds")

#Load Models
load(here("results/bt_tuned.rda"))
load(here("results/rf_tuned.rda"))
load(here("results/knn_tuned_default.rda"))
load(here("results/knn_tuned_interact.rda"))
load(here("results/en_tuned_default.rda"))
load(here("results/en_tuned_interact.rda"))
load(here("results/lm_fit_default.rda"))
load(here("results/lm_fit_interact.rda"))
load(here("results/null_fit_default.rda"))
load(here("results/null_fit_interact.rda"))


#Collect metrics
null_metrics_default <- collect_metrics(null_fit_default) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "null, default") |> 
  rename(mean_rmse = mean)

null_metrics_interact <- collect_metrics(null_fit_interact) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "null, interact") |> 
  rename(mean_rmse = mean)

lm_metrics_default <- collect_metrics(lm_fit_default) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "lm, default") |> 
  rename(mean_rmse = mean)

lm_metrics_interact <- collect_metrics(lm_fit_interact) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "lm, interact") |> 
  rename(mean_rmse = mean)

en_metrics_default <- collect_metrics(en_tuned_default) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "elastic net, default") |> 
  rename(mean_rmse = mean)

en_metrics_interact <- collect_metrics(en_tuned_interact) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "elastic net, interact") |> 
  rename(mean_rmse = mean)

knn_metrics_default <- collect_metrics(knn_tuned_default) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "knn, default") |> 
  rename(mean_rmse = mean)

knn_metrics_interact <- collect_metrics(knn_tuned_interact) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "knn, interact") |> 
  rename(mean_rmse = mean)

bt_metrics <- collect_metrics(bt_tuned) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "boosted tree") |> 
  rename(mean_rmse = mean)

rf_metrics <- collect_metrics(rf_tuned) |> 
  filter(.metric == "rmse") |> 
  mutate(.config = "random forest") |> 
  rename(mean_rmse = mean)


#Organize metrics into table
metrics_table <- null_metrics_default |>
  filter(mean_rmse == min(mean_rmse)) |> 
  mutate(model_type = "null", recipe = "default") |> 
  bind_rows(
    null_metrics_interact |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "null", recipe = "interact")
  ) |> 
  bind_rows(
    lm_metrics_default |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "linear regression", recipe = "default")
  ) |> 
  bind_rows(
    lm_metrics_interact |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "linear regression", recipe = "interact")
  ) |> 
  bind_rows(
    en_metrics_default |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "elastic net", recipe = "default")
  ) |> 
  bind_rows(
    en_metrics_interact |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "elastic net", recipe = "interact")
  ) |> 
  bind_rows(
    knn_metrics_default |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "k-nearest neighbor", recipe = "default")
  ) |> 
  bind_rows(
    knn_metrics_interact |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "k-nearest neighbor", recipe = "interact")
  ) |> 
  bind_rows(
    bt_metrics |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "boosted tree", recipe = "random forest")
  ) |> 
  bind_rows(
    rf_metrics |>
      filter(mean_rmse == min(mean_rmse)) |> 
      mutate(model_type = "random forest", recipe = "random forest")
  ) 

 
#select(model_type, mean, std_err) |> knitr::kable()

metrics_table

save(metrics_table, file = "tables/metrics_table.rda")


#TIE BREAKER
en_metrics_interact <- collect_metrics(en_tuned_interact) |> 
  mutate(.config = "elastic net, interact")
en_metrics_interact

en_table <- en_metrics_interact |>
  filter(std_err == min(std_err)) |> 
  mutate(model_type = "null", recipe = "interact") 

en_table |> kableExtra::kable()

save(en_table, file = "tables/en_interact_table.rda")
