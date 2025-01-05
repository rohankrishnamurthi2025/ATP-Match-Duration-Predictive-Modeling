# EDA of response variable
rm(list = ls(all.names = TRUE))
# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(janitor)
library(parsnip)
library(reshape2)

# handle common conflicts
tidymodels_prefer()

#Load Files
atp_matches <- read_rds("cleaned data/atp_matches.rds")
atp_train <- read_rds("cleaned data/atp_train.rds")

#EDA
#A: Minutes Distribution
minutes_histogram <- atp_train |> ggplot(aes(x = minutes)) + geom_histogram() +
  labs(title = "Number of minutes of ATP Matches, 2017-2022")
ggsave(filename = "figures/minutes_histogram.png", plot = minutes_histogram)

atp_matches$minutes |> summary()

#B: Correlation Matrix
atp_train_na <- atp_train %>% 
  drop_na()

subset_data_atp <- atp_train_na |> select("draw_size", "winner_ht", "winner_seed", "winner_age",
                                          "loser_ht", "loser_seed", "loser_age", "minutes",
                                          "w_ace", "w_df", "l_ace", "l_df", "winner_rank_points",
                                          "loser_rank_points", "service_games", "service_points")

atp_correlation_matrix <- cor(subset_data_atp)
#save(atp_correlation_matrix, file = here("tables/atp_correlation_matrix"))

# Convert correlation matrix to long format 
atp_correlation_matrix_long <- melt(atp_correlation_matrix)
# heatmap 
cor_pic <- ggplot(atp_correlation_matrix_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),  
        axis.text.y = element_text(size = 8)) +
  labs(x = NULL, y = NULL)

ggsave(cor_pic, file = here("figures/cor_matrix.png"))

#C: Qualitative Analysis
level_duration_boxplot <- atp_train |> ggplot(aes(x = tourney_level, y = minutes)) +
  geom_boxplot() +
  labs(title = "Duration of ATP Matches by tournament level, 2017-2022",
       xlab = "Tournament Level", ylab = "Minutes")
ggsave(level_duration_boxplot, file = here("figures/level_duration_boxplot.png"))

surface_duration_boxplot <- atp_train |> ggplot(aes(x = surface, y = minutes)) +
  geom_boxplot() +
  labs(title = "Duration of ATP Matches by surface, 2017-2022",
       xlab = "Surface", ylab = "Minutes")

ggsave(surface_duration_boxplot, file = here("figures/surface_duration_boxplot.png"))

