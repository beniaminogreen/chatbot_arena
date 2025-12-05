# STAN modelling of LLM-Arena Data
#
# Ben Green and Ivan Siynavin 
#
# This script performs the modelling to predict outcomes of the LLM-Arena matches using a set of custom models written in STAN. 
#
# The script expects the stan models and parquet files to be all located in the same directory, as shown below: 
#
#```
#├── basic_model_2.stan
#├── basic_model.stan
#├── calibration_plots/
#├── common_faliure_prob.stan
#├── derail_model.stan
#├── model_both.stan
#├── model_with_ties.stan
#├── train-00000-of-00007.parquet
#├── train-00001-of-00007.parquet
#├── train-00002-of-00007.parquet
#├── train-00003-of-00007.parquet
#├── train-00004-of-00007.parquet
#├── train-00005-of-00007.parquet
#└── train-00006-of-00007.parquet
#```
#
# Outputs are written to the `calibration_plots` directory, as well as to `predictions.csv` 

library(arrow)
library(tidyverse)
library(rstan)
library(patchwork)

source("functions.R")

####### Jay's Code from here ...
# Multiple file read & combine...

files <- dir(pattern = "parquet")
z <- list()
for (i in 1:length(files)) {
  # modification I made: this is faster than read_paruqet becuase 
  # it does not require pulling every column into memory
  x <- open_dataset(files[i]) %>% 
    select(id, model_a, model_b, winner) %>% 
    collect()
  z[[i]] <- x
}
x <- do.call(rbind, z)
saveRDS(x, file = 'chatbot.data.rds')

rm(list = ls())
x <- readRDS(file = 'chatbot.data.rds')

set.seed(1)
numextras <- 100

prediction_set <-x[sample(nrow(x), numextras),]
prediction_set$winner <- NA

df <- x

############################ to here

df <- df %>% 
  group_by(model_a, model_b, winner) %>%
  summarize(n = n()) %>%
  collect() %>%
  drop_na()

model_names <- unique(c(df$model_a, df$model_b))
n_models <- length(model_names)
model_lookup <- 1:n_models
names(model_lookup) <- model_names

stan_df <- df %>%
  mutate(
    model_a = model_lookup[model_a],
    model_b = model_lookup[model_b],
    winner = case_match(
      as.character(winner),
      "model_a" ~ 3,
      "tie" ~ 2,
      "model_b" ~ 1,
      "both_bad" ~ 99,
    ),
    both_bad = as.integer(winner == 99)
  )

stan_data <- list(
  N_models = n_models,
  N = nrow(stan_df),
  model_a = stan_df$model_a,
  model_b = stan_df$model_b,
  winner = stan_df$winner,
  both_bad = stan_df$both_bad,
  num = stan_df$n,
  N_test = nrow(prediction_set), 
  validation_model_a = model_lookup[prediction_set$model_a],
  validation_model_b = model_lookup[prediction_set$model_b]
)



#' Create a Dataframe with the predicted and empirical probabilities 
#' of each outcome by dyad
#'
#' @param model_file the location of the stan model used to generate
#' predicted probabilities
#'
#' @param stan_data a list or environment containing the data needed 
#' for STAN to fit the model
#' 
#'
create_matchup_df <- function(model_file, stan_data) {
  model_object <- stan_model(model_file)

  MAP <- optimizing(
    model_object, 
    data = stan_data
  )

  prediction_vector <- MAP$par[grepl("preds", names(MAP$par))]
  prediction_matrix <- matrix(prediction_vector, nrow = nrow(stan_df), ncol = 4)
  colnames(prediction_matrix) <- c("pred_a_win", "pred_b_win", "pred_tie", "pred_bb")

  full_df <- cbind(stan_df, prediction_matrix)  %>% 
    uncount(n) 

  matchups <- full_df %>% 
    group_by(model_a, model_b) %>% 
    select(-both_bad) %>% 
    summarize(
      perc_a_win = sum(winner == 1) / n(),
      perc_b_win = sum(winner == 3) / n(),
      perc_tie = sum(winner == 2) / n(),
      perc_bb = sum(winner == 99) / n(),
      pred_a_win = mean(pred_a_win),
      pred_b_win = mean(pred_b_win),
      pred_tie = mean(pred_tie),
      pred_bb = mean(pred_bb)
    )

  return(matchups)
}

#' Extract predictions for validation set
#'
#' @param model_file the location of the stan model used to generate
#' predicted probabilities
#'
#' @param stan_data a list or environment containing the data needed 
#' for STAN to fit the model
#' 
#'
extract_validation_predictions <- function(model_file, stan_data, n=100) {
  model_object <- stan_model(model_file)

  MAP <- optimizing(
    model_object, 
    data = stan_data
  )

  prediction_vector <- MAP$par[grepl("validation", names(MAP$par))]
  prediction_matrix <- matrix(prediction_vector, nrow = n, ncol = 4)
  colnames(prediction_matrix) <- c("pred_a_win", "pred_b_win", "pred_tie", "pred_bb")

  return(prediction_matrix)
}


############# Create Baseline that assumes constant probability of each outcome ##################

counts <- stan_df %>% 
  uncount(n) %>% 
  group_by(winner) %>% 
  summarize(n=n()) %>% 
  pull(n)
naive_probs <- counts/sum(counts)

constant_forecast <- stan_df %>% 
  uncount(n) %>% 
  group_by(model_a, model_b) %>% 
  select(-both_bad) %>% 
  summarize(
    perc_a_win = sum(winner == 1) / n(),
    perc_b_win = sum(winner == 3) / n(),
    perc_tie = sum(winner == 2) / n(),
    perc_bb = sum(winner == 99) / n(),
    pred_a_win = naive_probs[1],
    pred_b_win = naive_probs[2],
    pred_tie = naive_probs[3],
    pred_bb = naive_probs[4]
  )

############# Create Calibration Plots ########################

plts <- create_calibration_plots(constant_forecast, "Constant Forecast")
ggsave("calibration_plots/baseline.png", plts, width = 8, height = 8)

matchups_c <- create_matchup_df("common_faliure_prob.stan", stan_data)
plts <- create_calibration_plots(matchups_c, "Ordered-Logistic Baseline")
ggsave("calibration_plots/common_faliure_prob_calibration.png", plts, width = 8, height = 8)

matchups_d <- create_matchup_df("derail_model.stan", stan_data)
plts <- create_calibration_plots(matchups_d, "Model 2")
ggsave("calibration_plots/derail_model_calibration.png", plts, width = 8, height = 8)

matchups_b <- create_matchup_df("model_both.stan", stan_data)
plts <- create_calibration_plots(matchups_b, "Model 1")
ggsave("calibration_plots/both_model_calibration.png", plts,  width = 8, height = 8)


############# Create Final Leaderboard Plot ########################

leadeboard_plot <- create_leaderboard_plot("derail_model.stan")

ggsave("leaderboard_plots/derail_model.png", leadeboard_plot, width =8, height = 8)


############# Create Final Predictions ########################

preds <- extract_validation_predictions("derail_model.stan", stan_data)
preds <- cbind(prediction_set, preds)

as.data.frame(preds) %>% 
  write_csv("flagship_model_predictions.csv")



