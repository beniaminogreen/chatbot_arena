library(arrow)
library(tidyverse)
library(rstan)

parquet_files <- list.files(pattern = "*.parquet")

df <- open_dataset(parquet_files) %>%
  group_by(model_a, model_b, winner) %>%
  summarize(n = n()) %>%
  collect() %>%
  drop_na()

df_no_ties <- df %>%
  filter(!winner %in% c("tie", "both_bad"))
model_names <- unique(c(df$model_a, df$model_b))
n_models <- length(model_names)
model_lookup <- 1:n_models
names(model_lookup) <- model_names

stan_df_no_ties <- df_no_ties %>%
  mutate(
    model_a = model_lookup[model_a],
    model_b = model_lookup[model_b],
    winner = as.numeric(winner == "model_a")
  )

stan_df <- df %>%
  mutate(
    model_a = model_lookup[model_a],
    model_b = model_lookup[model_b],
    winner = case_match(
      winner,
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
  num = stan_df$n
)

both_model <-  stan_model("derail_model.stan")


create_matchup_df <- function(model_file) {
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

ce_loss <- function(df) {
  actual <- as.matrix(df[,c("perc_a_win", "perc_b_win", "perc_tie", "perc_bb")])
  predicted <- as.matrix(df[,c("pred_a_win", "pred_b_win", "pred_tie", "pred_bb")])

  valid_predicted_probabilities <- abs(rowSums(predicted) - 1) < .001
  stopifnot(all(valid_predicted_probabilities))
  valid_actual_probabilities <- abs(rowSums(actual) - 1) < .001
  stopifnot(all(valid_actual_probabilities))

  ce_loss_sum <- 0 
  for (i in 1:nrow(df)) {
    ce_loss_sum <- ce_loss_sum - actual[i,] %*% log(predicted[i,])
  }

  return (c(ce_loss_sum))
}

create_plots <- function(df, model_name = NULL) {
  a_win_plot <- ggplot(df) + 
    geom_point(
      aes(x=pred_a_win, y=perc_a_win), 
    ) + 
    geom_abline(slope=1, linetype = 2) + 
    ggtitle(
      "Precicted VS Actual Probabilities ",
      subtitle = model_name
    ) + 
    xlab("Predicted Probability of Model A winning") + 
    ylab("Observed Probability of Model A winning") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  b_win_plot <- ggplot(df) + 
    geom_point(
      aes(x=pred_b_win, y=perc_b_win), 
    ) + 
    geom_abline(slope=1, linetype = 2) + 
    ggtitle(
      "Precicted VS Actual Probabilities ",
      subtitle = model_name
    ) + 
    xlab("Predicted Probability of Model B winning") + 
    ylab("Observed Probability of Model B winning") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  tie_plot <- ggplot(df) + 
    geom_point(
      aes(x=pred_tie, y=perc_tie), 
    ) + 
    geom_abline(slope=1, linetype = 2) + 
    ggtitle(
      "Precicted VS Actual Probabilities ",
      subtitle = model_name
    )  + 
    xlab("Predicted Probability of tie") + 
    ylab("Observed Probability of tie") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  bb_plot <- ggplot(df) + 
    geom_point(
      aes(x=pred_bb, y=perc_bb), 
    ) + 
    geom_abline(slope=1, linetype = 2) + 
    ggtitle(
      "Precicted VS Actual Probabilities ",
      subtitle = model_name
    ) +
    xlab("Predicted Probability of both bad outcome") + 
    ylab("Observed Probability of both bad outcome") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  list(
    a_win_plot, 
    b_win_plot,
    tie_plot, 
    bb_plot
  )
}

matchups_d <- create_matchup_df("derail_model.stan")
ce_loss(matchups_d)
plts <- create_plots(matchups_d, "Model 2")

matchups_b <- create_matchup_df("model_both.stan")
ce_loss(matchups_b)
plts <- create_plots(matchups_b, "model 1")

matchups_c <- create_matchup_df("common_faliure_prob.stan")
ce_loss(matchups_c)
plts <- create_plots(matchups_c, "model 1")
plts[[3]]



