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

MAP <- optimizing(
  both_model, 
  data = stan_data
)

prediction_vector <- MAP$par[grepl("preds", names(MAP$par))]
prediction_matrix <- matrix(prediction_vector, nrow = nrow(stan_df), ncol = 4)
colnames(prediction_matrix) <- c("pred_a_win", "pred_b_win", "pred_tie", "pred_both_bad")

full_df <- cbind(stan_df, prediction_matrix)  %>% 
  uncount(n) 

matchups <- full_df %>% 
  group_by(model_a, model_b) %>% 
  select(-both_bad) %>% 
  summarize(
    perc_a_win = sum(winner == 1) / n(),
    perc_b_win = sum(winner == 2) / n(),
    perc_tie = sum(winner == 2) / n(),
    perc_bb = sum(winner == 2) / n(),
    pred_a_win = pred_a_win[1],
    pred_b_win = pred_b_win[1],
    pred_tie = pred_tie[1],
    pred_both_bad = pred_both_bad[1]
  )

ggplot(matchups) + 
  geom_point(
    aes(x=perc_a_win, y=pred_a_win), 
  ) + 
  geom_abline(slope=1)


ggplot(matchups) + 
  geom_point(
    aes(x=perc_b_win, y=pred_b_win), 
  ) + 
  geom_abline(slope=1)

ggplot(matchups) + 
  geom_point(
    aes(x=perc_tie, y=pred_tie), 
  ) + 
  geom_abline(slope=1)

ggplot(matchups) + 
  geom_point(
    aes(x=perc_bb, y=pred_both_bad), 
  ) + 
  geom_abline(slope=1)
