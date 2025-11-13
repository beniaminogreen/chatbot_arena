library(arrow)
library(tidyverse)
library(rstan)

parquet_files <- list.files(pattern = "*.parquet")

df <- open_dataset(parquet_files) %>%
  group_by(model_a, model_b, winner) %>%
  summarize(n = n()) %>%
  filter(winner != "both_bad") %>%
  collect() %>%
  drop_na()

df_no_ties <- df %>%
  filter(winner != "tie")
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
      "model_b" ~ 1
    )
  )


stan_data <- list(
  N_models = n_models,
  N = nrow(stan_df),
  model_a = stan_df$model_a,
  model_b = stan_df$model_b,
  winner = stan_df$winner,
  num = stan_df$n
)


stan_draws <- stan(
  file = "model_with_ties.stan",
  data = stan_data,
  chains = 4, # number of Markov chains
  warmup = 200, # number of warmup iterations per chain
  iter = 200,
  cores = 4,
)

posterior_means <- get_posterior_mean(stan_draws, "theta") %>%
  rowMeans()

df <- tibble(
  model_a = 1:length(posterior_means),
  score = posterior_means,
)

stan_df %>%
  inner_join(df) %>%
  group_by(model_a) %>%
  summarize(
    win_rate = sum(n[winner == 3]) / sum(n),
    score = mean(score)
  ) %>%
  ggplot(aes(x = score, y = win_rate)) +
  geom_point()
