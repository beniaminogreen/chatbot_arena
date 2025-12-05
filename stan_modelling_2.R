library(arrow)
library(tidyverse)
library(rstan)
library(patchwork)

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

# dim(x)
# head(x)
# table(x$winner)
# identical(sort(unique(x$model_a)), sort(unique(x$model_b)))  # Nice.

# Use this to gather 100 target matchups for prediction (see below).
# Minor modifications may be needed but if you use my set.seed(1)
# then every group should be making predictions for the same set
# of 100 matchups.

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


create_leaderboard_plot <- function(model_file) {
  model_object <- stan_model(model_file)

  MAP <- optimizing(
    model_object, 
    data = stan_data
  )

  thetas <- MAP$par[grepl("theta", names(MAP$par))]
  names(thetas) <- names(model_lookup)

  tibble(
    model = names(thetas),
    score = thetas
  )  %>% 
    ggplot(aes(x= reorder(model, score), y = score)) + 
    coord_flip() + 
    geom_point() + 
    ggtitle("Model Leaderboard") + 
    xlab("Model Name") + 
    ylab("Model Score")  + 
    theme_bw()
}

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
  a_win_plot <- ggplot(df, aes(x=pred_a_win, y=perc_a_win)) + 
    geom_point(alpha = .15) + 
    geom_abline(slope=1, linetype = 2) + 
    geom_smooth(method = "lm", se = F) +
    ggtitle("Model A Wins") + 
    xlab("Predicted Probability of Model A winning") + 
    ylab("Observed Probability of Model A winning") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  b_win_plot <- ggplot(df, aes(x=pred_b_win, y=perc_b_win)) + 
    geom_point(alpha = .15) +  
    geom_abline(slope=1, linetype = 2) + 
    geom_smooth(method = "lm", se = F ) +
    ggtitle("Model B Wins") + 
    xlab("Predicted Probability of Model B winning") + 
    ylab("Observed Probability of Model B winning") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  tie_plot <- ggplot(df,aes(x=pred_tie, y=perc_tie)) + 
    geom_point(alpha = .15) + 
    geom_smooth(method = "lm", se = F ) +
    geom_abline(slope=1, linetype = 2) + 
    ggtitle("Tie")  + 
    xlab("Predicted Probability of tie") + 
    ylab("Observed Probability of tie") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  bb_plot <- ggplot(df, aes(x=pred_bb, y=perc_bb)) + 
    geom_point(alpha = .15) + 
    geom_abline(slope=1, linetype = 2) + 
    geom_smooth(method = "lm", se = F ) +
    ggtitle("Both Bad") +
    xlab("Predicted Probability of both bad outcome") + 
    ylab("Observed Probability of both bad outcome") + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1)) + 
    theme_bw()

  plots <- list(
    a_win_plot, 
    b_win_plot,
    tie_plot, 
    bb_plot
  )

  ce_loss <- round(ce_loss(df),2)

  wrap_plots(plots) +
    plot_annotation(
      title = str_glue("Calibration Plot for {model_name} Model"), 
      subtitle = str_glue("Cross-Entropy Loss: {ce_loss}")
    )
}

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

plts <- create_plots(constant_forecast, "Constant Forecast")
ggsave("calibration_plots/baseline.png", plts, width = 8, height = 8)
matchups_d <- create_matchup_df("derail_model.stan", stan_data)
plts <- create_plots(matchups_d, "Derail Model")
ggsave("calibration_plots/derail_model_calibration.png", plts, width = 8, height = 8)
matchups_b <- create_matchup_df("model_both.stan", stan_data)
plts <- create_plots(matchups_b, "model 1")
ggsave("calibration_plots/both_model_calibration.png", plts,  width = 8, height = 8)
matchups_c <- create_matchup_df("common_faliure_prob.stan", stan_data)
plt <- create_plots(matchups_c, "Global Both Bad Probability")
ggsave("calibration_plots/common_faliure_prob_calibration.png", plts, width = 8, height = 8)




