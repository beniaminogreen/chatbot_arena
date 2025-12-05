# Create calibration plots
#
#' @param model_file the location of the stan model used to generate
#' predicted probabilities
#'
#' @param stan_data a list or environment containing the data needed 
#' for STAN to fit the model
#'
#' @return calibration_plot a four-panel patchwork plot that shows 
#' the predicted vs empirical frequency of outcomes in each dyad. 
#
create_calibration_plots <- function(df, model_name = NULL) {
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
      title = str_glue("Calibration Plot for {model_name}"), 
      subtitle = str_glue("Cross-Entropy Loss: {ce_loss}"), 
      caption = "Each point is a LLM-LLM dyad"
    )
}

# Calculate the Cross-Entropy loss for a series of predictions from STAN
#
# @param df a matchup_df, as produced by `create_matchup_df`
#
# @return the cross-entropy loss
#
ce_loss <- function(df) {
  actual <- as.matrix(df[,c("perc_a_win", "perc_b_win", "perc_tie", "perc_bb")])
  predicted <- as.matrix(df[,c("pred_a_win", "pred_b_win", "pred_tie", "pred_bb")])

  valid_predicted_probabilities <- abs(rowSums(predicted) - 1) < .01
  stopifnot(all(valid_predicted_probabilities))
  valid_actual_probabilities <- abs(rowSums(actual) - 1) < .01
  stopifnot(all(valid_actual_probabilities))

  ce_loss_sum <- 0 
  for (i in 1:nrow(df)) {
    ce_loss_sum <- ce_loss_sum - actual[i,] %*% log(predicted[i,])
  }

  return (c(ce_loss_sum))
}

# Create a leaderboard plot 
#
#' @param model_file the location of the stan model used to generate
#' predicted probabilities
#'
#' @param stan_data a list or environment containing the data needed 
#' for STAN to fit the model
#'
#' @return leaderboard_plot a gpplot object that ranks the models in terms of their estimated abilities
#
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
