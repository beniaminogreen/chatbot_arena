data {
  int <lower=0> N_models;
  int <lower=0> N;
  int model_a[N];
  int model_b[N];
  int winner[N]; // 1 (loss), 2 (tie), or 3 (win)
  int both_bad[N];
  vector [N] num;
}
parameters {
 vector[N_models]  theta;
 vector<lower=0, upper=1>[N_models] failure_probability;
 ordered[2] cutpoints;
}
model {
  theta ~ normal(0,2);
  cutpoints ~ normal(0,2);

  for (i in 1:N) {
    real p_both_fail = failure_probability[model_a[i]]*failure_probability[model_b[i]];
    real p_neither_fail = 1 - failure_probability[model_a[i]] - failure_probability[model_b[i]] + p_both_fail;
    real log_both_fail = log(p_both_fail);
    real log_neither_fail = log1m(p_both_fail);
    real p_a_only_fail = failure_probability[model_a[i]]*(1-failure_probability[model_b[i]]);
    real p_b_only_fail = failure_probability[model_b[i]]*(1-failure_probability[model_a[i]]);

    if (both_bad[i] == 1) {
    // if we observe both bad, then we know that the both models failed
      target += num[i]*log_both_fail;
    } else {
       // proability of outcomes assuming no faliures
       real log_beat_prob = ordered_logistic_lpmf(winner[i] | theta[model_a[i]] - theta[model_b[i]], cutpoints);
      // however, if we observe a tie, then we know neither failed
      if (winner[i] == 2) {
        target += num[i]*log_neither_fail;
        target += num[i]*log_beat_prob;
      } else {
        // if model_a wins, then it can be because of a genuine win, which happens with probability
        // (1-p_only_b_fail)/(1-P(both fail)), or it can be becuase model_b failed, which happened with probability failure_prop[model_b[i]].
        if (winner[i] == 1) {
          target += num[i]*log_mix(
              p_b_only_fail/(1-p_both_fail),
              log(failure_probability[model_b[i]]),
              log_beat_prob
          );
        } else {
          target += num[i]*log_mix(
              p_a_only_fail/(1-p_both_fail), 
              log(failure_probability[model_a[i]]),
              log_beat_prob
          );
        }
      }
    }
  }
}
generated quantities {
 matrix [N, 4]  preds;
 for (i in 1:N) {

  // Useful Quantities
  real p_both_fail = failure_probability[model_a[i]]*failure_probability[model_b[i]];
  real p_neither_fail = 1 - failure_probability[model_a[i]] - failure_probability[model_b[i]] + p_both_fail;
  real p_a_only_fail = failure_probability[model_a[i]]*(1-failure_probability[model_b[i]]);
  real p_b_only_fail = failure_probability[model_b[i]]*(1-failure_probability[model_a[i]]);
  
  // probability of A winning - either b only fails or neither fails and a wins
  preds[i,1] = p_neither_fail * (1 - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1])) + p_b_only_fail;
  // probability of B winning
  preds[i,2] = p_neither_fail * (inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2])) + p_a_only_fail;
  // probability of tie
  preds[i,3] = p_neither_fail * (inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]) - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2]));
  // probability of both bad
  preds[i,4] = p_both_fail;
  }
}
