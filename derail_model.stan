data {
  int <lower=0> N_models;
  int <lower=0> N_test;
  int <lower=0> N;
  int model_a[N];
  int model_b[N];
  int validation_model_a[N_test];
  int validation_model_b[N_test];
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
    real p_either_fail = failure_probability[model_a[i]] + failure_probability[model_b[i]] - failure_probability[model_a[i]]*failure_probability[model_b[i]];
    real p_neither_fail = 1 - p_either_fail;
    real log_either_fail = log(p_either_fail);
    real log_neither_fail = log(p_neither_fail);

    if (both_bad[i] == 1) {
    // if we observe both bad, then we know that either model failed
      target += num[i]*log_either_fail;
    } else {
      real log_beat_prob = ordered_logistic_lpmf(winner[i] | theta[model_a[i]] - theta[model_b[i]], cutpoints);
      target += num[i] * (log_neither_fail  +  log_beat_prob); 
    }
  }
}
generated quantities {
 matrix [N, 4]  preds;
 for (i in 1:N) {
  // Useful Quantities
  real p_either_fail = failure_probability[model_a[i]] + failure_probability[model_b[i]] - failure_probability[model_a[i]]*failure_probability[model_b[i]];
  real p_neither_fail = 1 - p_either_fail;

  // probability of A winning - either b only fails or neither fails and a wins
  preds[i,1] = p_neither_fail * (1 - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]));
  // probability of B winning
  preds[i,2] = p_neither_fail * (inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2]));
  // probability of tie
  preds[i,3] = p_neither_fail * (inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]) - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2]));
  // probability of both bad
  preds[i,4] = p_either_fail;
  }

 matrix [N_test, 4] validation; 
 for (i in 1:N_test) {
    // Useful Quantities
    real p_either_fail = failure_probability[validation_model_a[i]] + failure_probability[validation_model_b[i]] - failure_probability[validation_model_a[i]]*failure_probability[validation_model_b[i]];
    real p_neither_fail = 1 - p_either_fail;

    // probability of A winning - either b only fails or neither fails and a wins
    validation[i,1] = p_neither_fail * (1 - inv_logit(theta[validation_model_a[i]] -theta[validation_model_b[i]] - cutpoints[1]));
    // probability of B winning
    validation[i,2] = p_neither_fail * (inv_logit(theta[validation_model_a[i]] -theta[validation_model_b[i]] - cutpoints[2]));
    // probability of tie
    validation[i,3] = p_neither_fail * (inv_logit(theta[validation_model_a[i]] -theta[validation_model_b[i]] - cutpoints[1]) - inv_logit(theta[validation_model_a[i]] -theta[validation_model_b[i]] - cutpoints[2]));
    // probability of both bad
    validation[i,4] = p_either_fail;
  }

}
