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
 real<lower=0, upper=1> failure_probability;
 ordered[2] cutpoints;
}
model {
  theta ~ normal(0,2);
  cutpoints ~ normal(0,2);

  real log_fail = log(failure_probability);
  real log_nofail = log1m(failure_probability);
  for (i in 1:N) {
    if (both_bad[i] == 1) {
      target += num[i]*log_fail;
    } else {
      real log_beat_prob = ordered_logistic_lpmf(winner[i] | theta[model_a[i]] - theta[model_b[i]], cutpoints);
      target += num[i] * (log_nofail  +  log_beat_prob); 
    }
  }
}
generated quantities {
 matrix [N, 4]  preds;
 real p_no_fail = 1.0 - failure_probability;
 for (i in 1:N) {
  // probability of A winning
  preds[i,1] = p_no_fail * (1 - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]));
  // probability of B winning
  preds[i,2] = p_no_fail * (inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2]));
  // probability of tie
  preds[i,3] = p_no_fail * (inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]) - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2]));
  // probability of both bad
  preds[i,4] = failure_probability;
  }
}
