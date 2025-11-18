data {
  int <lower=0> N_models;
  int <lower=0> N;
  int model_a[N];
  int model_b[N];
  int winner[N]; // 1 (loss), 2 (tie), or 3 (win)
  vector [N] num;
}
parameters {
 vector[N_models]  theta;
 ordered[2] cutpoints;
}
model {
  theta ~ normal(0,2);
  cutpoints ~ normal(0,2);

  for (i in 1:N) {
    target += num[i]*ordered_logistic_lpmf(winner[i] | theta[model_a[i]] - theta[model_b[i]], cutpoints);
  }
}
generated quantities {
 matrix [N, 2]  preds;
 for (i in 1:N) {
  // A wins
  preds[i,1] = 1 - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]);
  // Tie
  preds[i,2] = inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[1]) - inv_logit(theta[model_a[i]] -theta[model_b[i]] - cutpoints[2]);
  }
}
