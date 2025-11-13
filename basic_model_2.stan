data {
  int <lower=0> N_models;
  int <lower=0> N;
  int model_a[N];
  int model_b[N];
  int winner[N];
  vector [N] num;
}
parameters {
 vector[N_models]  theta;
}
model {
  theta ~ normal(0,3);

  for (i in 1:N) {
    target += num[i]*bernoulli_logit_lpmf(winner[i] | theta[model_a[i]] - theta[model_b[i]]);
  }
}
