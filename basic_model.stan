data {
  int <lower=0> N_models;
  matrix [N_models, N_models] win_matrix;
}
parameters {
 vector[N_models]  theta;
}
model {
  theta ~ normal(0,3);

  for (i in 1:N_models) {
    for (j in 1:N_models) {
        target += win_matrix[i,j]*(inv_logit(theta[i]-theta[j]));
    }
  }

}
