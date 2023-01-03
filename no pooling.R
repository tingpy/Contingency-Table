m02 = '
data {
  int J;
  int T;
  int N_P;
  int<lower=0> y_t[T, J];
  int<lower=0> y_k[N_P, J];
  int<lower=1> price_tag[T];
  vector[J] probs[T];
}
parameters {
   simplex[J] theta_k[N_P];
   simplex[J] theta_t[T];
}
model {
  for (i in 1:N_P) y_k[i] ~ multinomial(theta_k[i]); 
  for (i in 1:T) {
          theta_t[i] ~ dirichlet(exp(theta_k[price_tag[i]]+probs[i]));
          y_t[i] ~ multinomial(theta_t[i]);
      }
}
generated quantities {
vector[N_P] log_lik;
  for (i in 1:N_P) {
          log_lik[i] = multinomial_lpmf(y_k[i] | theta_k[i]);
  }
}
'