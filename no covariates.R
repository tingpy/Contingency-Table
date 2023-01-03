m03 = '
data {
  int J;
  int T;
  int N_P;
  int<lower=0> y_t[T, J];
  int<lower=0> y_k[N_P, J];
  int<lower=1> price_tag[T];
}
parameters {
   simplex[J] gamma;
   vector<lower=4>[J] lambda;
   simplex[J] theta_k[N_P];
   simplex[J] theta_t[T];
}
model {
  for (i in 1:N_P) theta_k[i] ~ dirichlet((gamma).*lambda);
  gamma ~ dirichlet(rep_vector(1, J));
  for (i in 1:J) lambda[i] ~ pareto(4, 1);
  
  for (i in 1:N_P) y_k[i] ~ multinomial(theta_k[i]); 
  for (i in 1:T) {
          theta_t[i] ~ dirichlet(exp(theta_k[price_tag[i]]));
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