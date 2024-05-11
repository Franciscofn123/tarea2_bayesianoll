// Modelo tipo ANOVA para datos SAT, p(mu|tau) =1, p(tau) = 1
// usando reparametrización
data {
  int<lower=0> N; // number of schools
  real y[N]; // treatment effect
  real<lower=0> se[N]; // s.d.
}
parameters {
  real eta[N]; // school means
  real mu;
  real<lower=0> tau;
}
transformed parameters {
  real theta[N]; // school means
  for (j in 1:N)
    theta[j] = mu + tau * eta[j];
}
model {
      y ~ normal(theta,se);
      eta ~ normal(0,1);
	  }

