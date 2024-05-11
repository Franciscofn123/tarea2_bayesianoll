// Modelo para datos SAT, usando priori propia pero vaga
// con mu ~ N(0,10^4), tau ~ U(0,10^3)
//
// Siempre chequear el manual, disponible en página web de stan
// 	   	    http://mc-stan.org/
//
// Ver http://mc-stan.org/users/documentation/index.html
//
// Para más detalles de HMC, ver artículo de Michael Betancourt:
//
// https://arxiv.org/abs/1701.02434
//
// Esta implementación usa reparametrización del modelo, con
// 	 theta[j] = mu + tau * eta[j], eta[j] ~ N(0,1)
//
data {
  int<lower=0> N; // number of schools
  real y[N]; // treatment effects
  real<lower=0> se[N]; // standard deviations
}
parameters {
  real eta[N];
  real mu;
  real<lower=0,upper=10^3> tau;
}
transformed parameters {
  real theta[N]; // school means
  for (j in 1:N)
    theta[j] = mu + tau * eta[j];
}
model {
    y ~ normal(theta,se);
    eta ~ normal(0,1);
    mu ~ normal(0,10^2);
    tau ~ uniform(0,10^3);
}

