data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
  vector[N] x;
}

parameters {
  real beta_0;
  real beta_1;
}

model {
  // Priors
  beta_0 ~ normal(0, 5);
  beta_1 ~ normal(0, 2);
  
  // Likelihood
  y ~ bernoulli_logit(beta_0 + beta_1 * x);
}