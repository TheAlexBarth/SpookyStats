data {
  int<lower=0> N;          // Number of observations
  vector[N] x;             // Predictor variable
  vector[N] y;             // Response variable
}

parameters {
  real beta_0;             // Intercept
  real beta_1;             // Coefficient for linear term
  real<lower=0> sigma;     // Standard deviation of residuals
}

model {
  vector[N] mu;

  // Priors (weakly informative)
  beta_0 ~ normal(8, 4);
  beta_1 ~ normal(0, 10);
  sigma ~ normal(0, 5);

  // Likelihood
  y ~ normal(beta_0 + beta_1 * x, sigma);
}