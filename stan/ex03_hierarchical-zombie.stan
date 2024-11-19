data {
  int<lower=0> N;
  int<lower=0> n_groups;
  vector[N] x;
  vector[N] y;
  array[N] int<lower=1,upper=n_groups> type;
}

parameters {
  real beta_1;
  real group_mean;
  vector[3] group_intercept;
  real<lower=0> sigma;
  real<lower=0> tau;         // Standard deviation of group effects
}

model {
  vector[N] mu;

  // Prior distributions
  beta_1 ~ normal(0, 10);
  group_mean ~ normal(0,10);
  sigma ~ normal(0, 5);
  tau ~ normal(0, 5);

  for(i in 1:n_groups) {
    group_intercept ~ normal(group_mean, tau);
  }

  for (i in 1:N) {
    mu[i] = group_intercept[type[i]] + beta_1 * x[i];
  }
  y ~ normal(mu, sigma);
}