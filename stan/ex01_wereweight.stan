data {
  int<lower=0> n;            // Number of observations
  array[n] real y;                 // Observed data
}

parameters {
  real mu;
  real sigma;
}

model {
    sigma ~ normal(30, 10);
    mu ~ normal(150, sigma);
    
    y ~ normal(mu, sigma);
}