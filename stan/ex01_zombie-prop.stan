// This is saved as a .stan file
data {
  int<lower=0> n;         // Number of trials
  array[n] int<lower=0, upper=1> y; // Observed outcomes (0 or 1)
}

parameters {
  real<lower=0, upper=1> p; // Probability of success
}

model {
    // Prior:
    p ~ beta(0.5, 0.5);
    
    y ~ bernoulli(p);
}