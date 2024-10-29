data {
  int<lower=0> T;                // Total time steps
  array[T] int<lower=0> ghost_counts;   // Observed ghost counts
  array[5] int<lower=0> obs_times_reg;  // Observation times for regular observers
  array[8] int<lower=0> obs_times_hun;  // Observation times for ghost hunters
  real<lower=0,upper=1> theta_reg; // Regular observation probability
  real<lower=0,upper=1> theta_hun; // Ghost hunter observation probability
}

parameters {
  real<lower=0> lambda_entry;     // Entry rate parameter
  real<lower=0> lambda_priest;    // Priest removal rate
  real<lower=0> lambda_natural;   // Natural removal rate
}

transformed parameters {
  array[T] real G;  // True latent ghost counts
  G[1] = 0;   // Initial condition for ghost count (no ghosts at start)
}

model {
  // Priors
  lambda_entry ~ normal(0.5, 0.2);    // Prior for entry rate
  lambda_priest ~ normal(0.2, 0.1);   // Prior for priest removal rate
  lambda_natural ~ normal(0.1, 0.05); // Prior for natural removal rate

  // Process model: update ghost population over time
  for (t in 2:T) {
    real seasonal_effect = 0.5 * (1 + sin(2 * pi() * t / 365 - pi() / 2));  // Seasonal entry rate

    // Expected values for new ghosts, natural removals, and priest removals
    real expected_new_ghosts = seasonal_effect * lambda_entry;
    real expected_natural_removals = lambda_natural;
    real expected_priest_removals = lambda_priest;

    // Update ghost count using Poisson distributions for likelihoods
    G[t] ~ poisson(G[t-1] + expected_new_ghosts - expected_natural_removals - expected_priest_removals);
    
    // Ensure G[t] stays between 0 and 3 (carry capacity)
    G[t] = fmax(0, fmin(3, G[t]));
  }

  // Observation model for regular observers
  for (i in 1:5) {
    ghost_counts[obs_times_reg[i]] ~ binomial(3, theta_reg * G[obs_times_reg[i]]);
  }

  // Observation model for ghost hunters
  for (i in 1:8) {
    ghost_counts[obs_times_hun[i]] ~ binomial(3, theta_hun * G[obs_times_hun[i]]);
  }
}


generated quantities {
  array[T] real<lower=0> new_ghosts;
  array[T] real<lower=0> natural_removals;
  array[T] real<lower=0> priest_removals;

  for (t in 2:T) {
    real seasonal_effect = 0.5 * (1 + sin(2 * pi() * t / 365 - pi() / 2));  // Seasonal entry rate
    new_ghosts[t] = poisson_rng(seasonal_effect * lambda_entry);  // Simulate new ghost entries
    natural_removals[t] = poisson_rng(lambda_natural);            // Simulate natural removals
    priest_removals[t] = poisson_rng(lambda_priest);              // Simulate priest removals
  }
}

