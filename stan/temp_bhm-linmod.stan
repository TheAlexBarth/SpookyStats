data {
  int<lower=0> N;             // Number of observations
  int<lower=1> J_species;      // Number of species
  int<lower=1> K_location;     // Number of locations
  array[N] int<lower=1, upper=J_species> species;   // Species for each observation
  array[N] int<lower=1, upper=K_location> location; // Location for each observation
  vector[N] x1;                // First predictor variable
  vector[N] x2;                // Second predictor variable
  vector[N] x3;                // Third predictor variable
  vector[N] y;                 // Response variable
}

parameters {
  real alpha;                              // Global intercept
  vector[3] beta;                          // Global slopes for x1, x2, x3
  vector[J_species] alpha_species;         // Species-specific intercepts
  vector[K_location] alpha_location;       // Location-specific intercepts
  matrix[J_species, 3] beta_species;       // Species-specific slopes for x1, x2, x3
  matrix[K_location, 3] beta_location;     // Location-specific slopes for x1, x2, x3
  real<lower=0> sigma_y;                   // Observation noise
  real<lower=0> sigma_alpha_species;       // Species intercept variation
  vector<lower=0>[3] sigma_beta_species;   // Species slope variation for each predictor
  real<lower=0> sigma_alpha_location;      // Location intercept variation
  vector<lower=0>[3] sigma_beta_location;  // Location slope variation for each predictor
}

model {
  // Priors
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  
  // Species and location intercept priors
  alpha_species ~ normal(0, sigma_alpha_species);
  alpha_location ~ normal(0, sigma_alpha_location);
  
  // Species and location slope priors
  for (j in 1:3) {
    beta_species[, j] ~ normal(0, sigma_beta_species[j]);
    beta_location[, j] ~ normal(0, sigma_beta_location[j]);
  }
  
  sigma_y ~ normal(0, 10);
  
  // Likelihood
  for (n in 1:N) {
    y[n] ~ normal(alpha + alpha_species[species[n]] + alpha_location[location[n]]
                  + (beta[1] + beta_species[species[n], 1] + beta_location[location[n], 1]) * x1[n]
                  + (beta[2] + beta_species[species[n], 2] + beta_location[location[n], 2]) * x2[n]
                  + (beta[3] + beta_species[species[n], 3] + beta_location[location[n], 3]) * x3[n],
                  sigma_y);
  }
}
