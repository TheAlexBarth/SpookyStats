data {
  int<lower=0> n_steers;
  int<lower=1> n_ranches;
  array[n_steers] int<lower=1,upper = n_ranches> ranch_id;
  vector[n_steers] obs_horns;
}

parameters {
  real mu_global;
  vector[n_ranches] ranch_means;
  real<lower=0> sigma_cow;
  real<lower=0> sigma_ranch;
}

model {
  // Priors
  mu_global ~ normal(30, 20);
  ranch_means ~ normal(mu_global, sigma_ranch);
  sigma_cow ~ normal(0, 15);
  sigma_ranch ~ normal(0, 15);

  // Likelihood
  for (n in 1:n_steers) {
    obs_horns[n] ~ normal(ranch_means[ranch_id[n]], sigma_cow);
  }
}