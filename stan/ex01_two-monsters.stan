data {
  int<lower=0> N_yeti;
  int<lower=0> N_bigf;
  array[N_yeti] real<lower=0> yeti_obs;
  array[N_bigf] real bigf_obs;
}

parameters {
  // Parameters for the Yeti (log-normal distribution)
  real log_mean;
  real<lower=0> log_sd;
  
  // Parameters for the Bigfoot (normal distribution)
  real bigf_mean;
  real<lower=0> bigf_sd;
}

transformed parameters {
  real yeti_median = exp(log_mean);
}

model {
  // Priors (vague priors, adjust if you have prior knowledge)
  log_mean ~ normal(5.3, 2);        // Prior on log-mean of Yetis
  log_sd ~ normal(0, 2);            // Prior on log-SD of Yetis
  bigf_mean ~ normal(200, 10);      // Prior on Bigfoot mean
  bigf_sd ~ normal(0, 20);          // Prior on Bigfoot SD
  
  // Likelihoods
  yeti_obs ~ lognormal(log_mean, log_sd);
  bigf_obs ~ normal(bigf_mean, bigf_sd);
}

generated quantities {
  // Difference in means on the original scale
  real median_diff = yeti_median - bigf_mean;
}