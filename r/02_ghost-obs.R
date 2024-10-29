# Load the data
ghost_counts <- as.matrix(read.csv("ghost_counts.csv", header = FALSE))

# Prepare observation data
obs_times_reg <- sort(sample(1:T, 5))  # Sparse regular observations
obs_times_hun <- sort(sample(1:T, 8))  # More frequent ghost hunter observations

# Simulate observations
obs_data_reg <- ghost_counts[obs_times_reg, ] * rbinom(length(obs_times_reg) * n_houses, 
                                                      size = 1, prob = theta_reg)
obs_data_hun <- ghost_counts[obs_times_hun, ] * rbinom(length(obs_times_hun) * n_houses, 
                                                      size = 1, prob = theta_hun)

# Prepare Stan data and fit model as before
stan_data <- list(
  T_obs_reg = length(obs_times_reg),
  T_obs_hun = length(obs_times_hun),
  n_houses = n_houses,
  y_reg = obs_data_reg,
  y_hun = obs_data_hun,
  obs_times_reg = obs_times_reg,
  obs_times_hun = obs_times_hun
)

# Fit the model (same Stan model as before)
fit <- stan(model_code = stan_model_code, data = stan_data, 
            iter = 2000, chains = 4)

# Print results
print(fit)