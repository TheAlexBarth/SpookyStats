set.seed(1031)

rm(list = ls())
library(ggplot2)
library(rstan)
library(dplyr)
library(bayesplot)
library(tidyr)
source('./R/utils.R')


#######
# Different ranches with steers
#######

# Define parameters
global_mean <- 30
n_ranches <- 3
cows_per_ranch <- 15
sigma_ranch <- 5 # ranch level variation
sigma_cow <- 2 # individual variation

# simulate ranch specific means with ranch-variation
ranch_means <- rnorm(n_ranches, global_mean, sigma_ranch)

# Simulate data
ranch_data <- data.frame(
  ranch = rep(c(1:n_ranches), each = cows_per_ranch),
  cow_id = 1:(n_ranches * cows_per_ranch),
  horns = c(1:n_ranches) |> 
    lapply(function(r) rnorm(cows_per_ranch, ranch_means[r], sigma_cow)) |> 
    unlist()
)

ggplot(ranch_data) +
  geom_histogram(
    aes(
      x = horns, 
      fill = ranch
    ),
    alpha = 0.5
  ) +
  facet_wrap(.~ranch) +
  labs(x = "Horn Length (cm)", y = "Count")+
  theme_minimal() +
  theme(legend.position = 'none')


# \- Stan analysis ------------
horn_stan_data <- list(
  n_steers = nrow(ranch_data),
  n_ranches = n_ranches,
  ranch_id = ranch_data$ranch,
  obs_horns = ranch_data$horns
)

horn_fit <- stan(
  './stan/ex02_ranch-horns.stan',
  'ranch-horns',
  data = horn_stan_data,
  iter = 5000,
  warmup = 1000,
  cores = 14 # I have 14 cores on my laptop - you might have less
)

#only for web
saveRDS(horn_fit, './data/ex02_longhorn-ranches.rds')

horn_fit |> 
  mcmc_trace(
    pars = c(
      'mu_global', paste0("ranch_means[", 1:3, ']'),
      'sigma_cow', 'sigma_ranch'
    )
  )

horn_post = rstan::extract(horn_fit)

ggplot() +
  geom_density(
    aes(x = horn_post$mu_global),
    fill = 'grey', alpha = 0.5
  )+
  geom_density(
    data = horn_post$ranch_means |> 
      as.data.frame() |> 
      pivot_longer(
        cols = everything(),
        names_to = 'Ranch', values_to = 'horn_length'
      ) |> 
      mutate(Ranch = gsub('V', '', .data$Ranch)),
    aes(x = horn_length, fill = Ranch),
    alpha = 0.5
  ) +
  geom_vline(
    aes(xintercept = ranch_means, color = as.character(c(1:3))),
    linewidth = 2
  ) +
  #I'm truncating te y xis for clarity
  scale_x_continuous(limits = c(20, 40)) +
  labs(x = 'Horn Length (cm)', y = "") +
  theme_minimal()
