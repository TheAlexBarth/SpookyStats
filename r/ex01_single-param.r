###
# Single parameter examples
###
set.seed(1031)
library(ggplot2)
library(bayesplot)
library(rstan)
source('./R/utils.R')

###
# First for a binomial value ######
###


# \- simulate the data ---------
p = 0.75
num_cases = 50
bites = rbinom(num_cases,1,p)


# \- Set up data for stan

bite_data <- list(
  n = num_cases,
  y = bites
)

zombie_fit <- stan('./stan/ex01_single-param.stan', 'zombie-mod', data = bite_data, iter = 3000, warmup = 1000)

# I save the data to use later for website building purposes so I don't have to compile and run stan everytime I render website.
# but you don't need to save the data if you analyze it below as shown.
saveRDS(zombie_fit, './data/ex01_zombie-bite-model.rds') 

# get posterior for the estimates of p
zombie_p_post <- rstan::extract(zombie_fit)$p

posterior_param_plot(zombie_p_post, dbeta, seq(0,1,length.out = 1000), p, shape1 = 2, shape2 = 2)


###
# Werewolf weights ####
###

mu <- 150
sigma <- 15

werewolfs <- 50
obs_wereweight <- rnorm(werewolfs, mu, sigma)


were_stan_data <- list(
  n = werewolfs,
  y = obs_wereweight
)

were_fit <- stan('./stan/ex01_wereweight.stan', 'wereweight', data = were_stan_data, iter = 3000, warmup = 500)
saveRDS(were_fit, './data/ex01_werewolf-weight.rds')
posterior_wolfs <- extract(were_fit)
posterior_param_plot(posterior_wolfs$mu, dnorm, seq(100, 190, length.out = 10000), mu, mean = 150, sd = 30)
posterior_param_plot(posterior_wolfs$sigma, dnorm, seq(5, 40, length.out = 10000), sigma, mean = 30, sd = 10)
