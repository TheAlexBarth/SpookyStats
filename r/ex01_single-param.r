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
saveRDS(were_fit, './data/ex01_werewolf-weight.rds')# only for website
posterior_wolfs <- extract(were_fit) 
posterior_param_plot(posterior_wolfs$mu, dnorm, seq(100, 190, length.out = 10000), mu, mean = 150, sd = 30)
posterior_param_plot(posterior_wolfs$sigma, dnorm, seq(5, 40, length.out = 10000), sigma, mean = 30, sd = 10)


####
# Comparing two groups #####
####

# \- simulate the data -----------------------------

# yetis are log-normal distributed

yeti_lmean = log(130)
yeti_lsd = log(3)
yeti_ss = 25
yeti_obs = rlnorm(yeti_ss, yeti_lmean, yeti_lsd)


bigf_mean = 260
bigf_sd = 20
bigf_ss = 50
bigf_obs = rnorm(bigf_ss, bigf_mean, bigf_sd)


ggplot() +

  labs(x = 'kg', y = "Number of indv.", fill = "") +
  theme_minimal()

# \- Prepare data for stan ------------------

monster_stan_data <- list(
  N_yeti = yeti_ss,
  N_bigf = bigf_ss,
  yeti_obs = yeti_obs,
  bigf_obs = bigf_obs
)


monster_fit <- stan(
  './stan/ex01_two-monsters.stan',
  'monsters',
  data = monster_stan_data,
  iter = 3000,
  warmup = 500
)
monster_fit
saveRDS(monster_fit)
monster_posterior <- extract(monster_fit)

ggplot() +
  geom_density(
    aes(x = monster_posterior$bigf_mean, fill = 'bigfoot'),
    alpha = 0.5
  ) + 
  geom_density(
    aes(x = monster_posterior$yeti_median, fill = 'yeti'),
    alpha = 0.5
  ) + 
  geom_point(aes(x = mean(monster_posterior$bigf_mean), color = 'bigfoot', y = 0), size = 4) +
  geom_point(aes(x = mean(monster_posterior$yeti_median), color = 'yeti', y = 0), size = 4) +
  geom_segment(
    aes(
      y = 0, yend = 0, 
      x = HDInterval::hdi(monster_posterior$bigf_mean)[1], 
      xend = HDInterval::hdi(monster_posterior$bigf_mean)[2],
      color = 'bigfoot'
    ),
    linewidth = 1.5
  ) +
  geom_segment(
    aes(
      y = 0, yend = 0, 
      x = HDInterval::hdi(monster_posterior$yeti_median)[1], 
      xend = HDInterval::hdi(monster_posterior$yeti_median)[2], 
      color = 'yeti'
    ),
    linewidth = 1.5
  )+
  scale_fill_manual(values = c('brown', 'lightblue'), breaks = c('bigfoot', 'yeti'))+
  scale_color_manual(values = c('brown', 'lightblue'), breaks = c('bigfoot', 'yeti'))+
  labs(x = "Median Posterior Mass (kg)", y = "", fill = "", color = '') +
  theme_minimal() +
  theme(legend.position = 'top')


# Frequentist comparison
t.test(bigf_obs, yeti_obs)
var.test(bigf_obs, yeti_obs)

kruskal.test(yeti_obs, bigf_obs)
