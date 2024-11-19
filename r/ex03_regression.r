set.seed(1031)
library(ggplot2) |> suppressMessages()
library(bayesplot) |> suppressMessages()
library(rstan) |> suppressMessages()
library(ggpubr) |> suppressMessages()

rm(list = ls())
source('./R/utils.R')



####
# Single Case Zombie Speed #####
####


beta_0 = 2.0
beta_1 = 0.5
sigma = 1

n_zombies = 100
hours_since = rnorm(n_zombies, mean = 10, sd= 2) # simulate number of hours
speed = hours_since |> sapply(function(x) rnorm(1,beta_0 + beta_1 * x,sigma))


# simulated data
ggplot() +
  geom_point(aes(x = hours_since, y = speed)) +
  stat_smooth(aes(x = hours_since, y = speed), method = 'lm', color = '#5BD08D') +
  labs(x= 'Hours since eating', y = 'Speed [mph]')+
  theme_minimal()


# \- Running stan -------------------------------------------

lm(speed ~ hours_since) |> summary()

zombie_data <- list(
  N = n_zombies,
  y = speed,
  x = hours_since
)
zombie_single <- stan(
  './stan/ex03_single-zombie-linear.stan',
  'zombie-fit',
  data = zombie_data,
  iter = 5000, warmup = 1000, cores = 14
)

saveRDS(zombie_single, './data/ex03_single-zombie-linear.rds')
zombie_single

# \- Visualizng the results -------------
zombie_single_post <- zombie_single |> 
  rstan::extract(pars = c('beta_0', 'beta_1', 'sigma'))

b0_plot = posterior_param_plot(
  zombie_single_post$beta_0,
  dnorm, seq(1,20, length.out = 1000), beta_0,
  mean = 8, sd = 4
)
b1_plot = posterior_param_plot(
  zombie_single_post$beta_1,
  dnorm, seq(-1, 1, length.out = 1000), beta_1,
  mean = 0, sd = 3
)
sigma_plot = posterior_param_plot(
  zombie_single_post$sigma,
  dnorm, seq(-3,3,length.out = 1000), sigma,
  mean = 0, sd = 5
)
ggarrange(b0_plot, b1_plot, sigma_plot)

# \- predict out the regression --------

pred_range = make_prediction_data(data.frame(hours_since))
pred_zombie_speed = matrix(NA, length(zombie_single_post$beta_0), ncol = length(pred_range$hours_since))
for(i in 1:length(pred_range$hours_since)) {
  pred_zombie_speed[, i] <- zombie_single_post$beta_0 +
    zombie_single_post$beta_1 * pred_range$hours_since[i]
}
pred_rel <- data.frame(
  mean = apply(pred_zombie_speed, 2, mean),
  low = apply(pred_zombie_speed, 2, function(x) HDInterval::hdi(x)[1]),
  high = apply(pred_zombie_speed, 2, function(x) HDInterval::hdi(x)[2])
)

ggplot() +
  geom_point(aes(x = hours_since, y = speed)) + 
  geom_line(
    data = data.frame(
      hours = rep(pred_range$hours_since,100),
      y = pred_zombie_speed[sample(1:nrow(pred_zombie_speed),100),] |> 
        t() |> 
        as.vector(),
      line = as.character(rep(1:100, each = length(pred_range$hours_since)))
    ),
    aes(x = hours, y = y, group = line),
    color = 'grey', alpha = 0.25
  )+
  geom_line(aes(x = pred_range$hours_since, y = pred_rel$mean)) +
  geom_ribbon(
    aes(
      x = pred_range$hours_since,
      ymin = pred_rel$low,
      ymax = pred_rel$high
    ),
    fill = '#5BD08D',
    alpha = 0.5
  )+

  labs(x= 'Hours since eating', y = 'Speed [mph]')+
  theme_minimal()


# \- Hierarchical Zombie Model ----------------

beta_1 = 0.5
type_effect = c(0,2,10)
sigma = 1

n_zombies = 100
hours_since = rnorm(n_zombies, mean = 10, sd= 2) # simulate number of hours
type = sample(c('funny', 'scary', 'video_game'),n_zombies, replace = T) |> as.factor()
speed = beta_1 * hours_since + type_effect[type] + rnorm(n_zombies, 0, sigma)


# simulated data
ggplot() +
  geom_point(aes(x = hours_since, y = speed, color = type)) +
  stat_smooth(aes(x = hours_since, y = speed, color = type), method = 'lm') +
  labs(x= 'Hours since eating', y = 'Speed [mph]')+
  theme_minimal()


# \- Run the stan -----------------------

hier_zdata = list(
  N = n_zombies,
  n_groups = length(type_effect),
  x = hours_since,
  y = speed,
  type = as.numeric(type)
)

hz_fit <- stan(
  './stan/ex03_hierarchical-zombie.stan',
  'hz',
  data = hier_zdata,
  iter = 5000, warmup = 1000, cores = 14
)

saveRDS(hz_fit, './data/ex03_hz.rds')
hz_fit


# \- plot this output --------------
hz_post = extract(hz_fit, pars = c('beta_1', 'group_intercept'))
hz_pred_speed = list(
  matrix(NA, length(hz_post$beta_1), ncol = length(pred_range$hours_since)),
  matrix(NA, length(hz_post$beta_1), ncol = length(pred_range$hours_since)),
  matrix(NA, length(hz_post$beta_1), ncol = length(pred_range$hours_since))
)

for(i in 1:length(hz_pred_speed)) {
  for(j in 1:length(pred_range$hours_since)) {
    hz_pred_speed[[i]][, j] <- hz_post$group_intercept[,i] +
      hz_post$beta_1 * pred_range$hours_since[j]
  }
}

pred_hz <- hz_pred_speed |> 
  lapply(
    function(x) 
    data.frame(
      mean = apply(x, 2, mean),
      low = apply(x, 2, function(k) HDInterval::hdi(k)[1]),
      high = apply(x, 2, function(k) HDInterval::hdi(k)[2])
    )
  ) 
for(i in 1:3) {
  pred_hz[[i]]$type = c('funny', 'scary', 'video_game')[i]
}

pred_hz <- pred_hz |> 
  do.call(what = rbind,)

ggplot() +
  geom_point(aes(x = hours_since, y = speed)) + 
  geom_line(
    aes(x = rep(pred_range$hours_since,3), y = pred_hz$mean, color = pred_hz$type)
  ) +
  geom_ribbon(
    aes(
      x = rep(pred_range$hours_since,3), 
      ymin = pred_hz$low,
      ymax = pred_hz$high, 
      fill = pred_hz$type),
    alpha = 0.5
  )+
  labs(x= 'Hours since eating', y = 'Speed [mph]')+
  theme_minimal()
