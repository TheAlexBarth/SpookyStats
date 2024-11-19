set.seed(1031)
library(ggplot2) |> suppressMessages()
library(bayesplot) |> suppressMessages()
library(rstan) |> suppressMessages()
library(ggpubr) |> suppressMessages()

rm(list = ls())
source('./R/utils.R') |> suppressMessages()


###
# Simulating Ghost probabilities ######
###


# Simulate continuous predictor
n_houses <- 200
n_murders <- rpois(n_houses, 2)

# Simulate presence (1) or absence (0) based on logistic function
beta_0 <- -5  # Intercept
beta_1 <- 2  # Slope

# Logistic function to get probabilities
p_ghost <- 1 / (1 + exp(-(beta_0 + beta_1 * n_murders)))

# Simulate presence/absence data
haunted <- rbinom(n_houses, size = 1, prob = p_ghost)

ggplot() +
  geom_point(aes(x = n_murders, y = haunted)) +
  theme_minimal()

# \- Prep for Stan -----------

ghost_data <- list(
  N = n_houses,
  y = haunted,
  x = n_murders
)

ghost_fit <- stan(
  './stan/ex04_simple-ghosts.stan',
  'gfit',
  data = ghost_data,
  iter = 5000, warmup = 1000, cores =10
)

saveRDS(ghost_fit, './data/ex04_simple-ghost.rds')
ghost_fit

ghost_post <- extract(ghost_fit)
# \- Plot results ------------------
pred_range <- seq(min(n_murders), max(n_murders), length.out = 1000)


prob_matrix <- matrix(NA, nrow = length(ghost_post$beta_0), ncol = length(pred_range))
for(i in 1:length(pred_range)) {
  prob_matrix[,i] <- plogis(
    ghost_post$beta_0 + ghost_post$beta_1 * pred_range[i]
  )
}

pred_rel <- data.frame(
  mean = apply(prob_matrix, 2, mean),
  low = apply(prob_matrix, 2, function(x) HDInterval::hdi(x)[1]),
  high = apply(prob_matrix, 2, function(x) HDInterval::hdi(x)[2])
)


ggplot() +
  geom_point(aes(x = n_murders, y = haunted)) +
  geom_line(
    aes(x = pred_range, y = pred_rel$mean)
  )+
  geom_ribbon(
    aes(
      x = pred_range,
      ymin = pred_rel$low,
      ymax = pred_rel$high
    ),
    fill = 'lightblue',
    alpha = 0.75
  )+
  theme_minimal()



####
# Expand it out to be an occupancy model ###
####

n_houses

#clairvoyance of house residents
clvoy <- rnorm(n_houses, mean = 10, sd = 4)


# True occupancy probability
occupancy_prob <- plogis(beta_0 + beta_1 * n_murders) 

# Simulate occupancy states for each site
occupancy <- rbinom(n_houses, size = 1, prob = occupancy_prob)

# Simulate detection based on occupancy and a continuous variable

alpha_0 = -5
alpha_1 = 0.3

detection_prob <- plogis(alpha_0 + alpha_1 * clvoy)

n_visits <- sample(1:3, n_houses, replace = T)

detected <- matrix(NA, n_houses, 3)

for (i in 1:n_houses) {
  for (j in 1:n_visits[i]) {
    if (occupancy[i] == 1) {
      detected[i, j] <- rbinom(1, size = 1, prob = detection_prob[i])
    } else {
      detected[i, j] <- 0
    }
  }
}

ggplot() +
  geom_point(
    aes(x = n_murders,
    y = apply(detected, 1, function(x) min(1, sum(x,na.rm = T)))
    )
  )+
  theme_minimal()

detection_data <- list(
  N = n_houses,
  v = n_visits,
  occupancy = occupancy,
  detections = apply(detected, 1, sum, na.rm = T),
  x = n_murders,
  c = clvoy
)

detection_fit <- stan(
  './stan/ex04_occupancy-ghosts.stan',
  'occ-mod',
  data = detection_data,
  iter = 4000, warmup = 1000, cores = 10,
  thin = 10 # it was slow since it estimates every individual p/psi
)

saveRDS(detection_fit, './data/ex04_occupancy-ghosts.rds')
detection_fit

detect_post <- extract(
  detection_fit,
  pars = c('beta_0', 'beta_1', 'alpha_0', 'alpha_1')
)


# \- Plot results ------------------
pred_range <- seq(min(n_murders), max(n_murders), length.out = 1000)


occ_matrix <- matrix(NA, nrow = length(detect_post$beta_0), ncol = length(pred_range))
for(i in 1:length(pred_range)) {
  occ_matrix[,i] <- plogis(
    detect_post$beta_0 + detect_post$beta_1 * pred_range[i]
  )
}

occ_rel <- data.frame(
  mean = apply(occ_matrix, 2, mean),
  low = apply(occ_matrix, 2, function(x) HDInterval::hdi(x)[1]),
  high = apply(occ_matrix, 2, function(x) HDInterval::hdi(x)[2])
)


ggplot() +
  geom_point(
    aes(
      x = n_murders,
      y = haunted
    ),
    position = position_jitter(0,0.03),
    alpha = 0.3,
    color = 'red'
  )+
  geom_point(
    aes(
      x = n_murders,
      y = apply(detected, 1, function(x) min(1, sum(x,na.rm = T)))
    ),
    position = position_jitter(0,0.01),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = pred_range, y = occ_rel$mean)
  )+
  geom_ribbon(
    aes(
      x = pred_range,
      ymin = occ_rel$low,
      ymax = occ_rel$high
    ),
    fill = 'lightblue',
    alpha = 0.75
  )+
  theme_minimal()


# \- Also look at the influence of clvoy --------

crange <- seq(min(clvoy), max(clvoy), length.out = 1000)


detect_mat <- matrix(NA, nrow = length(detect_post$alpha_0), ncol = length(crange))
for(i in 1:length(crange)) {
  detect_mat[,i] <- plogis(
    detect_post$alpha_0 + detect_post$alpha_1 * crange[i]
  )
}

det_rel <- data.frame(
  mean = apply(detect_mat, 2, mean),
  low = apply(detect_mat, 2, function(x) HDInterval::hdi(x)[1]),
  high = apply(detect_mat, 2, function(x) HDInterval::hdi(x)[2])
)


ggplot() +
  geom_point(
    aes(
      x = clvoy,
      y = apply(detected, 1, function(x) min(1, sum(x,na.rm = T)))
    ),
    position = position_jitter(0,0.01),
    alpha = 0.3
  ) +
  geom_line(
    aes(x = crange, y = det_rel$mean)
  )+
  geom_ribbon(
    aes(
      x = crange,
      ymin = det_rel$low,
      ymax = det_rel$high
    ),
    fill = 'lightblue',
    alpha = 0.75
  )+
  theme_minimal()
