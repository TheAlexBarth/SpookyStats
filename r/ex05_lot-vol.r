set.seed(1031)
rm(list = ls())

# Load necessary libraries
library(deSolve)
library(ggplot2)
library(rstan)

# Define the Lotka-Volterra model
ghosts_dyanos <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {

    dGhosts <- g_arrival * ghosts - g_removal * ghosts * busters
    dBusters <- b_arrival * ghosts * busters - b_exit * busters
    
    list(c(dGhosts, dBusters))
  })
}

# Parameters
parameters <- c(g_arrival = 0.1,
                g_removal = 0.002,
                b_arrival = 0.002,
                b_exit = 0.1)

# Initial state
state <- c(ghosts = 40, busters = 9)

# Time sequence
times <- seq(0, 100, by = 1)

# Run the simulation using ode_rk45
out <- ode(y = state, times = times, func = ghosts_dyanos, parms = parameters, method = "rk4")

# Convert output to a data frame
out_df <- as.data.frame(out)



# \- Simulate observations ------------------------
b_times <- seq(1,max(times),2)
buster_sightings <- out_df$busters[b_times] |> 
  sapply(function(x) rnorm(1, x, 0.01)) |> # imperfect sightings
  sapply(function(x) max(0,x))

g_times <- sample(c(1:max(times)), 5) |> 
  sort()

ghost_sightings <- out_df$ghosts[g_times] |> 
  sapply(function(x) rnorm(1, x, 2)) |> 
  sapply(function(x) max(0,x))

# Plot the results
ggplot() +
  geom_line(
    data = out_df,
    aes(y = ghosts, color = "Ghosts", x= time)
  ) +
  geom_line(
    data = out_df,
    aes(y = busters, color = "Busters", x= time)
  ) +
  geom_point(
    aes(
      x = b_times,
      y = buster_sightings,
      color = "Busters"
    )
  )+
  geom_point(
    aes(
      x = g_times,
      y = ghost_sightings,
      color = "Ghosts"
    )
  )+
  labs(x = "Time", y = "Group Density") +
  scale_color_manual("", 
                     breaks = c("Ghosts", "Busters"),
                     values = c("blue", "red")) +
  theme_minimal()


# \- Prep for stan -------------------------

buster_stan <- list(
  T = times[-1],
  step = max(times),
  b_obs = length(b_times),
  g_obs = length(g_times),
  b_times = b_times,
  g_times = g_times,
  b_init = buster_sightings[1],
  b = buster_sightings,
  g = ghost_sightings
)

buster_fit <- stan(
  './stan/ex05_lotkavol.stan',
  'pred-prey',
  data = buster_stan,
  iter = 10000, warmup = 500, cores = 14, chains = 4,
  thin = 1
)


saveRDS(buster_fit, './data/ex05_ghost-lotvol.rds')

buster_fit = readRDS('./data/ex05_ghost-lotvol.rds')
buster_post <- extract(buster_fit)
# for memory mgmt
rm(buster_fit)

buster_states <- (1:dim(buster_post$z)[3])  |>  
  lapply(function(i) buster_post$z[,,i])


summarize_state <- function(mat) {
  mean_val = colMeans(mat)
  hdi_range = mat |> 
    apply(2, HDInterval::hdi)

  return(
    data.frame(
      mean = mean_val,
      low = hdi_range[1,],
      high = hdi_range[2,],
      time = times[-1]
    )
  )
}

buster_states <- buster_states |> 
  lapply(summarize_state)



ggplot() +
  geom_line(
    data = buster_states[[2]],
    aes(x = time, y = mean, color = 'Ghosts'),
    linetype = 'dashed'
  )+
  geom_line(
    data = out_df,
    aes(y = ghosts, color = "Ghosts", x= time)
  ) +
  geom_line(
    data = out_df,
    aes(y = busters, color = "Busters", x= time)
  ) +
  geom_point(
    aes(
      x = b_times,
      y = buster_sightings,
      color = "Busters"
    )
  )+
  geom_point(
    aes(
      x = g_times,
      y = ghost_sightings,
      color = "Ghosts"
    )
  )+
  labs(x = "Time", y = "Group Density") +
  scale_color_manual("", 
                     breaks = c("Ghosts", "Busters"),
                     values = c("blue", "red")) +
  theme_minimal()
