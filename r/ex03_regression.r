set.seed(1031)



# Define true parameters
beta_0 <- 2.0
beta_1 <- 4.0
sigma <- 1.5

# Simulate data
n_zombies <- 100
hours_since_eat <- rnorm(n, mean = 8, sd = 4)
zombie_speed <- beta_0 + beta_1 * x + rnorm(n, mean = 0, sd = sigma)

# Create a data frame
data <- data.frame(x = x, y = y)