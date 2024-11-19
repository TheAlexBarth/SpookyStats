##
#JP Thing##
###

rm(list = ls())
library(ggplot2)
library(rstan)
library(brms)
library(tidybayes)

df <- read.csv('./data/datasetforAlex.csv')



mod = brm(
  Averagerounded ~ Side, data = df,
  family = negbinomial(),
  iter = 3000
)


ggplot() +
  geom_histogram(aes(x = df$Averagerounded)) +
  facet_wrap(.~ Wind_direction)

glm(Abundance ~ Side, family)

## use tidybayes for plotting idk how.
post = mod |> extract_draws()

post