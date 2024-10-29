####
# Utility functions
####

posterior_param_plot <- function(post_samples, prior_function, prior_x_seq, true_value = NULL, ...) {
  post_hdi <- HDInterval::hdi(post_samples)
  prior_y <- prior_x_seq |> 
    sapply(prior_function, ...)

  outplot <- ggplot() +
    geom_area(
      aes(x = prior_x_seq, y = prior_y, fill = "prior"),
      color = 'black'
    ) +
    geom_density(
      aes(x = post_samples, fill = 'posterior')
    ) + 
    geom_point(aes(x = mean(post_samples), y = 0), size = 4, color = 'black') +
    geom_segment(
      aes(y = 0, yend = 0, x = post_hdi[1], xend = post_hdi[2]),
      linewidth = 1.5, color = 'black'
    ) +
    geom_vline(aes(xintercept = true_value), color = 'red', linewidth = 1) +
    scale_fill_manual(values = c('orange', 'lightblue'), breaks = c('prior', 'posterior'))+
    labs(x = "", y = "", fill = "") +
    theme_minimal() +
    theme(legend.position = 'top')
  
  return(outplot)
}
