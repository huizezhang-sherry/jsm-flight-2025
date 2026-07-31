library(tidyverse)
library(patchwork)

# Define basics
n <- 10000 # number of obs
dt <- 10 / n # sampling freq (10 s total)
x <- (0:(n - 1)) * dt # time (seconds)

y_og <- 6*sin(1*2*pi*x) + 4*sin(2*2*pi*x) + 2*sin(4*2*pi*x)
y_s <- 8*sin(1*2*pi*x)
y_c <- 3*sin(1*2*pi*x) + 2*sin(1.5*2*pi*x) +
       2*sin(2*2*pi*x) + 2*sin(2.5*2*pi*x) +
       3*sin(3*2*pi*x) + 1*sin(4*2*pi*x)

factor_orders <- c('Simple', 'Medium', 'Complex')
factor_colors <- c('Simple' = "#00a9b7",
                   'Medium' = 'black',
                   'Complex' = '#bf5700')

# Format data into df
x_y_df <- data.frame(x = rep(x, times = 3),
                     y = c(y_og, y_s, y_c),
                     type = c(rep('Medium', n),
                              rep('Simple', n),
                              rep('Complex', n))) |>
  mutate(type = factor(type, levels = factor_orders))

# First subplot: data
p_data <- x_y_df |>
  filter(x < 2) |>
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(color = type), show.legend = FALSE) +
  facet_wrap(~type) +
  labs(x = 'time (s)', y = 'Signal') +
  scale_color_manual(values = factor_colors) +
  theme_minimal(base_size = 10) +
  scale_x_continuous(breaks = c(0, 1, 2))

# Define fft function
get_amp_freq <- function(data, t){
  y <- data |> pull(y)
  n <- length(y)
  fft_result <- fft(y)
  amp <- 2 * Mod(fft_result)[2:(n/2)] / n
  freq <- (1:(n/2 - 1)) / t
  df <- data.frame('freq' = freq, 'amp' = amp)
  return(df)
}

# Use fft function and plot it
fft_df <- x_y_df |>
  nest(data = c(x, y), .by = type) |>
  rowwise() |>
  mutate(entropy = list(get_amp_freq(data, t = 10))) |>
  select(-data) |>
  unnest(cols = c(entropy)) |>
  filter(freq < 5)

# Plot that
p_fft <- fft_df |> ggplot(aes(x = freq, y = amp, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, ncol = 3) +
  labs(x = 'Frequency (1/s)', y = 'Amplitude') +
  scale_fill_manual(values = factor_colors) +
  theme_minimal(base_size = 10)

# Get probabilities (PSD)
prob_df <- fft_df |>
  group_by(type) |>
  mutate(prob = amp**2/sum(amp**2)) |>
  ungroup()

# Plot that
p_prob <- prob_df |> ggplot(aes(x = freq, y = prob, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, ncol = 3) +
  labs(x = 'Frequency (1/s)', y = 'Spectral \n Density') +
  scale_fill_manual(values = factor_colors) +
  theme_minimal(base_size = 10)

# Calculate entropy
entropy <- prob_df |>
  group_by(type) |>
  summarise(entropy = sum(-prob*log(prob)))

# make plot
fig5 <- p_data/p_fft/p_prob + plot_annotation(tag_levels = 'a')

# Save image
ggsave("figures/fig5.png",
       plot = fig5,
       units = 'cm',
       width = 8.5,
       height = 11)
