library(tidyverse)
library(patchwork)

# Define basics
n <- 10000 # number of obs
t <- 10 # time (seconds)
dt <- t / n # sampling freq
x <- (0:(n - 1)) * dt # time (seconds)
ya <- 6*sin(1*2*pi*x)
yb <- 4*sin(2*2*pi*x)
yc <- 2*sin(4*2*pi*x)
y <- ya + yb + yc
df_x <- data.frame('x' = x, 'y' = y)

# Make first plot (the full signal)
p_x <- df_x |>
  filter(x < 2) |>
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  theme_minimal(base_size = 10) +
  labs(x = 'time (s)', y = 'Signal')

# Make a dataframe of the decomposed signal
df_y <- data.frame('x' = x, 'ya' = ya, 'yb' = yb, 'yc' = yc) |>
  pivot_longer(cols = starts_with('y'),
               names_to = 'component',
               names_prefix = 'y',
               values_to = 'y') |>
  mutate(component = factor(component,
                            levels = c('a','b','c'),
                            labels = c('Component 1','Component 2', 'Component 3')))

# Plot that
p_y <- df_y |>
  filter(x < 2) |>
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(color = component), show.legend = FALSE) +
  theme_minimal(base_size = 10) +
  facet_wrap(~component, nrow = 3, scales = 'free_y') +
  scale_color_manual(values = c('Component 1' = '#f8971f',
                                'Component 2' = '#bf5700',
                                'Component 3' = '#00a9b7')) +
  labs(x = 'time (s)', y = 'Signal \n components')

# Perform FFT, report results
fft_result <- fft(y)
amp <- 2 * Mod(fft_result)[2:(n/2)] / n
freq <- (1:(n/2 - 1)) / t

df_f <- data.frame('x' = freq, 'y' = amp) |>
  mutate(component = as.factor(case_when(x == 1 ~ '1',
                                         x == 2 ~ '2',
                                         x == 4 ~ '4')))

p_f <- df_f |>
  filter(x < 5) |>
  ggplot(aes(x = x, y = y)) +
  geom_col(aes(fill = component), show.legend = FALSE) +
  theme_minimal(base_size = 10) +
  labs(x = 'Frequency (1/s)', y = 'Amplitude') +
  scale_fill_manual(values = c('1' = '#f8971f',
                               '2' = '#bf5700',
                               '4' = '#00a9b7'))

fig4 <- (p_x/p_f/p_y) +
  plot_layout(heights = c(2, 1, 4)) +
  plot_annotation(tag_levels = 'a')

# Save image
ggsave("figures/fig4.png",
       plot = fig4,
       units = 'cm',
       width = 8.5,
       height = 15)
