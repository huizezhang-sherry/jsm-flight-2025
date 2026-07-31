df <- read.csv("~/Downloads/top2_fft_2017.txt")

p <- df %>%
  filter(type == 'arr') %>%
  ggplot(aes(x = top1_amplitude, y = top2_amplitude)) +
  geom_point()

p <- df %>%
  filter(type == 'arr') %>%
  ggplot(aes(x = top1_amplitude, y = top1_period_mins)) +
  geom_point()

p <- df %>%
  filter(type == 'arr') %>%
  ggplot(aes(x = top1_period_mins, y = top2_period_mins)) +
  geom_label(aes(label = airport))

nrow(df)

df_2 <- df %>%
  select(airport, top1_amplitude, type) %>%
  pivot_wider(names_from = type, values_from = top1_amplitude)

p <- df_2 %>%
  ggplot(aes(x = log(arr), y = log(dep))) +
  geom_point

q <- df %>%
  mutate(amp_frac = top2_amplitude/top1_amplitude*100) %>%
  mutate(min_frac = top2_period_mins/top1_period_mins) %>%
  filter(type == 'arr') %>%
  ggplot(aes(x = min_frac, y = amp_frac)) +
  geom_label(aes(label = airport))
