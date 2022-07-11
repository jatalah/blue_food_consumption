library(tidyverse)
library(lubridate)
library(ggpubr)
library(anomalize)
library(tsibble)
library(feasts)
theme_set(theme_minimal())
rm(list = ls())

# read data----
data_type <- read_csv('clean_data/datos_sum_type.csv')

# time series data summaries by type -------
data_type %>% 
  group_by(type) %>% 
  summarise(across(
    value,
    list(
      mean = mean,
      median = median,
      min = min,
      max = max,
      sd = sd,
      Q1 = ~ quantile(., probs = 0.25, na.rm = T),
      Q3 = ~ quantile(., probs = 0.75, na.rm = T)
    ),
    na.rm = T
  ))

# raw time series by type -----------
time_series_type <-
  ggplot(data_type, aes(date, value, color = type)) +
  geom_line(alpha = .7) +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  labs(y = 'Consumption (kg per capita)', x = NULL) +
  # scale_color_viridis_d(option = 'D', end = .8, name = NULL) +
  scale_color_viridis_d(
    name = NULL,
    end = .8,
    begin = .2,
    option = 'A',
    direction = -1
  ) +
  theme_minimal() +
  theme(legend.position = 'bottom')

time_series_type

ggsave(
  plot = time_series_type,
  filename = 'figures/time_series_type.png',
  width = 6,
  height = 3.5,
  dpi = 600
)


# type time series decompostion -------
ts_decomp_type <-
  data_type  %>%
  group_by(type) %>%
  time_decompose(value, method = "stl", trend = '13 months') %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()

ts_decomp_type_plot <- 
ggplot(ts_decomp_type, aes(date, trend, color = type)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  geom_point(
    data = ts_decomp_type %>% filter(anomaly == "Yes"),
    aes(date, trend, color = anomaly),
    color = "black",
    size = 1,
    alpha = .3
  ) +
  theme_minimal(base_size = 11) +
  labs(y = 'Trend (kg per capita)', x = NULL)  +
  # scale_color_viridis_d(option = 'D', end = .8, name = NULL) +
  scale_color_viridis_d(
    name = NULL,
    end = .8,
    begin = .2,
    option = 'A',
    direction = -1
  ) +
  theme(legend.position = 'bottom')

ts_decomp_type_plot

ggsave(
  plot = ts_decomp_type_plot,
  filename = 'figures/ts_decomp_type_plot.png',
  width = 6,
  height = 3.5,
  dpi = 600
)

# convert data into a tsibble-------
tsibble_type <-
  data_type %>%
  mutate(yearmo = yearmonth(date)) %>%
  as_tsibble(index = yearmo, key = 'type', regular = T)


tsibble_type %>%
  group_by_key() %>%
  index_by(year = ~ year(.)) %>% # monthly aggregates
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE)
  )

# seasonal component oif time series by type -----
seasonal_trend_by_type <- 
  tsibble_type %>% 
  gg_season(value) +
  labs(x= NULL, y = 'Consumption (kg. per capita)') +
  scale_color_viridis_c(option = 'D', end = .9)

ggsave(
  plot = seasonal_trend_by_type,
  filename = 'figures/seasonal_trend_by_type.png',
  width = 6,
  height = 5,
  dpi = 600
)
# save raw and trend time series togetrher ---------
ts_type_both <- 
ggarrange(
  time_series_type,
  ts_decomp_type_plot,
  nrow = 2,
  common.legend = T,
  labels = 'auto',
  legend = 'bottom'
)

ggsave(
  plot = ts_type_both,
  filename = 'figures/ts_type_both.png',
  width = 6,
  height = 6,
  dpi = 600
)