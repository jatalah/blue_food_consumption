library(tidyverse)
library(lubridate)
library(ggpubr)
library(anomalize)
library(tsibble)
library(feasts)
theme_set(theme_minimal())
rm(list = ls())
filter <- dplyr::filter

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

# ggsave(
#   plot = time_series_type,
#   filename = 'figures/time_series_type.png',
#   width = 6,
#   height = 3.5,
#   dpi = 600
# )


# type time series decompostion -------
ts_decomp_type <-
  data_type  %>%
  pivot_wider(names_from = type, values_from = value) %>% 
  mutate(Total =  Fresh + Frozen + Preserved) %>% 
  pivot_longer(cols = Fresh:Total, names_to = "type") %>% 
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

# ggsave(
#   plot = ts_decomp_type_plot,
#   filename = 'figures/ts_decomp_type_plot.png',
#   width = 6,
#   height = 3.5,
#   dpi = 600
# )

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

# seasonal component of time series by type -----
seasonal_trend_by_type <- 
  tsibble_type %>% 
  gg_season(value) +
  labs(x= NULL, y = 'Consumption (kg. per capita)') +
  scale_color_viridis_c(option = 'D', end = .9)

# ggsave(
#   plot = seasonal_trend_by_type,
#   filename = 'figures/seasonal_trend_by_type.png',
#   width = 6,
#   height = 5,
#   dpi = 600
# )

# save raw and trend time series together ---------
ts_type_both <- 
ggarrange(
  time_series_type,
  ts_decomp_type_plot,
  nrow = 2,
  common.legend = T,
  labels = 'AUTO',
  legend = 'bottom'
)


### 
# Time series by source -----------
dat_totales <-  read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv')
source_ts_dat <- 
  dat_totales %>% 
  group_by(source, date) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  pivot_wider(names_from = source, values_from = value) %>% 
  mutate(Total =  Farmed +  Mixed  + Wild) %>% 
  pivot_longer(cols = Farmed:Total) 

source_ts_plot <- 
  ggplot(source_ts_dat, aes(date, value, color = name)) +
  geom_line(alpha = .7) +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  labs(y = 'Consumption (kg per capita)', x = NULL) +
  scale_color_viridis_d(
    name = NULL,
    end = .8,
    begin = .2,
    option = 'A',
    direction = -1
  ) +
  theme_minimal(base_size = 8) +
  theme(legend.position = 'bottom')

source_ts_decomp <- 
  source_ts_dat  %>%
  group_by(name) %>%
  time_decompose(value, method = "stl", trend = '13 months') %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()

source_trends_plot <- 
  ggplot(source_ts_decomp, aes(date, trend, color = fct_relevel(name, "Total", after = Inf))) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  geom_point(
    data = source_ts_decomp %>% filter(anomaly == "Yes"),
    aes(date, trend, color = anomaly),
    color = "black",
    size = 1,
    alpha = .3
  ) +
  theme_minimal(base_size = 8) +
  scale_color_viridis_d(
    name = NULL,
    end = .8,
    begin = .2,
    option = 'A',
    direction = -1
  ) +
  labs(y = 'Consumption (kg per capita)', x = NULL) +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1))

source_trends_plot

fig_3_new <- 
  ggarrange(ts_decomp_type_plot, source_trends_plot, nrow = 2, labels = "AUTO")

ggsave(
  plot = fig_2_new,
  filename = 'figures/pdf/figure2_new.pdf',
  width = 10,
  height = 8,
  dpi = 900
)

ggsave(
  plot = fig_2_new,
  filename = 'figures/pdf/figure2_new.tiff',
  width = 180,
  height = 144,
  dpi = 300,
  units = 'mm',
  bg = 'white',
  compression = 'lzw'
)



ggsave(
  plot = ts_type_both,
  filename = 'figures/pdf/figure3.pdf',
  width = 6,
  height = 6,
  dpi = 900
)

ggsave(
  plot = ts_type_both,
  filename = 'figures/pdf/figure3.tiff',
  width = 6,
  height = 6,
  dpi = 900,
  compression = 'lzw',
  bg = 'white'
)

