library(tidyverse)
library(anomalize)
library(tsibble)
library(lubridate)
library(zoo)
library(feasts)
theme_set(theme_minimal())
rm(list = ls())
filter <- dplyr::filter

# read data and convert into time series ----------
d <- read_csv('clean_data/datos_sum_source_TL.csv') 

d %>% 
  group_by(source) %>% 
  summarise(mean(value))

d_ts <- 
  d %>% 
  mutate(yearmo = yearmonth(date)) %>%
  as_tsibble(index = yearmo, key = c(source, catTL), regular = T) 

# raw series---
raw_tl_ts_plot <- 
ggplot(d, aes(date, value, color = fct_relevel(catTL, "Low", "Medium"))) +
  geom_line(alpha = .7) +
  scale_x_date(date_labels = "%Y", date_breaks = '5 year') +
  labs(y = 'Monthly consumption (kg per capita)', x = NULL)  +
  scale_color_viridis_d(option = 'D', end = .8, name = NULL) +
  facet_wrap(~ source, scale = 'free', nrow = 3) +
  theme_minimal(base_size = 9)  +
  theme(legend.position = 'bottom')

raw_tl_ts_plot


# time series decomposition------
ts_decomp <-
  d_ts  %>%
  group_by(source, catTL) %>%
  time_decompose(value, method = "stl", trend = '13 months') %>%
  anomalize(remainder, method = "gesd") %>%
  # anomalize(remainder, method = "iqr") %>%
  time_recompose()

# plot TL consumption trends --------------
labels <- as_labeller(
  c(Farmed = "a. Farmed",
    Mixed = "b. Mixed",
    Wild = "c. Wild"))

tl_trends_plot <- 
  ggplot(ts_decomp, aes(date, trend, color = fct_relevel(catTL, "Low", "Medium"))) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  scale_color_discrete(name = 'Trophic level') +
  facet_wrap(~ source, scale = 'free', nrow = 3, labeller = labels) +
  labs(y = 'Consumption trend (kg per capita)', x = NULL)  +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  geom_point(
    data = ts_decomp %>% filter(anomaly == "Yes"),
    aes(date, trend, color = anomaly),
    color = "black",
    size = 1,
    alpha = .3
  )

tl_trends_plot

ggsave(
  plot = tl_trends_plot,
  filename = 'figures/tl_trends_plot.png',
  dpi = 300,
  width = 6,
  height = 7
)

# seasonal trend --------
ggplot(ts_decomp, aes(date, season, color = catTL)) +
  geom_line() +
  # scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  theme_minimal() +
  scale_color_discrete(name = 'Trophic level') +
  labs(y = 'Trend of monthly per capita seafood consumption (kg.)', x = NULL)  +
  facet_wrap( ~ source, scale = 'free', nrow = 3) +
  labs(y = 'Consumption trend (kg per capita)', x = NULL)  +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom')

