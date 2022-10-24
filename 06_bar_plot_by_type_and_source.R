library(tidyverse)
library(anomalize)
library(tsibble)
library(feasts)
rm(list = ls())
filter <- dplyr::filter


dat_totales <-  read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv')

dat_totales %>% 
  group_by(source, date) %>% 
  summarise(value = sum(value), .groups = 'drop') %>% 
  group_by(source) %>% 
  summarise(mean = mean(value), 
            sd = sd(value, na.rm = T), 
            n = n(),
            se = sd/sqrt(n))

# Barplot of top sea foods by source-----------------
sums <-
  dat_totales %>%
  group_by(en_name_gen, source, type) %>%
  summarise_at(
    "value",
    list(
      mean_c = mean,
      sd = sd
    ),
    na.rm = T
  ) %>%
  group_by(source, type) %>% 
  slice_max(mean_c, n = 6) %>% 
  ungroup()

name_order <- 
  sums %>%
  group_by(en_name_gen) %>%
  summarise(mean_c = sum(mean_c)) %>%
  left_join(sums %>% select(en_name_gen, source), by = "en_name_gen") %>% 
  unique() %>% 
  arrange(source, -mean_c) %>% 
  write_csv('clean_data/spp_ranking_by_source.csv')

labs_p <-
  as_labeller(c(Farmed = "A. Farmed",
           Mixed = "B. Mixed",
           Wild = "C. Wild"))

bar_plot_source_type <-
  ggplot(sums, aes(en_name_gen, mean_c, fill = fct_rev(type))) +
  geom_col(aes(x = fct_rev(
    fct_relevel(en_name_gen, name_order %>% pull(en_name_gen))
  ))) +
  coord_flip() +
  facet_wrap(~ source, scales = 'free_y', labeller = labs_p) +
  scale_fill_viridis_d(
    name = NULL,
    end = .8,
    begin = .2,
    option = 'A',
    direction = -1
  ) +
  labs(x = NULL, y = Average~per~capita~consumption~(kg~month^-1)) +
  theme_minimal(base_size = 10) +
  theme(legend.position = c(.1,.2))

bar_plot_source_type

ggsave(
  plot = bar_plot_source_type,
  filename = 'figures/bar_plot_source_type.png',
  width = 7.6,
  height = 3,
  dpi = 900
)

# data summaries-------------
dat_totales %>%
  filter(value>0) %>% 
  group_by(source) %>%
  summarise_at(
    "value",
    list(
      mean_c = mean,
      sd = sd
    ),
    na.rm = T
  ) %>% 
  arrange(-mean_c) 


dat_totales %>%
  group_by(en_name_gen, source) %>%
  summarise(total = mean(value), sd = sd(value)) %>% 
  arrange(source, -total) %>% 
  print(n = Inf) 

sums %>% 
  print(n = Inf)


# Time series by source -----------
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
  theme_minimal(base_size = 10) +
    scale_color_viridis_d(
      name = NULL,
      end = .8,
      begin = .2,
      option = 'A',
      direction = -1
    ) +
  labs(y = 'Consumption (kg per capita)', x = NULL, subtitle = 'D. Time series') +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1))

source_trends_plot

ggsave(
  plot = source_trends_plot,
  filename = 'figures/source_trends_plot.png',
  width = 6,
  height = 4,
  dpi = 900
)

# save both plots --------
fig_2_new <- 
ggarrange(bar_plot_source_type, source_trends_plot, nrow = 2)

fig_2_new

ggsave(
  plot = fig_2_new,
  filename = 'figures/pdf/figure_2.pdf',
  width = 180,
  height = 144,
  dpi = 300,
  units = 'mm',
  bg = 'white'
)

ggsave(
  plot = fig_2_new,
  filename = 'figures/pdf/figure_2.tiff',
  width = 180,
  height = 144,
  dpi = 300,
  units = 'mm',
  bg = 'white',
  compression = 'lzw'
)

# summaries--
source_ts_decomp %>% group_by(name) %>% summarise(max(trend), min(trend))
