library(tidyverse)
theme_set(theme_minimal())
rm(list = ls())

dat_totales <- read_csv('clean_data/datos_consumo_per_capita_totales_factores.csv')
name_order <- read_csv('clean_data/spp_ranking_by_source.csv')


# Species groups time series by type--------
# Supplementary material 2-----

ts_spp_type <- 
  dat_totales %>%
  filter(en_name_gen %in% c(name_order$en_name_gen) &
           total != 1 & totales != 1 & value > 0) %>%
  ggplot(aes(date, value, color = type)) +
  geom_line(alpha = .7) +
  scale_x_date(date_labels = "%Y", date_breaks = '5 year') +
  labs(y = 'Monthly consumption (kg per capita)', x = NULL)  +
  scale_color_viridis_d(option = 'D', end = .8, name = NULL) +
  facet_wrap(~ en_name_gen, scales = 'free_y') +
  theme_minimal(base_size = 9)  +
  theme(legend.position = 'bottom')

ts_spp_type

ggsave(
  plot = ts_spp_type,
  filename = 'figures/time_series_top_spp.png',
  width = 8,
  height = 8,
  dpi = 600
)