library(tidyverse)
library(anomalize)
library(tsibble)
library(feasts)
st_data <- read_csv('clean_data/stressors_data_clean.csv')

d <- 
  read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv') %>% 
  filter(source == 'Farmed')

d %>% 
  distinct(en_name_gen, source, type) %>% 
  arrange(source) %>% 
  print(n = Inf) %>% 
  write_csv('clean_data/wild_mixed_taxa.csv')

blue_match <- read_csv('clean_data/bluefood_name_matching.csv')
blue_food <- read_csv('clean_data/stressors_data_clean.csv')

stressor_farmed_fg <- 
left_join(blue_food, blue_match, by = "full_taxa_name") %>%
  filter(source == "Farmed", stressor == "ghg") %>% 
  drop_na(name) %>%
  select(-source) %>% 
  write_csv('clean_data/stressor_farmed_fg.csv')

stressor_consumption_fg <- 
stressor_farmed_fg %>% 
  left_join(d, .,
            by = 'name') %>%
  mutate(st_con = value * median) %>% 
  group_by(date, stressor, source) %>%
  summarise(median = sum(st_con, na.rm = T), .groups = 'drop') %>%
  mutate(stressor = str_to_sentence(stressor),
         stressor = fct_recode(stressor, GHG = 'Ghg')) 


# Mixed and wild stressors---
stressor_wild_mixed <- read_csv('clean_data/wild_mixed_taxa_stressor_data.csv')

stressor_consumption_fg_wild_mixed <- 
read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv') %>%
  filter(source == 'Mixed' | source == 'Wild') %>%
  left_join(. , stressor_wild_mixed, by = c('source', 'en_name_gen')) %>%
  mutate(st_con = value * median) %>%
  group_by(date, stressor, source) %>%
  summarise(median = sum(st_con, na.rm = T), .groups = 'drop')


d_all <- 
bind_rows(stressor_consumption_fg_wild_mixed, stressor_consumption_fg) %>%
  select(-stressor) %>%
  pivot_wider(names_from = source, values_from = median) %>%
  rowwise() %>%
  mutate(Total = sum(across(Mixed:Farmed), na.rm = T)) %>%
  pivot_longer(cols = c(Mixed:Total)) %>%
  arrange(date) %>%
  drop_na(value)

ts_decomp_ghg_all <- 
 d_all %>% 
  group_by(name) %>%
  time_decompose(value, method = "stl", trend = '13 months') %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()

stressor_ghg_total_trends_plot <- 
ggplot(ts_decomp_ghg_all, aes(x = date, color = fct_relevel(name, "Total", after = Inf))) +
  geom_line(aes(y = trend)) +
  # geom_line(aes(y = observed)) +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  geom_point(
    data = ts_decomp_ghg_all %>% filter(anomaly == "Yes"),
    aes(date, trend, color = anomaly),
    color = "black",
    size = 1,
    alpha = .3
  ) +
  theme_minimal(base_size = 11) +
  scale_color_discrete(name = NULL) +
  labs(y = GHG~"("*kg~CO[2]~eq.*")" , x = NULL, parse = TRUE)  +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1))

stressor_ghg_total_trends_plot

ggsave(
  plot = stressor_ghg_total_trends_plot,
  filename = 'figures/stressor_ghg_total_trends_plot.png',
  width = 6,
  height = 3,
  dpi = 300,
  bg  = 'white'
)


ggsave(
  plot = stressor_ghg_total_trends_plot,
  filename = 'figures/pdf/figure_5.pdf',
  width = 6,
  height = 3,
  dpi = 900,
  bg  = 'white'
)

ggsave(
  plot = stressor_ghg_total_trends_plot,
  filename = 'figures/pdf/figure_5.tiff',
  width = 6,
  height = 3,
  dpi = 900,
  bg  = 'white',
  compression = 'lzw'
)

d_all %>% 
  filter(name=="Total") %>% 
  summarise(mean(value))

ts_decomp_ghg_all %>% 
  group_by(name) %>% 
  summarise(mean(observed))
