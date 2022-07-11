library(tidyverse)
theme_set(theme_minimal())

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
  as_labeller(c(Farmed = "a. Farmed",
           Mixed = "b. Mixed",
           Wild = "c. Wild"))

bar_plot_source_type <-
  ggplot(sums, aes(en_name_gen, mean_c, fill = fct_rev(type))) +
  geom_col(aes(x = fct_rev(
    fct_relevel(en_name_gen, name_order %>% pull(en_name_gen))
  ))) +
  coord_flip() +
  facet_wrap(~ source, scales = 'free', labeller = labs_p) +
  scale_fill_viridis_d(
    name = 'Type',
    end = .8,
    begin = .2,
    option = 'A',
    direction = -1
  ) +
  labs(x = NULL, y = "Mean (± SD) monthly per capita consumption (kg)") 

bar_plot_source_type

ggsave(
  plot = bar_plot_source_type,
  filename = 'figures/bar_plot_source_type.png',
  width = 10,
  height = 4,
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

## other option ---------
x <- 
  sums %>% 
  group_by(source) %>% 
  nest() %>% 
  mutate(plots = map(data, ~ggplot(.x, aes(en_name_gen, mean_c, fill = fct_rev(type))) +
                       geom_col(aes(x = fct_rev(
                         fct_relevel(en_name_gen, name_order %>% pull(en_name_gen))
                       ))) +
                       coord_flip() +
                       scale_fill_viridis_d(
                         name = 'Type',
                         end = .8,
                         begin = .2,
                         option = 'A',
                         direction = -1
                       ) +
                       labs(x = NULL, y = "Mean (± SD) monthly per capita consumption (kg)")))


library(ggpubr)
ggarrange(plotlist = x$plots, ncol = 3, common.legend = T, labels = 'auto')
