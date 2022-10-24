library(tidyverse)
library(anomalize)
library(tsibble)
library(feasts)

st_data <- read_csv('clean_data/stressors_data_clean.csv')

d <- 
  read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv') %>% 
  filter(source == 'Farmed')

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
  mutate(st_con = median) %>% 
  group_by(date, source) %>%
  summarise(median = mean(median, na.rm = T),
            .groups = 'drop') 


# Mixed and wild stressors---
stressor_wild_mixed <- read_csv('clean_data/wild_mixed_taxa_stressor_data.csv')

stressor_consumption_fg_wild_mixed <- 
read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv') %>%
  filter(source == 'Mixed' | source == 'Wild') %>%
  left_join(. , stressor_wild_mixed, by = c('source', 'en_name_gen')) %>%
  mutate(st_con = median) %>%
  group_by(date, source) %>%
  summarise(median = mean(median, na.rm = T),
            .groups = 'drop')


d_all <- 
bind_rows(stressor_consumption_fg_wild_mixed, stressor_consumption_fg) %>%
  mutate(across(.cols = median, ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  pivot_wider(names_from = source, values_from = median) %>%
  rowwise() %>%
  mutate(Total = sum(across(Mixed:Farmed)/3, na.rm = T)) %>%
  pivot_longer(cols = c(Mixed:Total)) %>%
  arrange(date) %>%
  drop_na(value)

ggplot(d_all, aes(x = date, color = fct_relevel(name, "Total", after = Inf))) +
  geom_line(aes(y = value)) +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  theme_minimal(base_size = 11) +
  scale_color_discrete(name = NULL) +
  labs(y = Average~GHG~"("*kg~CO[2]~eq.~k^{-1}*") per kg consumed" , x = NULL, parse = TRUE)  +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1))

d_all %>% 
  group_by(name) %>% 
  summarise(mean(value))
