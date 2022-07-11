library(tidyverse)
rm(list = ls())

# Read variables definition and factors ----
vars_all <- read_csv('clean_data/vars_all.csv')

# Save data with all variables ------------
d <- 
  left_join(vars_all, read_csv('clean_data/datos_consumo_per_capita_totales.csv'), by = 'name') %>% 
  select(- comunidad, - var) %>% 
  drop_na(date) %>% 
  write_csv('clean_data/datos_consumo_per_capita_totales_factores.csv')

# Save data without totals-----------
d1 <- 
  d %>% 
  filter(totales == 0)  %>% 
  write_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv')


# Save data summaries by source and TL-----------
d1 %>% 
  group_by(date, source, catTL) %>% 
  summarise(value = sum(value, na.rm = T), .groups =  'drop') %>% 
  write_csv('clean_data/datos_sum_source_TL.csv')

# Weighted consumption data by TL---------------
d1 %>%
  mutate(wTL = TL * value) %>%
  group_by(date, catTL, source) %>%
  summarise(sumTL = sum(wTL, na.rm = T), .groups = 'drop') 

# Save data summaries by type-----------
d1 %>% 
  group_by(date, type) %>% 
  summarise(value = sum(value, na.rm = T), .groups =  'drop') %>% 
  write_csv('clean_data/datos_sum_type.csv')


# Save data summaries by source-----------
d1 %>% 
  group_by(date, source) %>% 
  summarise(value = sum(value, na.rm = T), .groups =  'drop') %>% 
  write_csv('clean_data/datos_sum_source.csv')
