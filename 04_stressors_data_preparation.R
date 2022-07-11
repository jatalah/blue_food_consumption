library(tidyverse)
library(lubridate)
theme_set(theme_minimal())
rm(list = ls())

blue_food <- 
  read_csv('clean_data/Gephart_et_al_2021_stressors_data.csv') %>% 
  filter(.width == 0.95) %>% 
  mutate(across(median:.upper, ~.x/1000)) %>% 
  select(-.width) %>%
  filter(!stressor %in% c("land", "water"))

# Read national and imported production data for seabream and seabass compiled from APROMAR reports 2012 -2021 (https://apromar.es/informes-anteriores/). Data for missing years, i.e. 1999-2011 and 2022 was replace with the overall mean proportion for each species------ 

prod_prop <- read_csv('clean_data/national_production_proportion_seabass_seabream.csv')

prod_prop %>% 
  pivot_longer(-Year) %>% 
  ggplot() +
  geom_line(aes(Year, value, color =  name))

summary(prod_prop)

vars_all <- read_csv('clean_data/vars_all.csv')

# create new variables for imported and local seabream and seabass-------
d <- 
  read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv') %>% 
  filter(source == 'Farmed') %>% 
  select(name, date, value) %>%  
  pivot_wider() %>% 
  mutate(Year = year(date)) %>% 
  left_join(prod_prop) %>%  
  select(-Year) %>% 
  mutate(
    `DORADA` = DORADA * p_seabream,
    `DORADA IMPORTADA` = DORADA * (1 - p_seabream),
    `LUBINA` = LUBINA * p_seabass,
    `LUBINA IMPORTADA` = LUBINA * (1 - p_seabass),  .keep = 'unused') %>% 
  pivot_longer(cols = -date)


# match stressor and consumption data from Gephart et al 2021 (https://github.com/jagephart/FishPrint)---------
blue_match <- read_csv('clean_data/bluefood_name_matching.csv')

# select and refine stressors based on Ziegler et al. 2013 ---------

####need to fix CI for salmon and adjusted seabass and seabream****
stressors <-
  left_join(blue_food, blue_match, by = "full_taxa_name") %>%
  filter(source == "Farmed") %>%
  drop_na(name) %>%
  select(-source) %>%
  mutate(across(
    median:.upper,
    ~ if_else(str_detect(name, "SALMON") &
                stressor == 'ghg',
              .x * 1.4368, # add 25% ghg from transport
              .x)
  )) %>%
  add_row(tibble(
    name = c(rep("DORADA IMPORTADA", 3), rep("LUBINA IMPORTADA", 3)),
    stressor = rep(c("ghg", "phosphorus", "nitrogen"), 2),
    full_taxa_name = rep('misc marine', 6),
    median = rep(
      c(
        filter(blue_food, stressor == "ghg" &
                 full_taxa_name == 'misc marine') %>% select(median) %>% pull() * 1.5085,
        filter(
          blue_food,
          stressor == "phosphorus" &
            full_taxa_name == 'misc marine'
        ) %>% select(median) %>% pull(),
        filter(
          blue_food,
          stressor == "nitrogen" &
            full_taxa_name == 'misc marine'
        ) %>% select(median) %>% pull()
      ),
      2
    ),
    .lower = rep(
      c(
        filter(blue_food, stressor == "ghg" &
                 full_taxa_name == 'misc marine') %>% select(.lower) %>% pull() * 1.5085,
        filter(
          blue_food,
          stressor == "phosphorus" &
            full_taxa_name == 'misc marine'
        ) %>% select(.lower) %>% pull(),
        filter(
          blue_food,
          stressor == "nitrogen" &
            full_taxa_name == 'misc marine'
        ) %>% select(.lower) %>% pull()
      ),
      2
    ),
    .upper = rep(
      c(
        filter(blue_food, stressor == "ghg" &
                 full_taxa_name == 'misc marine') %>% select(.upper) %>% pull() * 1.5085,
        filter(
          blue_food,
          stressor == "phosphorus" &
            full_taxa_name == 'misc marine'
        ) %>% select(.upper) %>% pull(),
        filter(
          blue_food,
          stressor == "nitrogen" &
            full_taxa_name == 'misc marine'
        ) %>% select(.upper) %>% pull()
      ),
      2
    )
  )) %>%
  left_join(vars_all %>% select(name, source, en_name, en_name_gen, type),  by = "name") %>%
  mutate(
    source = if_else(
      name == "DORADA IMPORTADA" |
        name == "LUBINA IMPORTADA",
      'Farmed',
      source
    ),
    en_name = if_else(name == "DORADA IMPORTADA" , 'Imported Seabream', en_name),
    en_name = if_else(name == "LUBINA IMPORTADA" , 'Imported Seabass', en_name),
    en_name_gen = if_else(name == "LUBINA IMPORTADA" , 'Seabass', en_name_gen),
    en_name_gen = if_else(name == "DORADA IMPORTADA" , 'Seabream', en_name_gen)
  ) %>%
  mutate(type = if_else(str_detect(name, "IMPORTADA"), "Fresh", type)) %>% 
  filter(source == 'Farmed') %>%
  write_csv('clean_data/stressors_data.csv')


# Oslo - Paris = 1,702 km = 25%
# Oslo - Madrid = 2,974 km = 43.7%
# 
# Istanbul - Madrid = 3,646 km 
# Athens - Madrid = 3,206 km
# mean distance = 3426 km = 50%
# 


# calculate stressor consumption data --------------- 
stressor_consumption_raw <- 
  d %>% 
  left_join(stressors,
            by = 'name') %>%
  mutate(st_con = value * median) 

stressor_consumption <-
  stressor_consumption_raw %>%
  group_by(date, stressor, en_name_gen) %>%
  summarise(median = sum(st_con, na.rm = T), .groups = 'drop') %>%
  mutate(stressor = str_to_sentence(stressor),
         stressor = fct_recode(stressor, GHG = 'Ghg')) %>%
  filter(!(str_detect(en_name_gen, c('Turbot|Sea')) & median==0)) %>% 
  write_csv('clean_data/stressors_consumption_data.csv')

