library(tidyverse)
library(readxl)
library(janitor)
library(fuzzyjoin)
library(sf)
rm(list = ls())

# read functions -----------
read_funct <- function(path) {
  path %>%
    excel_sheets() %>%
    .[-1] %>%
    set_names() %>%
    map_df( ~ read_excel(path = path, sheet = .x), .id = 'month') %>% 
    group_by(month) %>% 
    nest() %>% 
    mutate(long_data = map(.x = data,
                           ~ as_tibble(t(.x)) %>% 
                             fill(V1) %>% 
                             mutate(V2 = if_else(is.na(V2), 'var', V2)) %>% 
                             row_to_names(row_number = 1) %>%  
                             select(1, 2, `TOTAL CARNE`,`TOTAL PESCA`:`PESCADO SALADO`) %>% 
                             mutate(date = names(.)[1]) %>% 
                             rename(region = 1) %>% 
                             pivot_longer(cols =  `TOTAL CARNE`:`PESCADO SALADO`))) %>%  
    select(long_data) %>% 
    unnest(cols = c(long_data)) %>% 
    ungroup() 
}
read_funct2 <- function(path) {
  path %>%
    excel_sheets() %>%
    .[-1] %>%
    set_names() %>%
    map_df( ~ read_excel(path = path, sheet = .x), .id = 'month') %>% 
    group_by(month) %>% 
    nest() %>% 
    mutate(long_data = map(.x = data,
                           ~ as_tibble(t(.x)) %>% 
                             fill(V1) %>% 
                             mutate(V2 = if_else(is.na(V2), 'var', V2)) %>% 
                             row_to_names(row_number = 1) %>%  
                             select(1, 2,  `TOTAL CARNE`,`TOTAL PESCA`:`OTRAS CONS.PESCADO`) %>% 
                             mutate(date = names(.)[1]) %>% 
                             rename(region = 1) %>% 
                             pivot_longer(cols = `TOTAL CARNE`:`OTRAS CONS.PESCADO`))) %>%  
    select(long_data) %>% 
    unnest(cols = c(long_data)) %>% 
    ungroup() 
}

# get files directories --------------
file_list <- 
  dir("data/data_consumo/") %>% 
  as_tibble() %>% 
  mutate(value = paste0("data/data_consumo/",value))

# read_data-----------
dat_new <- map_df(file_list$value[7:23], ~read_funct(path = .x)) 
dat_old <- map_df(file_list$value[1:6], ~read_funct2(path = .x)) %>%  
  mutate(region = fct_recode(region,  T.ESPAÑA = ".TOTAL ESPAÑA"))

# data old total españa issues----
dat_old %>%
  mutate(year = str_extract(date, pattern = "(\\d)+")) %>% 
  filter(region =="T.ESPAÑA") %>% 
  distinct(region, year) %>% 
  print(n = Inf) # faltan totales para año 2002 y 2003


tot <- 
dat_old %>%
  filter(region !="T.ESPAÑA" & var =="CONSUMO X CAPITA") %>%
  group_by(month, var, date, name) %>%
  summarise(value = mean(as.numeric(value), na.rm = T)) %>% 
  mutate(region = 'T.ESPAÑA') %>% 
  ungroup()

# check values ----
old_totals <- 
  dat_old %>% 
  filter(region =="T.ESPAÑA" & var =="CONSUMO X CAPITA") %>% 
  mutate(value = as.numeric(value))

left_join(old_totals, tot, by = c('var', 'date', 'name')) %>%
  ggplot(aes(value.x, value.y)) +
  geom_point() +
  geom_abline(
    slope = 1,
    intercept = 0,
    color = 2,
    lty = 2
  )


# add new calculated totals-------
missing_totals <- 
  tot %>% 
  mutate(year = str_extract(date, pattern = "(\\d)+")) %>% 
  filter(year%in%c("02","03"))

dat_old1 <- 
dat_old %>% 
  mutate(value = as.numeric(value)) %>% 
  bind_rows(missing_totals) %>% 
  select(-year)




# clean dates and months----------------
dat_raw <- 
  bind_rows(dat_old1, dat_new %>% mutate(value = as.numeric(value))) %>% 
  mutate(mon = 
           fct_recode(tolower(month),  
                      `01` = "enero", 
                      `02` =  "febrero",
                      `03` = "marzo",
                      `04` = "abril",
                      `05` = "mayo",
                      `06` = "junio",
                      `07` = "julio" ,
                      `08` = "agosto",
                      `09` = "septiembre",
                      `10` = "octubre",
                      `11` = "noviembre",
                      `12` = "diciembre",
                      `09` = "septeimbre"),
         year = str_extract(date, pattern = "(\\d)+"),
         date = as.Date(paste("01", mon, year, sep = '-'), "%d-%m-%y")) 

dat_raw %>%
  mutate(year = str_extract(date, pattern = "(\\d)+")) %>% 
  filter(region =="T.ESPAÑA") %>% 
  distinct(region, year) %>% 
  print(n = Inf) 

# clean comunidades names-----

names_m <- read_sf('data/CCAA/Comunidades_Autonomas_ETRS89_30N.shp') %>%  as_tibble() %>% select(Texto)
names_d <- dat_raw %>% distinct(region) %>%  mutate(Texto = str_to_sentence(region))

names_matching <- 
stringdist_join(names_m, names_d, 
                by = "Texto",
                mode = "full",
                ignore_case = FALSE, 
                method = "lv", 
                max_dist = 99, 
                distance_col = "dist") %>% 
  group_by(Texto.y) %>%
  slice_min(order_by = dist, n = 1) %>% 
  mutate(comunidad = case_when(Texto.y == "Valencia" ~ 'Comunidad Valenciana',
                          Texto.y == "Navarra" ~ 'Comunidad Foral de Navarra',
                          Texto.y == "Madrid" ~ 'Comunidad de Madrid',
                          Texto.y == "Murcia" ~ 'Región de Murcia',
                          Texto.y == "Asturias" ~ 'Principado de Asturias',
                          Texto.y == "Baleares" ~ 'Islas Baleares',
                          str_detect(Texto.y, "españ") ~ 'Total España',
         TRUE ~ Texto.x)) %>% 
  ungroup() %>% 
  select(-dist, - Texto.y) %>% 
  # # rename("region" = Texto.y) %>% 
  distinct(region, comunidad)


data_clean <- 
dat_raw %>% 
  # mutate(region = str_to_sentence(region)) %>% 
  left_join(., names_matching, by = 'region') %>% 
  select(date, comunidad, region, var, name, value) 

# data_clean <- read_csv('clean_data/raw_data_clean.csv')
# glimpse(data_clean)
# data_clean %>% distinct(var)


# clean fish names------
var_names_mat <- read_csv('clean_data/variable_names_matching.csv')

data_clean <-
  left_join(data_clean, var_names_mat, by = 'name') %>%
  mutate(name = ifelse(is.na(new_name), name, new_name)) %>%
  group_by(date, comunidad, var, name) %>%
  summarise(value = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(name = fct_collapse(name, `SARDINA/BOQUERON` = "SARDINA/BOQUERON FR"))


write_csv(data_clean, 'clean_data/datos_consumo_espana.csv')

# summary(data_clean)
glimpse(data_clean)
# distinct(data_clean, name) %>% clipr::write_clip()
# distinct(data_clean, var) 

# save data and subsets ------------
dat_per_capita <-
  data_clean %>%
  filter(var =="CONSUMO X CAPITA") %>%
  write_csv('clean_data/datos_consumo_per_capita.csv')

# datos totales------
dat_totales <- 
  dat_per_capita %>% 
  filter(comunidad == "Total España") %>% 
  write_csv('clean_data/datos_consumo_per_capita_totales.csv')

# datos comunitarios ------
dat_comu <-
  dat_per_capita %>%
  filter(comunidad != "Total España") %>% 
  write_csv('clean_data/datos_consumo_per_capita_comunidades.csv')

# Raw data preparation with all variables----
data_clean <- read_csv('clean_data/datos_consumo_espana.csv') %>% mutate(name = if_else(name == "MEJILLONES", "MEJILLON", name))
var_def <- read_csv('clean_data/vars_all.csv') %>% relocate(totales, .after = total)

all <- 
  left_join(data_clean, var_def, by = 'name') %>%
  mutate(var_type = case_when(totales == 1 ~ "totales",
                              ambos == 2 ~ "totales",
                              # ambos == 1 ~ "subtotal",
                              .default = as.character('individual')), .after = "totales") %>% 
  select(date,
         autonomous_community = comunidad,
         es_name = name,
         en_name,
         en_group = en_name_gen,
         definition_es = definicion,
         source, 
         type,
         var_type:catTL)


write_csv(all, 'clean_data/blue_food_raw_data_all_variables.csv')
