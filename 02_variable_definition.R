library(tidyverse)
library(readxl)
library(janitor)
rm(list = ls())

vars_pesca <- 
  read_csv('clean_data/datos_consumo_per_capita_totales.csv') %>% 
  distinct(name)

definiciones <-
  read_excel(
    "clean_data/definiciones-domestico-octubre-2021_tcm30-552507.xlsx",
    col_types = c("text", "text", "skip", "skip"),
    skip = 1
  ) %>% 
  rename(name = 1) %>% 
  clean_names()

vars <- 
  left_join(vars_pesca, definiciones, by = 'name') %>% 
  mutate(frescos = if_else(str_detect(definicion, 'fresc|Bacaladilla|Pez espada o emperador'), 1, 0),
         congelados = if_else(str_detect(definicion, 'cong'), 1, 0),
         conserva = if_else(str_detect(definicion, 'conserv|cocido|ahuma|salazones'), 1, 0),
         ambos = congelados + frescos,
         total = if_else(str_detect(name, 'TOTAL') | str_detect(name, "PESCADOS CONGELADOS|PESCADOS FRESCOS"), 1, 0),
         acuicultura = if_else(str_detect(name, "LUBINA|DORADA|TRUCHA|SALMON|MEJIL|RODABALLO"), 1, 0),
         mixed = if_else(str_detect(name, "ALME|BERB|LANG|OTROS CON|OTROS MARIS|OTROS PESC|MARISCO/MOLUSC|OTRAS CONS.PESCADO"), 1, 0)) 


# Joint TL data ----
vars_all <-
  left_join(vars,
            read_csv('clean_data/trophic_level_data.csv')) %>%
  mutate(
    type = case_when(
      frescos == 1 ~ 'Fresh',
      congelados == 1 ~ 'Frozen',
      conserva == 1 ~ 'Preserved'
    ),
    source = case_when(
      acuicultura == 1~ "Farmed",
      mixed == 1 ~ "Mixed",
      acuicultura == 0 & mixed == 0 ~ "Wild")
  ) %>%
  select(-c(frescos:conserva), -acuicultura, - mixed) %>%
  left_join(read_csv('clean_data/subtotales.csv')) %>% # get subtotal variables selected manually in excel using the definitions
  mutate(name = fct_recode(name,  `MEJILLON CONSERVA` = "MEJILLONES")) %>% 
  left_join(read_csv('clean_data/vars_translation.csv'),  by = "name") %>% # add english names
  write_csv('clean_data/vars_all.csv')


# Supplementary table 1----------
sup_table_1 <- 
  vars_all %>% 
  dplyr::filter(totales!=1) %>% 
  select(en_name_gen, source, catTL, type) %>% 
  arrange(source, catTL, type, en_name_gen) %>% 
  rename(Taxa = "en_name_gen", TL = 'catTL', Type = 'type', Source = "source") 

write_csv(sup_table_1, 'tables/supp_table_1.csv')
