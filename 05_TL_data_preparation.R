# Trophic level----
library(tidyverse)
library(rfishbase)
rm(list = ls())

# Get TL for taxa at species level--------
troph_sp <-
  bind_rows(
    ecology(
      c(
        "Salmo salar",
        "Gadus morhua",
        "Oncorhynchus mykiss",
        "Sparus aurata",
        "Sardina pilchardus",
        "Dicentrarchus labrax",
        "Sardinops sagax",
        "Engraulis encrasicolus",
        "Scophthalmus maximus",
        "Merluccius merluccius",
        "Sarda sarda",
        "Sparus aurata",
        "Trachurus picturatus",
        "Dicentrarchus labrax",
        "Micromesistius poutassou",
        "Xiphias gladius"
      ),
      
      fields = c("FoodTroph", "DietTroph","SpecCode", "Species")
    ) %>% 
      mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
      left_join(load_taxa()) 
    ,
    ecology(
      c('Mytilus galloprovincialis'),
      fields = c("FoodTroph", "DietTroph","SpecCode", "Species"),
      server = "sealifebase"
    ) %>%
      mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
      left_join(load_taxa(server = 'sealifebase'))
  ) %>% 
  group_by(Species, Phylum) %>% 
  summarise(TL = mean(TL, na.rm =T), .groups = 'drop') %>%
  rename(Taxa = "Species") %>%  
  replace_na(list(Phylum = "Chordata"))

# Genus level ------
troph_genus <-
  bind_rows(
    ecology(
      c(
        species_list(Genus = 'Scomber'),
        species_list(Genus = 'Solea'),
        species_list(Genus = 'Merluccius'),
        species_list(Genus = 'Thunnus'),
        species_list(Genus = 'Lophius')
      ),
      fields = c("FoodTroph", "SpecCode", "DietTroph", "Species")
    ) %>% 
      mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
      left_join(load_taxa())
    ,
    ecology(c(
      species_list(Genus = 'Loligo', server = 'sealifebase'),
      species_list(Genus = 'Cerastoderma', server = 'sealifebase'),
      species_list(Genus = 'Octopus', server = 'sealifebase')),
      fields = c("FoodTroph", "DietTroph", "SpecCode", "Species"),
      server = "sealifebase"
    ) %>%
      mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
      left_join(load_taxa(server = 'sealifebase'))) %>%  
  group_by(Genus, Phylum) %>% 
  summarise(TL = mean(TL, na.rm =T), .groups = 'drop') %>% 
  rename(Taxa = "Genus") %>%
  replace_na(list(Phylum = "Chordata"))

# Family level ----
troph_fam <- 
bind_rows(
  ecology(c(
  species_list(Family = 'Veneridae', server = 'sealifebase'),
  species_list(Family = 'Penaeidae', server = 'sealifebase')),
  fields = c("FoodTroph", "DietTroph","SpecCode", "Species"),
  server = "sealifebase"
) %>%
  left_join(load_taxa(server = 'sealifebase')),
ecology(
  c(species_list(Family = 'Scombridae'),
    species_list(Family = 'Soleidae')),
  fields = c("FoodTroph", "DietTroph", "SpecCode", "Species")
) %>% 
  left_join(load_taxa())
) %>% 
  mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
  group_by(Family, Phylum) %>% 
  summarise(TL = mean(TL, na.rm =T), .groups = 'drop') %>%
  rename(Taxa = "Family") %>%
  replace_na(list(Phylum = "Chordata"))

# Merge with spp names----
troph_comb <- left_join(read_csv('clean_data/spp_names.csv'), bind_rows(troph_sp, troph_genus, troph_fam))


# Otros pescados---------
otros_pescados_tl <- 
ecology(
  c(
    "Oreochromis niloticus",
    species_list(Genus = 'Tetrapturus'),
    "Pagellus bogaraveo",
    "Beryx splendens",
    "Pangasius hypophthalmus",
    "Luvarus imperialis",
    "Isurus oxyrinchus",
    "Lamna nasus",
    "Mustelus punctulatus",
    "Centrophorus squamosus",
    "Callorhinchus callorynchus",
    "Odontaspis ferox"
  ),
  fields = c("DietTroph", "FoodTroph", "Species")
)  %>% 
  mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
  summarise(TL = mean(TL, na.rm = T))

# Salados-------
salados_tl <- 
  ecology(
    c(
      "Clupea harengus",
      species_list(Genus = 'Gadus'),
      species_list(Genus = 'Thunnus')
    ),
    fields = c("DietTroph", "FoodTroph", "Species")
  )  %>% 
  mutate(TL = if_else(is.na(DietTroph), FoodTroph, DietTroph)) %>% 
  summarise(TL = mean(TL, na.rm = T)) %>% 
  mutate(Phylum = 'Chordata', name = "PESCADO SALADO")

# Mariscos TL----
marisc_tl <- 
  troph_comb %>% 
  filter(Phylum=="Mollusca"| Phylum=="Crustacea") %>% 
  summarise(TL = mean(TL, na.rm = T))


# Pescado TL-------
pescado_tl <- 
  troph_comb %>% 
  filter(Phylum=="Chordata") %>% 
  summarise(TL = mean(TL, na.rm = T))

# Otros TL -----------
otros_tl <- 
  troph_comb %>% 
  filter(str_detect(Phylum, "Chordata|Mollusca|Chordata")) %>% 
  summarise(TL = mean(TL, na.rm = T))


troph_all <-   
troph_comb %>% 
mutate(TL = if_else(name == "PESCADO SALADO", salados_tl$TL,  TL),
       TL = if_else(str_detect(name, "OTROS PESCADOS"), otros_pescados_tl$TL,  TL),
       TL = if_else(str_detect(name, "MARISCO/MOLUSC |OTROS MARISC."), marisc_tl$TL,  TL),
       TL = if_else(str_detect(name, "CONS.PESCADO/MOLUSCO|OTROS AHUMADOS|OTRAS CONS.PESCADO"), otros_tl$TL,  TL)) %>% 
  mutate(catTL = cut(
    TL,
    breaks = c(1, 3.12, 4, 5),
    labels = c("Low", "Medium", "High")
  )) %>% 
  write_csv('clean_data/trophic_level_data.csv')

tl_consumption_data <- 
read_csv('clean_data/datos_consumo_per_capita_totales_factores_sin_totales.csv', show_col_types = FALSE) %>% 
group_by(date, source, catTL) %>% 
  summarise(value = sum(value, na.rm = T), .groups = 'drop') %>% 
  mutate(yearmo = yearmonth(date)) 

