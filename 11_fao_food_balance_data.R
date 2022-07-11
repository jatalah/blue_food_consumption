library(tidyverse)
library(lubridate)
library(sf)
library(rnaturalearth)
library(rworldmap)
library(rnaturalearthdata)
rm(list = ls())

theme_set(theme_minimal())
options(readr.show_col_types = FALSE)

filter <- dplyr::filter

food_bal <- read_csv('data/FAO data/FAOSTAT_data_3-25-2022_consumo_new.csv')

food_bal %>% 
  distinct(Item)

# top consumer countries in 2019-------
food_bal %>% 
  filter(Year ==2019 & Element == "Food supply quantity (kg/capita/yr)") %>% 
  group_by(Area) %>% 
  summarise(sum = sum(Value)) %>% 
  arrange(-sum) %>% 
  print(n = 20)

# Spanish consumption---
fb_spain <- 
food_bal %>% 
  filter(Area=='Spain' & Element == "Food supply quantity (kg/capita/yr)") 

ggplot(fb_spain,aes(Year , Value, color = Item)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2010,2020, 2)) +
  facet_wrap(~Item, scales = 'free_y')

# world consumtion in 2019 ----
fb_world <- 
  food_bal %>% 
  filter(Element == "Food supply quantity (kg/capita/yr)") %>% 
  filter(Item!=str_detect(Item, 'oil'), Year == max(Year)) %>% 
  group_by(`Area Code (FAO)`, Area) %>% 
  summarise(Value = sum(Value, na.rm = T), .groups = 'drop') %>%
  left_join(read_csv('data/FAO data/nocsDataExport_20220524-100648.csv'), by = c("Area Code (FAO)" = "FAOSTAT"))


countries <-
  ne_countries(returnclass = "sf", scale = 'small') %>% 
  rename(Area = "admin")

fb_world_sf <- 
  left_join(fb_world, countries, by = c("ISO3" = "iso_a3")) %>% 
  st_sf(sf_column_name = 'geometry') 

labs_plot <- prettyNum(seq(0,100,10), big.mark = ",")

world_consumption_2019_map <- 
ggplot() +
  geom_sf(aes(fill = Value), data = fb_world_sf) +
  # scale_fill_gradientn(colours=grDevices::hcl.colors(11, "Lajolla"), name = "Kg per capita") +
  # scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(11, "RdYlBu")), name = "Kg per capita") +
  theme_void() +
  labs(caption = "Data source: FAOSTAT"
  ) +
  theme(legend.position = 'bottom',
        plot.caption = element_text(
          size = 9, color = "grey60",
          hjust = 0.5, vjust = 0,
          margin = margin(t = 5, b = 10)
        )) +
  scale_fill_gradientn(
    name = "Kg per capita",
    labels = seq(0,100,10),breaks = seq(0,100,10),
    colors = grDevices::hcl.colors(9, "Lajolla"),
    guide = guide_legend(
      direction = "horizontal",
      keyheight = 0.5,
      keywidth = 2.5,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = .5,
      nrow = 1,
      byrow = TRUE,
      reverse = FALSE,
      label.position = "bottom"
    )
  ) 

world_consumption_2019_map

# save map--
ggsave('figures/world_consumption_2019_map.png', width = 8, height = 4)

