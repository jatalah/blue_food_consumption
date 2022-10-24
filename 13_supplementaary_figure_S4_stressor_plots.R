library(tidyverse)
theme_set(theme_minimal())
rm(list = ls())

stressor_wild_mixed <-read_csv('clean_data/wild_mixed_taxa_stressor_data.csv')
stressor_farmed_fg <- read_csv('clean_data/stressor_farmed_fg.csv')

fg_ghg_data <-
  stressor_wild_mixed %>%
  bind_rows(
    .,
    stressor_farmed_fg %>%
      left_join(read_csv('clean_data/vars_all.csv')) %>%
      distinct(en_name_gen, stressor, median, .lower, .upper) %>%
      mutate(source = "Farmed")
  ) %>%
  distinct(en_name_gen, stressor, median, .lower, .upper, source) 

fg_ghg_plot <- 
ggplot(fg_ghg_data, aes(x = en_name_gen, color = source)) +
geom_point(aes(y = median)) +
geom_errorbar(aes(ymin = .lower, ymax = .upper),
              position = "dodge",
              width = 0.1) +
facet_wrap( ~ source, scales = 'free_y') +
coord_flip() +
scale_color_discrete(name = NULL) +
labs(y = GHG~"("*kg~CO[2]~eq.~k^{-1}*")", x = NULL, parse = TRUE) +
theme(legend.position = 'bottom', axis.title.y = element_blank())

fg_ghg_plot

ggsave(
  plot = fg_ghg_plot,
  filename = 'figures/pdf/figure_S4.pdf',
  width = 8,
  height = 4,
  dpi = 900,
  bg = 'white'
)

ggsave(
  plot = fg_ghg_plot,
  filename = 'figures/pdf/figure_S4.tiff',
  width = 8,
  height = 4,
  dpi = 900,
  bg = 'white'
)
