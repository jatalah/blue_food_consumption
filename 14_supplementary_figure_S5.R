library(tidyverse)
theme_set(theme_minimal())
rm(list = ls())

stressors <- read_csv('clean_data/stressors_data.csv')

# plot Farmed stressors by taxa---------
labels <- as_labeller(
  c(
    GHG = 'a.~GHG~"("*kg~CO[2]~eq.~k^{-1}*")"' ,
    Nitrogen = 'b.~Nitrogen~"("*kg~N~eq.~k^{-1}*")"',
    Phosphorus = 'c.~Phosphorus~"("*kg~P~eq.~k^{-1}*")"'
  ),
  label_parsed
)


farmed_stressor_dat <-
  stressors %>%
  mutate(en_name_gen = if_else(str_detect(en_name, "Import"), en_name, en_name_gen)) %>%
  group_by(en_name_gen, stressor) %>%
  summarise(across(median:.upper, mean), .groups = 'drop') %>%
  mutate(
    en_name_gen = str_to_sentence(en_name_gen),
    stressor = str_to_sentence(stressor),
    stressor = fct_recode(stressor, GHG = 'Ghg')
  )

farmed_stressor_plot <- 
  ggplot(farmed_stressor_dat, aes(x = en_name_gen, color = stressor)) +
  geom_point(aes(y = median)) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper),
                position = "dodge",
                width = 0.1) +
  facet_wrap( ~ stressor, scales = 'free_x', labeller = labels) +
  coord_flip() +
  scale_color_discrete(guide = NULL) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = 'bottom')

farmed_stressor_plot

ggsave(
  plot = farmed_stressor_plot,
  filename = 'figures/pdf/figure_S5.tiff',
  width = 8,
  height = 3,
  dpi = 900,
  bg = 'white',
  compression = 'lzw'
)

ggsave(
  plot = farmed_stressor_plot,
  filename = 'figures/pdf/figure_S5.pdf',
  width = 8,
  height = 3,
  dpi = 900,
  bg = 'white'
)
