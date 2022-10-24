library(tidyverse)
library(anomalize)
library(tsibble)
library(feasts)
theme_set(theme_minimal())
rm(list = ls())
filter <- dplyr::filter


stressors <- read_csv('clean_data/stressors_data.csv')
# labels <- as_labeller(
#   c(
#     ghg = 'a.~GHG~"("*kg~CO[2]~eq.~k^{-1}*")"' ,
#     nitrogen = 'b.~Nitrogen~"("*kg~N~eq.~k^{-1}*")"',
#     phosphorus = 'c.~Phosphorus~"("*kg~P~eq.~k^{-1}*")"'
#   ), 
#   label_parsed
# )
# 
# 
# # plot stressors by taxa---------
# stressors_plot <- 
#   stressors %>%
#   distinct(en_name, en_name_gen, stressor, median, .lower, .upper, type) %>%
#   ggplot(aes(x = en_name, color = type)) +
#   geom_point(aes(y = median)) +
#   geom_errorbar(aes(ymin = .lower, ymax = .upper),
#                 position = "dodge",
#                 width = 0.1) +
#   facet_wrap(~ stressor, scales = 'free_x', labeller = labels) +
#   coord_flip() +
#   scale_color_discrete(name = NULL) +
#   labs(x = NULL, y = NULL) +
#   theme(legend.position = 'bottom')
# 
# stressors_plot
# 
# ggsave(
#   plot = stressors_plot,
#   filename = 'figures/stressors_plot.png',
#   width = 7,
#   height = 3.5,
#   dpi = 600
# )
# 
# ggsave(
#   plot = stressors_plot,
#   filename = 'figures/pdf/figureS4.pdf',
#   width = 7,
#   height = 3.5,
#   dpi = 900
# )


stressor_consumption <- read_csv('clean_data/stressors_consumption_data.csv')
labels <- as_labeller(
  c(
    GHG = 'a.~GHG~"("*kg~CO[2]~eq.~kg^{-1}*")"' ,
    Nitrogen = 'b.~Nitrogen~"("*kg~N~eq.~kg^{-1}*")"',
    Phosphorus = 'c.~Phosphorus~"("*kg~P~eq.~kg^{-1}*")"'
  ), 
  label_parsed
)

# plot consumption related stressor emissions ------
stressor_consumption %>%
  ggplot(aes(date, median, color = en_name_gen)) +
  geom_line() +
  facet_wrap(~stressor, scale = 'free', nrow = 3, labeller = labels) +
  labs(x = NULL, y = 'Monthly median emissions per capita') +
  scale_color_discrete(name = NULL) +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1))


# stressor consumption time series-----
ts_decomp_stressor <- 
  stressor_consumption %>%
  pivot_wider(names_from = en_name_gen , values_from = median) %>% 
  rowwise() %>% 
  mutate(Total = sum(across(Mussel:Turbot), na.rm = T)) %>% 
  pivot_longer(cols = c(Mussel:Total)) %>% 
  drop_na(value) %>% 
  group_by(name, stressor) %>%
  time_decompose(value, method = "stl", trend = '13 months') %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose()

stressor_trends_plot <- 
  ggplot(ts_decomp_stressor, aes(date, trend, color = fct_relevel(name, "Total", after = Inf))) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = '2 year') +
  geom_point(
    data = ts_decomp_stressor %>% filter(anomaly == "Yes"),
    aes(date, trend, color = anomaly),
    color = "black",
    size = 1,
    alpha = .3
  ) +
  facet_wrap(~ stressor, scale = 'free', nrow = 3, labeller = labels) +
  theme_minimal(base_size = 11) +
  scale_color_discrete(name = NULL) +
  labs(y = 'Stressor emissions trend', x = NULL)  +
  theme(axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = 'bottom') +
  guides(colour = guide_legend(nrow = 1))

stressor_trends_plot

ggsave(
  plot = stressor_trends_plot,
  filename = 'figures/stressor_trends_plot.png',
  width = 7,
  height = 7,
  dpi = 300
)


ggsave(
  plot = stressor_trends_plot,
  filename = 'figures/pdf/figure6.pdf',
  width = 7,
  height = 7,
  dpi = 900
)

ggsave(
  plot = stressor_trends_plot,
  filename = 'figures/pdf/figure6.tiff',
  width = 7,
  height = 7, 
  bg = 'white',
  compression = 'lzw',
  dpi = 900
)


# stressor trend box-plots------------
# ggplot(ts_decomp_stressor) +
#   geom_boxplot(aes(en_name_gen, trend)) +
#   facet_wrap(~stressor, scales = 'free_x') +
#   coord_flip()
# 
# # trends summary ------
# ts_decomp_stressor %>% 
#   group_by(en_name_gen, stressor) %>% 
#   summarise(mean = mean(trend), sd = sd(trend)) %>% 
#   arrange(stressor, mean)
