#### Plot comparing selection estimates per trait: Plot code ####
# By: Daniel Reyes Corral
# Date: 6/13/2022

length_depth <- left_join(length_pivot, depth_pivot)
length_depth <- na.omit(length_depth)


ggplot(length_depth) +
  aes(
    x = length_mean,
    y = depth_mean,
    group = population,
    colour = population,
    shape = survival
  ) +
  geom_point(size = 3L, show.legend = T) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  #scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Length (mm)",
    y = "Depth (mm)",
    title = "Length - Depth Selection Estimates Plot",
    caption = "Means per Population,
    per Island or all mericarps and survived mericarps",
    color = "Populations",
    shape = "Survival"
  ) +
  #geom_text_repel(aes(label = population)) +
  ggthemes::theme_few() + 
  facet_grid(rows = vars(island), cols = vars(year),
             scales = "free")

# Length - Depth comparisons per Island ####
length_depth %>%
  filter(island %in% "Santa.Cruz") %>%
  ggplot() +
  aes(x = length_mean, y = depth_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Length (mm)",
       y = "Depth (mm)",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")
