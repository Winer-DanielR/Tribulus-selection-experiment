# Plot comparing selection estimates per trait: Plot code ####
# By: Daniel Reyes Corral
# Date: 6/13/2022
# Load the datasets from the trait comparison plot folder. 
# Use the datasets to make trait comparisons between all mericarps and
# survived mericarps averaged by population.

# We potentially could use these for model analysis

# Data loading ####
depth <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Trait comparison plot/depth survival mean.csv")
length <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Trait comparison plot/length survival mean.csv")
longest_spine <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Trait comparison plot/longest spine survival mean.csv")
spine_position <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Trait comparison plot/spine position survival mean.csv")
tip_distance <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Trait comparison plot/tip distance survival mean.csv")
width <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Trait comparison plot/width survival mean.csv")

# Data prepping ####
# Year, island, population and survival are going to be grouping factors
depth <- depth %>% mutate_at(vars(year, island, population, survival), list(factor))
length <- length %>% mutate_at(vars(year, island, population, survival), list(factor))
longest_spine <- longest_spine %>% mutate_at(vars(year, island, population, survival), list(factor))
spine_position <- spine_position %>% mutate_at(vars(year, island, population, survival), list(factor))
tip_distance <- tip_distance %>% mutate_at(vars(year, island, population, survival), list(factor))
width <- width %>% mutate_at(vars(year, island, population, survival), list(factor))

# Trait combinations ####
# Here I combined the datasets into a series of trait combinations for the plots
# For each trait combination we remove the NAs

## Length - Depth ####
length_depth <- left_join(length, depth)
length_depth <- na.omit(length_depth)



### Plots ####
# Using the plot comparisons function above we generate plots per island
# for each comparison. Later we could combine these into a single plot.
#### Santa Cruz ####
length_depth %>%
  filter(island %in% "Santa.Cruz") %>%
  ggplot() +
  aes(x = length_mean, y = depth_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Santa Cruz",
       x = "Length",
       y = "Depth",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none", colour = "none") +
  facet_wrap(vars(year), scales = "free")


#### San Cristobal ####
length_depth %>%
  filter(island %in% "San.Cristobal") %>%
  ggplot() +
  aes(x = length_mean, y = depth_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Length",
       y = "Depth",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none", colour = "none") +
  facet_wrap(vars(year), scales = "free")

#### Floreana ####
length_depth %>%
  filter(island %in% "Floreana") %>%
  ggplot() +
  aes(x = length_mean, y = depth_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Floreana",
       x = "Length",
       y = "Depth",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none", colour = "none") +
  facet_wrap(vars(year), scales = "free")

#### Isabela ####
length_depth %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = length_mean, y = depth_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Isabela",
       x = "Length",
       y = "Depth",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(colour = "none") +
  facet_wrap(vars(year), scales = "free")

## Merged plot ####
# Plot with all island plots combined
# length_depth_plot <- ggarrange(santa_cruz1 + rremove("ylab") + rremove("xlab"),
#                            san_cristobal1  + rremove("ylab") + rremove("xlab"),
#                            floreana1  + rremove("ylab") + rremove("xlab"),
#                            isabela1  + rremove("ylab") + rremove("xlab"),
#                            labels = NULL,
#                            ncol = 1,
#                            nrow = 4,
#                            common.legend = T,
#                            legend = "bottom",
#                            align = "hv",
#                            font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))
# annotate_figure(length_depth_plot, left = textGrob("Depth (mm)", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
#                 bottom = textGrob("Length (mm)", gp = gpar(cex = 1.3)))

## Length-Width ####
length_width <- left_join(length, width)
length_width <- na.omit(length_width)
length_width <- length_width %>% unite("year_pop", year, population, remove = F)


### Plots ####
#### Santa Cruz ####
length_width %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = length_mean, y = width_mean, colour = population) +
  #geom_point(shape = "circle", size = 2) +
  geom_line(aes(group = year_pop, colour = population), 
            #colour = island, 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  #scale_color_hue(direction = 1) +
  labs(title = "Isabela",
       x = "Length",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  guides(shape = "none") +
  facet_wrap(vars(island))

length_width %>%
  filter(island %in% "Santa.Cruz") %>%
  ggplot() +
  aes(x = length_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_line(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Santa Cruz",
       x = "Length",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### San Cristobal ####
length_width %>%
  filter(island %in% "San.Cristobal") %>%
  ggplot() +
  aes(x = length_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Length",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Floreana ####
length_width %>%
  filter(island %in% "Floreana") %>%
  ggplot() +
  aes(x = length_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Floreana",
       x = "Length",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Isabela ####
length_width %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = length_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Isabela",
       x = "Length",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

## Length-Longest spine ####
# I think longest spine from before 2018 is tip distance
length_longest_spine <- left_join(length, longest_spine)
length_longest_spine <- na.omit(length_longest_spine)

### Plots ####
#### Santa Cruz ####
length_longest_spine %>%
  filter(island %in% "Santa.Cruz") %>%
  ggplot() +
  aes(x = length_mean, y = longest_spine_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Santa Cruz",
       x = "Length",
       y = "Longest spine",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### San Cristobal ####
length_longest_spine %>%
  filter(island %in% "San.Cristobal") %>%
  ggplot() +
  aes(x = length_mean, y = longest_spine_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Length",
       y = "Longest spine",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Floreana ####
length_longest_spine %>%
  filter(island %in% "Floreana") %>%
  ggplot() +
  aes(x = length_mean, y = longest_spine_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Floreana",
       x = "Length",
       y = "Longest spine",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Isabela ####
length_longest_spine %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = length_mean, y = longest_spine_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Isabela",
       x = "Length",
       y = "Longest spine",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

## Length-Spine position ####
length_spine_position <- left_join(length, spine_position)
length_spine_position <- na.omit(length_spine_position)

### Plots ####
#### Santa Cruz ####
length_spine_position %>%
  filter(island %in% "Santa.Cruz") %>%
  ggplot() +
  aes(x = length_mean, y = spine_position_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Santa Cruz",
       x = "Length",
       y = "Spine Position",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### San Cristobal ####
length_spine_position %>%
  filter(island %in% "San.Cristobal") %>%
  ggplot() +
  aes(x = length_mean, y = spine_position_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Length",
       y = "Spine Position",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Floreana ####
length_spine_position %>%
  filter(island %in% "Floreana") %>%
  ggplot() +
  aes(x = length_mean, y = spine_position_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Floreana",
       x = "Length",
       y = "Spine Position",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Isabela ####
length_spine_position %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = length_mean, y = spine_position_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Isabela",
       x = "Length",
       y = "Spine Position",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

## Length-Tip distance ####
# Tip distance may be discarted because the are so few points per year.

length_tip_distance <- left_join(length, tip_distance)
length_tip_distance <- na.omit(length_tip_distance)

### Plots ####
#### Santa Cruz ####
length_tip_distance %>%
  filter(island %in% "Santa.Cruz") %>%
  filter(year %in% c("2017", "2018")) %>%
  ggplot() +
  aes(x = length_mean, y = tip_distance_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Santa Cruz",
       x = "Length",
       y = "Tip distance",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### San Cristobal ####
length_tip_distance %>%
  filter(island %in% "San.Cristobal") %>%
  ggplot() +
  aes(x = length_mean, y = tip_distance_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Length",
       y = "Tip distance",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Floreana ####
length_tip_distance %>%
  filter(island %in% "Floreana") %>%
  ggplot() +
  aes(x = length_mean, y = tip_distance_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Floreana",
       x = "Length",
       y = "Tip distance",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Isabela ####
length_tip_distance %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = length_mean, y = tip_distance_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Isabela",
       x = "Length",
       y = "Tip distance",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

## Depth-Width ####
depth_width <- left_join(depth, width)
depth_width <- na.omit(depth_width)

### Plots ####
#### Santa Cruz ####
depth_width %>%
  filter(island %in% "Santa.Cruz") %>%
  ggplot() +
  aes(x = depth_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Santa Cruz",
       x = "Length",
       y = "Spine Position",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### San Cristobal ####
depth_width %>%
  filter(island %in% "San.Cristobal") %>%
  ggplot() +
  aes(x = depth_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "San Cristobal",
       x = "Depth",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Floreana ####
depth_width %>%
  filter(island %in% "Floreana") %>%
  ggplot() +
  aes(x = depth_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Floreana",
       x = "Depth",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

#### Isabela ####
depth_width %>%
  filter(island %in% "Isabela") %>%
  ggplot() +
  aes(x = depth_mean, y = width_mean, colour = population, shape = survival) +
  geom_point(size = 3L) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(title = "Isabela",
       x = "Depth",
       y = "Width",
       color = "Populations", shape = "Selection") +
  ggthemes::theme_few() +
  theme(legend.position = "right") +
  #guides(shape = "none") +
  facet_wrap(vars(year), scales = "free")

