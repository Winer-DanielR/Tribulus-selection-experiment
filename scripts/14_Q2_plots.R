# Question 1: Figures ####
# 
# Date: 02 January, 2023
# By: Daniel Reyes
# 
# Goal: ####
# This script contains the format and code the figure of question 2.
# 
# Description: #### 
# First, load the datasets
# Then, we set up the theme and functions for each plot.
# The plot is a point plot between mean traits and selection estimate (uneaten - eaten)
#
# 

# Data loading ####
## Islands ####
depth_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_island.csv")
length_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_island.csv")
longest_spine_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_island.csv")
tip_distance_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_island.csv")
width_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_island.csv")
lower_spine_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_island.csv")
spine_position_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_island.csv")

## Populations ####
depth_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_population.csv")
length_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_population.csv")
longest_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_population.csv")
tip_distance_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_population.csv")
width_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_population.csv")
lower_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_pop.csv")
spine_position_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_pop.csv")


# Data preparation ####
# Making char into factors (island, populations)
# 
### Island ####
depth_island <- depth_island %>% mutate_at(vars(island), list(factor))
length_island <- length_island %>% mutate_at(vars(island), list(factor))
longest_spine_island <- longest_spine_island %>% mutate_at(vars(island), list(factor))
lower_spine_island  <- lower_spine_island %>% mutate_at(vars(island, lower_spine), list(factor))
spine_position_island <- spine_position_island %>% mutate_at(vars(island, spine_position), list(factor))
tip_distance_island <- tip_distance_island %>% mutate_at(vars(island), list(factor))
width_island <- width_island %>% mutate_at(vars(island), list(factor))


### Populations #####
depth_pop <- depth_pop %>% mutate_at(vars(island, population), list(factor))
length_pop <- length_pop %>% mutate_at(vars(island, population), list(factor))
longest_spine_pop  <- longest_spine_pop %>% mutate_at(vars(island, population), list(factor))
lower_spine_pop <- lower_spine_pop %>% mutate_at(vars(island, population, lower_spine), list(factor))
spine_position_pop <- spine_position_pop %>% mutate_at(vars(island, population, spine_position), list(factor))
tip_distance_pop <- tip_distance_pop %>% mutate_at(vars(island, population), list(factor))
width_pop <- width_pop %>% mutate_at(vars(island, population), list(factor))


# Plot theme and function ####
## Theme ####

plot_theme <-     theme(axis.line = element_line(linetype = "solid", size = 1), 
                        axis.title = element_text(size = 12, 
                                                  #face = "bold"
                        ),
                        axis.text = element_text(size = 11), 
                        #axis.text.x = element_text(size = 8), 
                        plot.title = element_text(size = 12, face = "bold", hjust = 0),
                        text = element_text(family = "Noto Sans"),
                        legend.text = element_text(size = 11), 
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.position = "right",
                        panel.background = element_rect(fill = NA),
                        legend.background = element_rect(fill = NA, size = 0),
                        strip.text = element_text(size = 10, face = "bold"),
                        strip.background = element_blank(),
                        panel.spacing = unit(1, "cm")
) 


## Functions ####
# This function is for the plots per trait grouped by pop.

island_fig <- function(dataset, x, y, title, subtitle)
{ggplot(dataset, aes(x = x, y = y, colour = island)) + #For the function to work the colors need to be defined here.
    geom_point(shape = "circle", size = 3.5) + #Type of plot
    scale_color_manual(
      values = c("#D55E00",
                          "#E69F00",
                          "#009E73",
                          "#0072B2",
                          "#56B4E9",
                          "#CC79A7",
                          "#666666")) + # Set colors
                            labs(
                              x = "Selection (Uneaten - Eaten)",
                              y = "Mean Trait",
                              title = title,
                              subtitle = subtitle,
                              color = "Islands"
                            ) +
    plot_theme +
    #geom_smooth(method = "lm")
    #scale_x_continuous(limits = c(0,NA)) +
    #scale_y_continuous(limits = c(0,NA)) +
    geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
    # + geom_abline(color = "black", size = 1) # Abline is a 1:1 line!
}

# Plots ####
## Lower Spine ####
# Lower spine plots are based on their counts. They don't use the function above.

ggplot(lower_spine_island) +
  aes(
    x = S_lower_spine,
    y = freq_all,
    colour = island,
    shape = lower_spine
  ) +
  geom_point(size = 3.5) +
  scale_color_manual(
    values = c("#D55E00",
                        "#E69F00",
                        "#009E73",
                        "#0072B2",
                        "#56B4E9",
                        "#CC79A7",
                        "#666666")) +
                          labs(
                            x = "Selection (Uneaten - Eaten)",
                            y = "Total frequency Island",
                            title = "Lower Spines",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
  #geom_abline(color = "black", size = 1)

#### All populations in a plot ####
ggplot(lower_spine_pop) +
  aes(
    x = S_lower_spine,
    y = freq_all,
    colour = island,
    shape = lower_spine
  ) +
  geom_point(size = 3.5) +
  scale_color_manual(
    values = c("#D55E00",
                        "#E69F00",
                        "#009E73",
                        "#0072B2",
                        "#56B4E9",
                        "#CC79A7",
                        "#666666")) +
                          labs(
                            x = "Selection (Uneaten - Eaten)",
                            y = "Total Frequency Population",
                            title = "Lower Spines",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
  #geom_abline(color = "black", size = 1)

#### All populations separated ####
ggplot(lower_spine_pop) +
  aes(
    x = S_lower_spine,
    y = freq_all,
    colour = island,
    shape = lower_spine
  ) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("#D55E00",
                        "#E69F00",
                        "#009E73",
                        "#0072B2",
                        "#56B4E9",
                        "#CC79A7",
                        "#666666")) +
                          labs(
                            x = "Selection (Uneaten - Eaten)",
                            y = "Total Frequency Population",
                            title = "Lower Spines per population",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme + theme(axis.line = element_line()) +
  #geom_abline(color = "black", size = 0.8) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")


## Spine position (counts) ####
## Similar to lower spines if spine position is a factor then we should use this
## plot.

# By island separated. Spine position counts
ggplot(spine_position_island) +
  aes(x = S_spine_position, y = freq_all, colour = spine_position) +
  geom_point(shape = "circle", size = 3.3) +
  scale_color_manual(
    values = c(`0` = "#1B9E77",
               `10` = "#AE6D1C",
               `20` = "#0072B2",
               `29` = "#9B58A5",
               `30` = "#D8367D",
               `40` = "#749829",
               `50` = "#E69F00",
               `55` = "#56B4E9",
               `60` = "#CC79A7",
               `70` = "#666666")
  ) +
  labs(
    x = "Selection (Uneaten - Eaten)",
    y = "Total frequency Island",
    title = "Spine Position Island",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")

# By island showing populations ###

ggplot(spine_position_pop) +
  aes(x = S_spine_position, y = freq_all, colour = spine_position) +
  geom_point(shape = "circle", size = 3.3) +
  scale_color_manual(
    values = c(`0` = "#1B9E77",
               `10` = "#AE6D1C",
               `20` = "#0072B2",
               `29` = "#9B58A5",
               `30` = "#D8367D",
               `40` = "#749829",
               `50` = "#E69F00",
               `55` = "#56B4E9",
               `60` = "#CC79A7",
               `70` = "#666666")
  ) +
  labs(
    x = "Selection (Uneaten - Eaten)",
    y = "Total Frequency Population",
    title = "Spine Position Island",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")


## Depth ####

island_depth <- island_fig(depth_island,
                           depth_island$S_depth,
                           depth_island$mean_all,
                           "Mericarp Depth (mm)  ",
                           " ")

pop_depth <- island_fig(depth_pop,
                        depth_pop$S_depth,
                        depth_pop$mean_all,
                        "Mericarp Depth (mm)  ",
                        " "
)


## Length ####

island_length <- island_fig(length_island, #Dataset
                            length_island$S_length, # x label (eaten)
                            length_island$mean_all, # y label (uneaten)
                            "Mericarp Length (mm)  ", # Title
                            " " # Subtitle (Island or Populations)
)

pop_length <- island_fig(length_pop,
                         length_pop$S_length,
                         length_pop$mean_all,
                         "Mericarp Length (mm)  ",
                         " "
)

## Width ####

island_width <- island_fig(width_island,
                           width_island$S_width,
                           width_island$mean_all,
                           "Mericarp Width (mm)  ",
                           " "
)


pop_width <- island_fig(width_pop,
                        width_pop$S_width,
                        width_pop$mean_all,
                        "Mericarp Width (mm)  ",
                        " "
)




## Longest Spine ####

island_spine <- island_fig(longest_spine_island,
                           longest_spine_island$S_longest_spine,
                           longest_spine_island$mean_all,
                           "Spine Length (mm)  ",
                           " "
)

pop_spine <- island_fig(longest_spine_pop,
                        longest_spine_pop$S_longest_spine,
                        longest_spine_pop$mean_all,
                        "Spine Length (mm)  ",
                        " "
)

## Spine tip distance ####

island_tip_distance <- island_fig(tip_distance_island,
                                  tip_distance_island$S_spine_tip_distance,
                                  tip_distance_island$mean_all,
                                  "Spine Distance (mm)  ",
                                  " "
)

pop_tip_distance <- island_fig(tip_distance_pop,
                               tip_distance_pop$S_spine_tip_distance,
                               tip_distance_pop$mean_all,
                               "Spine Distance (mm)  ",
                               " "
)

## Spine position ####

island_spine_pos <- island_fig(spine_position_means_island,
                               spine_position_means_island$spine_position_mean_1,
                               spine_position_means_island$spine_position_mean_0,
                               "Spine Position",
                               " "
)

pop_spine_pos <- island_fig(spine_position_means_pop,
                            spine_position_means_pop$spine_position_mean_1,
                            spine_position_means_pop$spine_position_mean_0,
                            "Spine Position",
                            " "
)

# Spine position was taken as a factor, so I am not sure if it should be
# summarized this way.
# 
# 
# Individual traits figure ####
# Supplemental figure with individual mericarp traits. From the PCA.
Q1_island_plot <- ggarrange(#island_length,
  #island_width,
  island_depth,
  #island_spine,
  island_tip_distance,
  island_spine_pos,
  common.legend = T,
  
  legend = "right",
  labels = c("D", "E", "F"),
  ncol = 3,
  nrow = 1)

Q1_pop_plot <- ggarrange(pop_length,
                         pop_width,
                         #pop_depth,
                         pop_spine,
                         #pop_tip_distance,
                         #pop_spine_pos,
                         common.legend = T,
                         
                         legend = "right",
                         labels = c("A", "B", "C"),
                         ncol = 3,
                         nrow = 1)

