# Question 1: Figures ####
# 
# Date: 02 January, 2023
# By: Daniel Reyes
# 
# Goal: ####
# This script contains the format and code the figure of question 1.
# 
# Description: #### 
# First, we set up the theme and functions for each plot.
# 
# The figure is a 1:1 ratio line comparing eaten and uneaten mericarps. Per traits. 
# CHECK LOWER SPINES METHODS HOW TO SUMMARIZE

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
depth_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_population.csv")
length_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_population.csv")
longest_spine_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_population.csv")
tip_distance_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_population.csv")
width_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_population.csv")
lower_spine_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_pop.csv")
spine_position_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_pop.csv")

## PCA Populations ####
pca_means <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_population_NAs.csv")

pca <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_scores.csv")

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

### PCA populations ####
pca_means <- pca_means %>% mutate_at(vars(island, population), list(factor))
str(pca_means)

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
# NOTE for ggplot stuff: Turns out that to use a single legend, the colors, shapes and fills need to have the
# SAME labels. Also, if you want outlines in the shapes you need characters 21:25 (values)
# and fill controls the inner colors and colour controls the outline!
# 
island_fig <- function(dataset, x, y, title, subtitle)
  {ggplot(dataset, aes(x = x, y = y, colour = island, shape = island, fill = island)) + #For the function to work the colors need to be defined here.
    geom_abline(color = "black", size = 1) + # Abline is a 1:1 line!
  geom_point(size = 3.5, stroke = 1) + #Type of plot
    scale_x_continuous(limits = c(-2,1), breaks = c(-2,-1.5,-1,-0.5,0,0.5,1)) +
    scale_y_continuous(limits = c(-1,2), breaks = c(-1,-0.5,0,0.5,1,1.5,2)) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.8) + #Adds a dashed line trough origin 
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.8) +
    scale_fill_manual(values = c("#D55E00",
                                 "#E69F00",
                                 "#009E73",
                                 "#0072B2",
                                 "#56B4E9",
                                 "#CC79A7",
                                 "#666666"), name = "Islands",
                                 labels = c("Floreana",
                                            "Isabela",
                                            "San Cristobal",
                                            "Santa Cruz")) +
  scale_color_manual(values = c("black", 
                                "black", 
                                "black", 
                                "black"),
               name = "Islands",
    labels = c("Floreana",
               "Isabela",
               "San Cristobal",
               "Santa Cruz")) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Islands",
                     labels = c("Floreana",
                                "Isabela",
                                "San Cristobal",
                                "Santa Cruz")) +
  labs(
    x = "Eaten",
    y = "Uneaten",
    title = title,
    subtitle = subtitle,
    #color = "Islands"
    fill = "Islands"
  ) +
  plot_theme
    #geom_smooth(color = "black", size = 1, method = "glm",aes(group=1)) 
  
  }

# Plots ####
## PCA ####
## These plots are the same as the indvidual plots, no model was fitted here
### Size ####
pca_size <- island_fig(pca_means,
                       pca_means$Size_mean_1,
                       pca_means$Size_mean_0,
                         "Mericarp Size (PC1)  ",
                         " "
)


### Defense ####
pca_defense <- island_fig(pca_means,
                          pca_means$Defense_mean_1,
                          pca_means$Defense_mean_0,
                          "Mericarp Defense (PC2)  ",
                          " ")
### Position ####
pca_position <- island_fig(pca_means,
                           pca_means$Position_mean_1,
                           pca_means$Position_mean_0,
                           "Spine Position (PC3)  ",
                           " ")

pca_position1 <- island_fig(pca_means,
                           pca_means$PC3_mean_1,
                           pca_means$PC3_mean_0,
                           "Spine Position (PC3)  ",
                           " ")

### Combined PCA plots ####
Q1_pca_plot <- ggarrange(pca_size + rremove("ylab") + rremove("xlab"),
                            pca_defense + rremove("ylab") + rremove("xlab"),
                         pca_position1 + rremove("ylab") + rremove("xlab"),
                            common.legend = T,
                            legend = "right",
                            labels = c("A", "B", "C"),
                            ncol = 3,
                            nrow = 1)

annotate_figure(Q1_pca_plot, left = textGrob("Morphology of Uneaten Mericarps", 
                                               rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Morphology of Eaten Mericarps", gp = gpar(cex = 1.1)))

#+ geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))

plot(ggpredict(mericarp_size, terms = "Size [all]",
                   allow.new.levels = T))

### PC1 ####
pca_PC1 <- island_fig(pca_means,
                       pca_means$PC1_mean_1,
                       pca_means$PC1_mean_0,
                       "PC1 (Untrasformed)  ",
                       " "
)

## Lower Spine ####
# Lower spine plots are based on their counts. They don't use the function above.

ggplot(lower_spine_island) +
  aes(
    x = freq_1,
    y = freq_0,
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
    x = "Eaten",
    y = "Uneaten",
    title = "Lower Spines",
    color = "Islands",
    shape = "Presence of lower spines"
  ) +
  plot_theme +
  geom_abline(color = "black", size = 1)

#### All populations in a plot ####
ggplot(lower_spine_pop) +
  aes(
    x = freq_1,
    y = freq_0,
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
                            x = "Eaten",
                            y = "Uneaten",
                            title = "Lower Spines",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_abline(color = "black", size = 1)

#### All populations separated ####
ggplot(lower_spine_pop) +
  aes(
    x = freq_1,
    y = freq_0,
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
    x = "Eaten",
    y = "Uneaten",
    title = "Lower Spines per population",
    color = "Islands",
    shape = "Presence of lower spines"
  ) +
  plot_theme + theme(axis.line = element_line()) +
  geom_abline(color = "black", size = 0.8) +
  facet_wrap(vars(island), scales = "free")


## Spine position (counts) ####
## Similar to lower spines if spine position is a factor then we should use this
## plot.

# By island separated. Spine position counts
ggplot(spine_position_island) +
  aes(x = freq_0, y = freq_1, colour = spine_position) +
  geom_point(shape = "circle", size = 3.3) +
  scale_color_manual(
    values = c(`0` = "#1B9E77",
               `10` = "#AE6D1C",
               `20` = "#0072B2",
               #`29` = "#9B58A5",
               `30` = "#D8367D",
               `40` = "#749829",
               `50` = "#E69F00",
               #`55` = "#56B4E9",
               `60` = "#CC79A7",
               `70` = "#666666")
  ) +
  labs(
    x = "Eaten",
    y = "Uneaten",
    title = "Spine Position Island",
    color = "Spine Angle",
    ) +
  plot_theme +
  geom_abline(color = "black", size = 1) +
  facet_wrap(vars(island), scales = "free")

# By island showing populations ###

ggplot(spine_position_pop) +
  aes(x = freq_0, y = freq_1, colour = spine_position) +
  geom_point(shape = "circle", size = 3.3) +
  scale_color_manual(
    values = c(`0` = "#1B9E77",
               `10` = "#AE6D1C",
               `20` = "#0072B2",
               #`29` = "#9B58A5",
               `30` = "#D8367D",
               `40` = "#749829",
               `50` = "#E69F00",
               #`55` = "#56B4E9",
               `60` = "#CC79A7",
               `70` = "#666666")
  ) +
  labs(
    x = "Eaten",
    y = "Uneaten",
    title = "Spine Position Island",
    color = "Spine Angle",
  ) +
  plot_theme +
  geom_abline(color = "black", size = 1) +
  facet_wrap(vars(island), scales = "free")


## Length ####

island_length <- island_fig(length_island, #Dataset
                            length_island$length_mean_1, # x label (eaten)
                            length_island$length_mean_0, # y label (uneaten)
           "Mericarp Length (mm)  ", # Title
           " " # Subtitle (Island or Populations)
           )

pop_length <- island_fig(length_pop,
                         length_pop$length_mean_1,
                         length_pop$length_mean_0,
           "Mericarp Length (mm)  ",
           " "
           )

## Width ####

island_width <- island_fig(width_island,
                           width_island$width_mean_1,
                           width_island$width_mean_0,
           "Mericarp Width (mm)  ",
           " "
)


pop_width <- island_fig(width_pop,
                        width_pop$width_mean_1,
                        width_pop$width_mean_0,
           "Mericarp Width (mm)  ",
           " "
)


## Depth ####

island_depth <- island_fig(depth_island,
                           depth_island$depth_mean_1,
                           depth_island$depth_mean_0,
           "Mericarp Depth (mm)  ",
           " "
)

pop_depth <- island_fig(depth_pop,
                        depth_pop$depth_mean_1,
                        depth_pop$depth_mean_0,
           "Mericarp Depth (mm)  ",
           " "
)


## Longest Spine ####

island_spine <- island_fig(longest_spine_island,
                           longest_spine_island$longest_spine_mean_1,
                           longest_spine_island$longest_spine_mean_0,
           "Spine Length (mm)  ",
           " "
)

pop_spine <- island_fig(longest_spine_pop,
                        longest_spine_pop$longest_spine_mean_1,
                        longest_spine_pop$longest_spine_mean_0,
           "Spine Length (mm)  ",
           " "
)

## Spine tip distance ####

island_tip_distance <- island_fig(tip_distance_island,
                                  tip_distance_island$spine_tip_distance_mean_1,
                                  tip_distance_island$spine_tip_distance_mean_0,
           "Spine Distance (mm)  ",
           " "
)

pop_tip_distance <- island_fig(tip_distance_pop,
                               tip_distance_pop$spine_tip_distance_mean_1,
                               tip_distance_pop$spine_tip_distance_mean_0,
           "Spine Distance (mm)  ",
           " "
)

 
# Individual traits figure ####
# I've used this script to generate 2 plots, there is a set that have
# Baltra Espanola and Seymour in addition to the other islands. Those traits are:
# Length
# Width
# Spine length
# The rest of the traits:
# Depth
# Tip distance 
# These dont have Seymour Espanola and Baltra. 
# I edit this accordingly and later put together all in a single plot using inkscape.
Q1_island_plot <- ggarrange(island_length,
                            island_width,
                            #island_depth,
                            island_spine,
                            #island_tip_distance,
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

