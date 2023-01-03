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

# Plot theme and function ####
## Theme ####

plot_theme <-     theme(axis.line = element_line(linetype = "solid", size = 1.5), 
                        axis.title = element_text(size = 12, 
                                                  #face = "bold"
                                                  ),
                        axis.text = element_text(size = 12), 
                        axis.text.x = element_text(size = 11), 
                        plot.title = element_text(size = 12, face = "bold", hjust = 0),
                        text = element_text(family = "Noto Sans"),
                        legend.text = element_text(size = 11), 
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.position = "right",
                        panel.background = element_rect(fill = NA),
                        legend.background = element_rect(fill = NA, size = 0)
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
    x = "Eaten",
    y = "Uneaten",
    title = title,
    subtitle = subtitle,
    color = "Islands"
  ) +
  plot_theme +
  geom_abline(color = "black", size = 1) # Abline is a 1:1 line!
  }

# Plots ####
## Length ####

island_length <- island_fig(length_means_island, #Dataset
           length_means_island$length_mean_1, # x label (eaten)
           length_means_island$length_mean_0, # y label (uneaten)
           "Mericarp Length (mm)  ", # Title
           " " # Subtitle (Island or Populations)
           )

pop_length <- island_fig(length_means_pop,
           length_means_pop$length_mean_1,
           length_means_pop$length_mean_0,
           "Mericarp Length (mm)  ",
           " "
           )

## Width ####

island_width <- island_fig(width_means_island,
           width_means_island$width_mean_1,
           width_means_island$width_mean_0,
           "Mericarp Width (mm)  ",
           " "
)


pop_width <- island_fig(width_means_pop,
           width_means_pop$width_mean_1,
           width_means_pop$width_mean_0,
           "Mericarp Width (mm)  ",
           " "
)


## Depth ####

island_depth <- island_fig(depth_means_island,
           depth_means_island$depth_mean_1,
           depth_means_island$depth_mean_0,
           "Mericarp Depth (mm)  ",
           " "
)

pop_depth <- island_fig(depth_means_pop,
           depth_means_pop$depth_mean_1,
           depth_means_pop$depth_mean_0,
           "Mericarp Depth (mm)  ",
           " "
)


## Longest Spine ####

island_spine <- island_fig(longest_spine_means_island,
           longest_spine_means_island$longest_spine_mean_1,
           longest_spine_means_island$longest_spine_mean_0,
           "Spine Length (mm)  ",
           " "
)

pop_spine <- island_fig(longest_spine_means_pop,
           longest_spine_means_pop$longest_spine_mean_1,
           longest_spine_means_pop$longest_spine_mean_0,
           "Spine Length (mm)  ",
           " "
)

## Spine tip distance ####

island_tip_distance <- island_fig(spine_tip_distance_means_island,
           spine_tip_distance_means_island$spine_tip_distance_mean_1,
           spine_tip_distance_means_island$spine_tip_distance_mean_0,
           "Spine Distance (mm)  ",
           " "
)

pop_tip_distance <- island_fig(spine_tip_distance_means_pop,
           spine_tip_distance_means_pop$spine_tip_distance_mean_1,
           spine_tip_distance_means_pop$spine_tip_distance_mean_0,
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

