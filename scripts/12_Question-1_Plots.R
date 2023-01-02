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


# Plot theme and function ####
## Theme ####

plot_theme <-     theme(axis.line = element_line(linetype = "solid", size = 1.5), 
                        axis.title = element_text(size = 14, face = "bold"),
                        axis.text = element_text(size = 12), 
                        axis.text.x = element_text(size = 11), 
                        plot.title = element_text(size = 16, face = "bold", hjust = 0),
                        text = element_text(family = "Noto Sans"),
                        legend.text = element_text(size = 12), 
                        legend.title = element_text(size = 14, face = "bold"),
                        legend.position = "right",
                        panel.background = element_rect(fill = NA),
                        legend.background = element_rect(fill = NA, size = 0))


## Functions ####
# This function is for the plots per trait grouped by island.

island_fig <- function(dataset, x, y, title)
  {ggplot(dataset, aes(x = x, y = y, colour = island)) + #For the function to work the colors need to be defined here.
  geom_point(shape = "circle", size = 3.5) + #Type of plot
  scale_color_manual(                        
    values = c(Baltra = "#E69F00",
               Espanola = "#56B4E9",
               Floreana = "#009E73",
               Isabela = "#0072B2",
               San.Cristobal = "#D55E00",
               Santa.Cruz = "#CC79A7",
               Seymour.Norte = "#666666")) + # Set colors
  labs(
    x = "Eaten",
    y = "Uneaten",
    title = title,
    subtitle = "Means per island",
    color = "Islands"
  ) +
  plot_theme +
  geom_abline(color = "black", size = 1) # Abline is a 1:1 line!
  }

# Plots ####
## Length ####

island_fig(length_means_island,
           length_means_island$length_mean_1,
           length_means_island$length_mean_0,
           "Mericarp Length (mm)")

