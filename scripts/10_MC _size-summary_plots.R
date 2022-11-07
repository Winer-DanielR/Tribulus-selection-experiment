#### Mark Recapture summary script ####
# By: Daniel Reyes Corral
# In this script I want to create simple boxplots showing the size variation between 
# treatment groups for the mark recapture experiment.
# First I will use the datasets from script 06 and use those to create the plots

# This script will use each dataset per island and per year then I will join
# the plots into a single one.


library(dplyr)
library(ggplot2)

# Floreana 2018 size plot ####
MC_Floreana_18 %>%
 filter(time %in% "0") %>%
 ggplot() +
 aes(x = size, y = length) +
 geom_boxplot(fill = "#E69F00", size = 1) +
 labs(x = "Mericarp Size", 
 y = "Length (mm)", title = "Floreana 2018") +
 theme(axis.line = element_line(linetype = "solid", size = 1.5), 
      axis.title = element_text(size = 14, face = "bold", family = "Noto Sans"), 
      axis.text = element_text(size = 14, family = "Noto Sans"),
      axis.text.x = element_text(size = 12), 
      plot.title = element_text(size = 16, family = "Noto Sans", face = "bold"),
      text = element_text(family = "Noto Sans"),
      panel.background = element_rect(fill = NA))

# Isabela 2018 size plot ####
MC_Isabela_18 %>%
  filter(time %in% "0") %>%
  ggplot() +
  aes(x = size, y = length) +
  geom_boxplot(fill = "#56B4E9", size = 1) +
  labs(x = "Mericarp Size", 
       y = "Length (mm)", title = "Isabela 2018") +
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold", family = "Noto Sans"), 
        axis.text = element_text(size = 14, family = "Noto Sans"),
        axis.text.x = element_text(size = 12), 
        plot.title = element_text(size = 16, family = "Noto Sans", face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA))

# Santa Cruz 2018 ####
MC_StCruz_18 %>%
  filter(time %in% "0") %>%
  ggplot() +
  aes(x = size, y = length) +
  geom_boxplot(fill = "#009E73", size = 1) +
  labs(x = "Mericarp Size", 
       y = "Length (mm)", title = "Santa Cruz 2018") +
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold", family = "Noto Sans"), 
        axis.text = element_text(size = 14, family = "Noto Sans"),
        axis.text.x = element_text(size = 12), 
        plot.title = element_text(size = 16, family = "Noto Sans", face = "bold"),
        text = element_text(family = "Noto Sans"),
        panel.background = element_rect(fill = NA))

