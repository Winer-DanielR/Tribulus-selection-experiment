# Question 3: Figures ####
# 
# Date: 19 January, 2023
# By: Daniel Reyes
# 
# Goal: ####
# This script contains the format and code the figure of question 3.
# Selection plotted with bioclimate variables.
# # 
# Description: #### 
# The question is how does the environment effects mericarp selection. To do this I used the World Clim
# bioclimate database and extract the climate variablesÑ
# Annual Precipitation (BIO_1)
# Temperature Seasonality (Bio_4)
# Annual Precipitation BIO_12
# Precipitation SeasonalityBIO_15
# Finch Beak (The presence/absence of large beak finches on islands)
# 
# These variables were used in our previous paper of mericarp phenotypic divergence.
# Since we are using the same locations I edited the database and added this information.
# 
# 1/19/2023: I've only updated the populations scripts for now. Later I need to add bioclimate variables to the island datasets.
# 2/3/2023: Incorportated PC scores.
# 
# Data loading ####
## Islands ####
# depth_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_island.csv")
# length_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_island.csv")
# longest_spine_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_island.csv")
# tip_distance_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_island.csv")
# width_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_island.csv")
# lower_spine_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_island.csv")
# spine_position_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_island.csv")

## Populations ####
depth_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_population.csv")
length_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_population.csv")
longest_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_population.csv")
tip_distance_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_population.csv")
width_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_population.csv")
lower_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_pop.csv")
spine_position_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_pop.csv")

## PCA Populations ####
pca_means_Q3 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_populations_bioclimate.csv")


# Data preparation ####
# Making char into factors (island, populations)
# 
### Island ####
# depth_island <- depth_island %>% mutate_at(vars(island), list(factor))
# length_island <- length_island %>% mutate_at(vars(island), list(factor))
# longest_spine_island <- longest_spine_island %>% mutate_at(vars(island), list(factor))
# lower_spine_island  <- lower_spine_island %>% mutate_at(vars(island, lower_spine), list(factor))
# spine_position_island <- spine_position_island %>% mutate_at(vars(island, spine_position), list(factor))
# tip_distance_island <- tip_distance_island %>% mutate_at(vars(island), list(factor))
# width_island <- width_island %>% mutate_at(vars(island), list(factor))


### Populations #####
depth_pop <- depth_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
length_pop <- length_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
longest_spine_pop  <- longest_spine_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
lower_spine_pop <- lower_spine_pop %>% mutate_at(vars(island, population, lower_spine, finch_beak), list(factor))
spine_position_pop <- spine_position_pop %>% mutate_at(vars(island, population, spine_position, finch_beak), list(factor))
tip_distance_pop <- tip_distance_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
width_pop <- width_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))

### PCA populations ####
pca_means_Q3 <- pca_means_Q3 %>% mutate_at(vars(island, population, finch_beak), list(factor))
str(pca_means_Q3)
pca_means_Q3 <- na.omit(pca_means_Q3)

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
{ggplot(dataset, aes(x = x, y = y, colour = island, shape = island, fill = island)) + #For the function to work the colors need to be defined here.
    geom_point(size = 3.5, stroke = 1) + #Type of plot
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
      x = "Bioclimate Variable",
      y = "Selection (Uneaten - Eaten)",
      title = title,
      subtitle = subtitle,
      #color = "Islands"
      fill = "Islands"
    ) +
    plot_theme +
    geom_smooth(color = "black", size = 1, method = "glm",aes(group=1))
  #geom_abline(color = "black", size = 1) # Abline is a 1:1 line!
}

# Plots ####
## PCA ####
### Size ####
#### Bioclimate PC1 ####
pca_size_bio1 <- island_fig(pca_means_Q3,
                       pca_means_Q3$PC1_bioclimate,
                       pca_means_Q3$S_Size,
                       "Mericarp Size (PC1) ",
                       " "
)

# #### Bioclimate PC2 ####
# pca_size_bio4 <- island_fig(pca_means_Q3,
#                        pca_means_Q3$PC2_bioclimate,
#                        pca_means_Q3$S_Size,
#                        "Bioclimate (PC2)  ",
#                        " "
# )
# 
# #### Bioclimate PC3 ####
# pca_size_bio12 <- island_fig(pca_means_Q3,
#                        pca_means_Q3$PC3_bioclimate,
#                        pca_means_Q3$S_Size,
#                        "Bioclimate (PC3)  ",
#                        " "
# )
# 
# #### BIO15 ####
# pca_size_bio15 <- island_fig(pca_means_Q3,
#                        pca_means_Q3$Bio_15,
#                        pca_means_Q3$S_Size,
#                        "Precipitation Seasonality  ",
#                        " "
# )

#### Finch Beak ####
pca_size_beak <- island_fig(pca_means_Q3,
                             pca_means_Q3$finch_beak,
                             pca_means_Q3$S_Size,
                             "Mericarp Size (PC1) ",
                             " "
)

### Combined PCA plots ####
Q3_pca_size <- ggarrange(pca_size_beak + rremove("ylab") + rremove("xlab"),
                         pca_Defense_beak + rremove("ylab") + rremove("xlab"),
                         pca_Position_beak + rremove("ylab") + rremove("xlab"),
                         #pca_size_bio4 + rremove("ylab") + rremove("xlab"),
                         #pca_size_bio12 + rremove("ylab") + rremove("xlab"),
                         #pca_size_bio15 + rremove("ylab") + rremove("xlab"),
                         #pca_size_beak + rremove("ylab") + rremove("xlab"),
                         common.legend = T,
                         legend = "right",
                         labels = c("A", "B", 
                                    "C"
                                    #"D"
                                    #, "E"
                                    ),
                         ncol = 3,
                         nrow = 1)

annotate_figure(Q3_pca_size, left = textGrob("Selection (Uneaten - Eaten)", 
                                             rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Finch Beak", gp = gpar(cex = 1.1)))

### Defense ####

#### BIO1 ####
pca_Defense_bio1 <- island_fig(pca_means_Q3,
                            pca_means_Q3$PC1_bioclimate,
                            pca_means_Q3$S_Defense,
                            "Mericarp Defense (PC2) ",
                            " "
)

# #### BIO4 ####
# pca_Defense_bio4 <- island_fig(pca_means_Q3,
#                             pca_means_Q3$Bio_4,
#                             pca_means_Q3$S_Defense,
#                             "Temperature Seasonality  ",
#                             " "
# )
# 
# #### BIO12 ####
# pca_Defense_bio12 <- island_fig(pca_means_Q3,
#                              pca_means_Q3$Bio_12,
#                              pca_means_Q3$S_Defense,
#                              "Annual Precipitation  ",
#                              " "
# )
# 
# #### BIO15 ####
# pca_Defense_bio15 <- island_fig(pca_means_Q3,
#                              pca_means_Q3$Bio_15,
#                              pca_means_Q3$S_Defense,
#                              "Precipitation Seasonality  ",
#                              " "
# )

#### Finch Beak ####
pca_Defense_beak <- island_fig(pca_means_Q3,
                            pca_means_Q3$finch_beak,
                            pca_means_Q3$S_Defense,
                            "Mericarp Defense (PC2)  ",
                            " "
)

### Combined PCA plots ####
Q3_pca_Defense <- ggarrange(pca_Defense_bio1 + rremove("ylab") + rremove("xlab"),
                         #pca_Defense_bio4 + rremove("ylab") + rremove("xlab"),
                         #pca_Defense_bio12 + rremove("ylab") + rremove("xlab"),
                         #pca_Defense_bio15 + rremove("ylab") + rremove("xlab"),
                         pca_Defense_beak + rremove("ylab") + rremove("xlab"),
                         common.legend = T,
                         legend = "right",
                         labels = c("A", 
                                    "B" 
                                    #"C",
                                    #"D", 
                                    #"E"
                                    ),
                         ncol = 2,
                         nrow = 1)

annotate_figure(Q3_pca_Defense, left = textGrob("Selection (Uneaten - Eaten)", 
                                             rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))



### Position ####


#### BIO1 ####
pca_Position_bio1 <- island_fig(pca_means_Q3,
                               pca_means_Q3$PC1_bioclimate,
                               pca_means_Q3$S_Position,
                               "Mericarp Position (PC3)  ",
                               " "
)

# #### BIO4 ####
# pca_Position_bio4 <- island_fig(pca_means_Q3,
#                                pca_means_Q3$Bio_4,
#                                pca_means_Q3$S_Position,
#                                "Temperature Seasonality  ",
#                                " "
# )
# 
# #### BIO12 ####
# pca_Position_bio12 <- island_fig(pca_means_Q3,
#                                 pca_means_Q3$Bio_12,
#                                 pca_means_Q3$S_Position,
#                                 "Annual Precipitation  ",
#                                 " "
# )
# 
# #### BIO15 ####
# pca_Position_bio15 <- island_fig(pca_means_Q3,
#                                 pca_means_Q3$Bio_15,
#                                 pca_means_Q3$S_Position,
#                                 "Precipitation Seasonality  ",
#                                 " "
# )

#### Finch Beak ####
pca_Position_beak <- island_fig(pca_means_Q3,
                               pca_means_Q3$finch_beak,
                               pca_means_Q3$S_Position,
                               "Mericarp Position (PC3) ",
                               " "
)

### Combined PCA plots ####
Q3_pca_Position <- ggarrange(pca_Position_bio1 + rremove("ylab") + rremove("xlab"),
                            pca_Position_bio4 + rremove("ylab") + rremove("xlab"),
                            pca_Position_bio12 + rremove("ylab") + rremove("xlab"),
                            pca_Position_bio15 + rremove("ylab") + rremove("xlab"),
                            pca_Position_beak + rremove("ylab") + rremove("xlab"),
                            common.legend = T,
                            legend = "right",
                            labels = c("A", "B", "C",
                                       "D", "E"),
                            ncol = 3,
                            nrow = 2)

annotate_figure(Q3_pca_Position, left = textGrob("Selection (Uneaten - Eaten)", 
                                                rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))

## Lower Spine ####
# Lower spine plots are based on their counts. They don't use the function above.

# ggplot(lower_spine_island) +
#   aes(
#     x = S_lower_spine,
#     y = freq_all,
#     colour = island,
#     shape = lower_spine
#   ) +
#   geom_point(size = 3.5) +
#   scale_color_manual(
#     values = c("#D55E00",
#                         "#E69F00",
#                         "#009E73",
#                         "#0072B2",
#                         "#56B4E9",
#                         "#CC79A7",
#                         "#666666")) +
#                           labs(
#                             x = "Selection (Uneaten - Eaten)",
#                             y = "Total frequency Island",
#                             title = "Lower Spines",
#                             color = "Islands",
#                             shape = "Presence of lower spines"
#                           ) +
#   plot_theme +
#   geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
#   #geom_abline(color = "black", size = 1)

#### All populations in a plot ####
##### BIO_1 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_1,
    y = S_lower_spine,
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
                            x = "Annual Temperature",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines - BIO_1",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
  #geom_abline(color = "black", size = 1)

##### BIO_4 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_4,
    y = S_lower_spine,
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
                            x = "Temperature Seasonality",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines - BIO_4",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
#geom_abline(color = "black", size = 1)
#

##### BIO_12 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_12,
    y = S_lower_spine,
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
                            x = "Annual Precipitation",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines - BIO_12",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
#geom_abline(color = "black", size = 1)

##### BIO_15 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_15,
    y = S_lower_spine,
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
                            x = "Precipitation Seasonality",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines - BIO_15",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
#geom_abline(color = "black", size = 1)

##### Finch Beak ####
ggplot(lower_spine_pop) +
  aes(
    x = finch_beak,
    y = S_lower_spine,
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
                            x = "Finch Beak",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines - Finch Beak",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
#geom_abline(color = "black", size = 1)

#### All populations separated ####
##### BIO_1 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_1,
    y = S_lower_spine,
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
                            x = "Annual Temperature",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines per population - BIO_1",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme + theme(axis.line = element_line()) +
  #geom_abline(color = "black", size = 0.8) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")


##### BIO_4 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_4,
    y = S_lower_spine,
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
                            x = "Temperature Seasonality",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines per population - BIO_4",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme + theme(axis.line = element_line()) +
  #geom_abline(color = "black", size = 0.8) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")

##### BIO_12 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_12,
    y = S_lower_spine,
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
                            x = "Annual Precipitation",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines per population - BIO_12",
                            color = "Islands",
                            shape = "Presence of lower spines"
                          ) +
  plot_theme + theme(axis.line = element_line()) +
  #geom_abline(color = "black", size = 0.8) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")

##### BIO_15 ####
ggplot(lower_spine_pop) +
  aes(
    x = Bio_15,
    y = S_lower_spine,
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
                            x = "Precipitation Seasonality",
                            y = "Selection (Uneaten - Eaten)",
                            title = "Lower Spines per population - BIO_15",
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

### By island separated ####
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

### By island showing populations #####
#### BIO_1 ####
ggplot(spine_position_pop) +
  aes(x = Bio_1, 
      y = S_spine_position, 
      colour = spine_position) +
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
    x = "Annual Temperature",
    y = "Selection (Uneaten - Eaten)",
    title = "Spine Position Island - BIO1",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")


#### BIO_4 ####
ggplot(spine_position_pop) +
  aes(x = Bio_4, 
      y = S_spine_position, 
      colour = spine_position) +
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
    x = "Temperature Seasonality",
    y = "Selection (Uneaten - Eaten)",
    title = "Spine Position Island - BIO4",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")

#### BIO_12 ####
ggplot(spine_position_pop) +
  aes(x = Bio_12, 
      y = S_spine_position, 
      colour = spine_position) +
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
    x = "Annual Precipitation",
    y = "Selection (Uneaten - Eaten)",
    title = "Spine Position Island - BIO12",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")


#### BIO_15 ####
ggplot(spine_position_pop) +
  aes(x = Bio_15, 
      y = S_spine_position, 
      colour = spine_position) +
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
    x = "Precipitation Seasonality",
    y = "Selection (Uneaten - Eaten)",
    title = "Spine Position Island - BIO15",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1)) +
  facet_wrap(vars(island), scales = "free")


#### Finch Beak ####
ggplot(spine_position_pop) +
  aes(x = finch_beak, 
      y = S_spine_position, 
      colour = spine_position) +
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
    x = "Annual Temperature",
    y = "Selection (Uneaten - Eaten)",
    title = "Spine Position Island - Finch Beak",
    color = "Spine Angle",
  ) +
  plot_theme +
  #geom_abline(color = "black", size = 1) +
  geom_smooth(color = "black", size = 1, method = "lm",aes(group=1))
  #facet_wrap(vars(island), scales = "free")


## Depth ####
### BIO1 ####
island_depth <- island_fig(depth_island,
                           depth_island$Bio_1,
                           depth_island$S_depth,
                           "Depth (mm) - Annual Temperature ",
                           " ")

pop_depth_Bio1 <- island_fig(depth_pop,
                        depth_pop$Bio_1,
                        depth_pop$S_depth,
                        "Depth (mm) - Annual Temperature ",
                        " "
)

### BIO4 ####
pop_depth_Bio4 <- island_fig(depth_pop,
                        depth_pop$Bio_4,
                        depth_pop$S_depth,
                        "Temperature Seasonality ",
                        " "
)

### BIO12 ####
pop_depth_Bio12 <- island_fig(depth_pop,
                             depth_pop$Bio_12,
                             depth_pop$S_depth,
                             "Annual Precipitation ",
                             " "
)

### BIO15 ####
pop_depth_Bio15 <- island_fig(depth_pop,
                             depth_pop$Bio_15,
                             depth_pop$S_depth,
                             "Precipitation Seasonality ",
                             " "
)

### Finch Beak ####
pop_depth_finch <- island_fig(depth_pop,
                             depth_pop$finch_beak,
                             depth_pop$S_depth,
                             "Finch Beak ",
                             " "
)

### Combine depth plots ####

bioclim_depth <- ggarrange(pop_depth_Bio1 + rremove("ylab") + rremove("xlab"),
                           pop_depth_Bio4 + rremove("ylab") + rremove("xlab"),
                           pop_depth_Bio12 + rremove("ylab") + rremove("xlab"),
                           pop_depth_Bio15 + rremove("ylab") + rremove("xlab"),
                           pop_depth_finch + rremove("ylab") + rremove("xlab"),
                           common.legend = T,
                           legend = "right",
                           labels = c("A", "B", "C",
                                      "D", "E"),
                           ncol = 3,
                           nrow = 2)

annotate_figure(bioclim_depth, left = textGrob("Selection (Uneaten - Eaten)", 
                                               rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))

## Length ####

island_length <- island_fig(length_island, #Dataset
                            length_island$S_length, # x label (eaten)
                            length_island$mean_all, # y label (uneaten)
                            "Mericarp Length (mm)  ", # Title
                            " " # Subtitle (Island or Populations)
)

### BIO1 ####
pop_length_Bio1 <- island_fig(length_pop,
                         length_pop$Bio_1,
                         length_pop$S_length,
                         "Length (mm) - Annual Temperature  ",
                         " "
)

### BIO4 ####
pop_length_Bio4 <- island_fig(length_pop,
                              length_pop$Bio_4,
                              length_pop$S_length,
                              "Temperature Seasonality  ",
                              " "
)

### BIO12 ####
pop_length_Bio12 <- island_fig(length_pop,
                              length_pop$Bio_12,
                              length_pop$S_length,
                              "Annual Precipitation  ",
                              " "
)

### BIO15 ####
pop_length_Bio15 <- island_fig(length_pop,
                              length_pop$Bio_15,
                              length_pop$S_length,
                              "Precipitation Seasonality  ",
                              " "
)

### Finch Beak ####
pop_length_finch <- island_fig(length_pop,
                              length_pop$finch_beak,
                              length_pop$S_length,
                              "Finch Beak  ",
                              " "
)

### Combine depth plots ####

bioclim_length <- ggarrange(pop_length_Bio1 + rremove("ylab") + rremove("xlab"),
                            pop_length_Bio4 + rremove("ylab") + rremove("xlab"),
                            pop_length_Bio12 + rremove("ylab") + rremove("xlab"),
                            pop_length_Bio15 + rremove("ylab") + rremove("xlab"),
                            pop_length_finch + rremove("ylab") + rremove("xlab"),
                           common.legend = T,
                           legend = "right",
                           labels = c("A", "B", "C",
                                      "D", "E"),
                           ncol = 3,
                           nrow = 2)

annotate_figure(bioclim_length, left = textGrob("Selection (Uneaten - Eaten)", 
                                               rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))


## Width ####

island_width <- island_fig(width_island,
                           width_island$S_width,
                           width_island$mean_all,
                           "Mericarp Width (mm)  ",
                           " "
)

### BIO1 ####
pop_width_Bio1 <- island_fig(width_pop,
                        width_pop$Bio_1,
                        width_pop$S_width,
                        "Width (mm) - Annual Temperature  ",
                        " "
)

### BIO4 ####
pop_width_Bio4 <- island_fig(width_pop,
                             width_pop$Bio_4,
                             width_pop$S_width,
                             "Temperature Seasonality  ",
                             " "
)

### BIO12 ####
pop_width_Bio12 <- island_fig(width_pop,
                             width_pop$Bio_12,
                             width_pop$S_width,
                             "Annual Precipitation  ",
                             " "
)

### BIO15 ####
pop_width_Bio15 <- island_fig(width_pop,
                             width_pop$Bio_15,
                             width_pop$S_width,
                             "Precipitation Seasonality  ",
                             " "
)


### Finch Beak ####
pop_width_finch <- island_fig(width_pop,
                             width_pop$finch_beak,
                             width_pop$S_width,
                             "Finch Beak  ",
                             " "
)


### Combine width plots ####

bioclim_width <- ggarrange(pop_width_Bio1 + rremove("ylab") + rremove("xlab"),
                           pop_width_Bio4 + rremove("ylab") + rremove("xlab"),
                           pop_width_Bio12 + rremove("ylab") + rremove("xlab"),
                           pop_width_Bio15 + rremove("ylab") + rremove("xlab"),
                           pop_width_finch + rremove("ylab") + rremove("xlab"),
                            common.legend = T,
                            legend = "right",
                            labels = c("A", "B", "C",
                                       "D", "E"),
                            ncol = 3,
                            nrow = 2)

annotate_figure(bioclim_width, left = textGrob("Selection (Uneaten - Eaten)", 
                                                rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))


## Longest Spine ####

island_spine <- island_fig(longest_spine_island,
                           longest_spine_island$S_longest_spine,
                           longest_spine_island$mean_all,
                           "Spine Length (mm)  ",
                           " "
)

### BIO1 ####
pop_spine_Bio1 <- island_fig(longest_spine_pop,
                        longest_spine_pop$Bio_1,
                        longest_spine_pop$S_longest_spine,
                        "Spine Length (mm) - Annual Temperature  ",
                        " "
)

### BIO4 ####
pop_spine_Bio4 <- island_fig(longest_spine_pop,
                             longest_spine_pop$Bio_4,
                             longest_spine_pop$S_longest_spine,
                             "Temperature Seasonality  ",
                             " "
)

### BIO12 ####
pop_spine_Bio12 <- island_fig(longest_spine_pop,
                             longest_spine_pop$Bio_12,
                             longest_spine_pop$S_longest_spine,
                             "Annual Precipitation  ",
                             " "
)

### BIO15 ####
pop_spine_Bio15 <- island_fig(longest_spine_pop,
                             longest_spine_pop$Bio_15,
                             longest_spine_pop$S_longest_spine,
                             "Precipitation Seasonality  ",
                             " "
)

### Finch Beak ####
pop_spine_finch <- island_fig(longest_spine_pop,
                              longest_spine_pop$finch_beak,
                              longest_spine_pop$S_longest_spine,
                              "Finch Beak  ",
                              " "
)

### Combine longest spine plots ####

bioclim_spine <- ggarrange(pop_spine_Bio1 + rremove("ylab") + rremove("xlab"),
                           pop_spine_Bio4 + rremove("ylab") + rremove("xlab"),
                           pop_spine_Bio12 + rremove("ylab") + rremove("xlab"),
                           pop_spine_Bio15 + rremove("ylab") + rremove("xlab"),
                           pop_spine_finch + rremove("ylab") + rremove("xlab"),
                           common.legend = T,
                           legend = "right",
                           labels = c("A", "B", "C",
                                      "D", "E"),
                           ncol = 3,
                           nrow = 2)

annotate_figure(bioclim_spine, left = textGrob("Selection (Uneaten - Eaten)", 
                                               rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))



## Spine tip distance ####

island_tip_distance <- island_fig(tip_distance_island,
                                  tip_distance_island$S_spine_tip_distance,
                                  tip_distance_island$mean_all,
                                  "Spine Distance (mm)  ",
                                  " "
)

### BIO1 ####
pop_tip_distance_Bio1 <- island_fig(tip_distance_pop,
                               tip_distance_pop$Bio_1,
                               tip_distance_pop$S_spine_tip_distance,
                               "Spine Distance (mm) - Annual Temperature  ",
                               " "
)

### BIO4 ####
pop_tip_distance_Bio4 <- island_fig(tip_distance_pop,
                                    tip_distance_pop$Bio_4,
                                    tip_distance_pop$S_spine_tip_distance,
                                    "Temperature Seasonality  ",
                                    " "
)

### BIO12 ####
pop_tip_distance_Bio12 <- island_fig(tip_distance_pop,
                                    tip_distance_pop$Bio_12,
                                    tip_distance_pop$S_spine_tip_distance,
                                    "Annual Precipitation  ",
                                    " "
)

### BIO15 ####
pop_tip_distance_Bio15 <- island_fig(tip_distance_pop,
                                    tip_distance_pop$Bio_15,
                                    tip_distance_pop$S_spine_tip_distance,
                                    "Precipitation Seasonality  ",
                                    " "
)

### Finch Beak ####
pop_tip_distance_finch <- island_fig(tip_distance_pop,
                                    tip_distance_pop$finch_beak,
                                    tip_distance_pop$S_spine_tip_distance,
                                    "Finch Beak  ",
                                    " "
)

### Combine longest spine plots ####

bioclim_tip_distance <- ggarrange(pop_tip_distance_Bio1 + rremove("ylab") + rremove("xlab"),
                           pop_tip_distance_Bio4 + rremove("ylab") + rremove("xlab"),
                           pop_tip_distance_Bio12 + rremove("ylab") + rremove("xlab"),
                           pop_tip_distance_Bio15 + rremove("ylab") + rremove("xlab"),
                           pop_tip_distance_finch + rremove("ylab") + rremove("xlab"),
                           common.legend = T,
                           legend = "right",
                           labels = c("A", "B", "C",
                                      "D", "E"),
                           ncol = 3,
                           nrow = 2)

annotate_figure(bioclim_tip_distance, left = textGrob("Selection (Uneaten - Eaten)", 
                                               rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Bioclimate Variable", gp = gpar(cex = 1.1)))
