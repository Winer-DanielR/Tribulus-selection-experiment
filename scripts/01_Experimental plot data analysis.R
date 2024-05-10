# Experimental plots data analysis ####
# By: Daniel Reyes Corral
# Date: 11/28/2023
# Load the datasets from the experimental plot folder
# 
# The experimental plots are a series of plots where we placed Tribulus mericarps
# in the field on locations without Tribulus, these mericarps grew in plots of
# XX by YY and were separated by size groups (large, small and two-spines)
# The two spines group are naturally occuring in the source population at a lower
# frequency (12 parental plants).
#  

# Data loading ####

general_set <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Experimental plot/Experimental_plots.csv")
general_set <- arrange(general_set, by = year, parcel, mericarp)

# This dataset is already filtered, we removed the average group treatment

# Data prepping ####
# Year, island, population and survival are going to be grouping factors
general_set <- general_set %>% mutate_at(vars(year, parcel, treatment, eaten, germinated,lower_spine), list(factor))

# Checking outliers in the dataset
# Depth has a couple of outliers larger than 9.
# Longest spine has a couple of outliers larger than 10

data_plot_filter <- filter(general_set, !depth > 9)
data_plot_filter <- filter(data_plot_filter, !longest_spine > 10)

# Create a new column with spine treatment categories
# four and two spines

data_plot_filter$spine_treatment <- recode_factor(data_plot_filter$treatment,
                                      Small = "Four_spines",
                                      Large = "Four_spines",
                                      Two_spines = "Two_spines")

# 4 samples removed that had potential outliers.

# Data summary ####
# Here is a data summary of each treatment

data_plot_filter %>% group_by(treatment,year) %>%
  summarise(mean_treatment = mean(lower_spine, na.rm = T),
            sd_treatment = sd(lower_spine, na.rm = T),
            count_treatmet = n())

# Spine summary

data_plot_filter %>% group_by(treatment, year) %>% count(lower_spine) %>% mutate(percent = n/sum(n))

# The data summary was included in the methods section of the paper.

# Models ####
# These models are per trait, focusing only on the end of the experiment.
# First, we filter the dataset to use only the 2023 data
# then, for size traits (length, depth and width) we use the large and small treatments
# For lower spines, we use all treatments.

end_exp_data <- filter(data_plot_filter, year == "2023") # This dataset filters out the mericarps at the start of the experiment

spine_data <- end_exp_data # I copy the previous dataset to edit the spine categories

spine_data$treatment <- recode_factor(spine_data$treatment,
                                      Small = "Four_spines",
                                      Large = "Four_spines",
                                      Two_spines = "Two_spines")
# This renames the treatment category into four spines for large and small mericarps and two spines
# This dataset is used for the spine analysis

size_data <- filter(end_exp_data, !treatment == "Two_spines") 
# This dataset is used for size analysis

## Size analysis ####
### Length ####
length_model <- glmmTMB(length ~ treatment + (1|parcel),
                        data = size_data,
                        family = "gaussian")

# Model fit
testResiduals(length_model)

summary(length_model)
Anova(length_model)
# Treatment is significant for length at the end of the experiment,
# meaning that length differences are still hold suggesting a strong genetic component
# The difference is at least 1 mm between small and large mericarps the intercept is 7.56

### Depth ####
depth_model <- glmmTMB(depth ~ treatment + (1|parcel),
                        data = size_data,
                        family = "gaussian")

# Model fit
testResiduals(depth_model)

summary(depth_model)
Anova(depth_model)
# Depth is also significant between treatments, that's interesting
# Is barely significant, the difference is -0.29 mm between small and large mericarps

### Width ####
width_model <- glmmTMB(width ~ treatment + (1|parcel),
                       data = size_data,
                       family = "gaussian")

# Model fit
testResiduals(width_model)

summary(width_model)
Anova(width_model)
# Width is not significant, meaning that the differences at the start
# of the experiment were lost at the end. Suggesting not a weak genetic component

## Spine analysis ####
#### Lower spines ####

lower_spine_model <- glmmTMB(lower_spine ~ treatment + (1|parcel),
                             data = spine_data,
                             family = "binomial")

# Model fit
testResiduals(lower_spine_model)

summary(lower_spine_model)
emmeans(lower_spine_model, ~ treatment, type = "response")
Anova(lower_spine_model)
# Spine treatments are significant meaning that at the end of the experiment the presence of four or two spines is maintained

# PCA data preparation ####

data_plot_PCA <- select(data_plot_filter, c(5:7,9,10,12))
# I am selecting the same traits that we used in the point in time PCA
plot_PCA <- prcomp(data_plot_PCA, scale = T)

summary(plot_PCA)

# Eigenvalues
fviz_eig(plot_PCA)

# The loadings show the proportions for each trait
loadings <- plot_PCA$rotation # "rotation" is what R calls the PCA loadings
loadings <- as.data.frame(loadings)

# This is the PCA scores, used for the models later
scores <- plot_PCA$x # "x" is what R calls the species scores
scores <- as.data.frame(scores)

# Combine the scores and the mericarp dataset.
data_plot_filter <- bind_cols(data_plot_filter, scores)
# I exported this dataset to use it for the later questions.


## PCA plots ####
fviz_pca_var(plot_PCA, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             axes = c(2,3)) # plot axes


### Theme individual Biplot ####
biplot2 <- fviz_pca_biplot(plot_PCA,
                           # Fill individuals by groups
                           title = "PCA Experimental Plots",
                           axes = c(1,2),
                           geom.ind = "point",
                           pointshape = c(21),
                           #label = "none",
                           #habillage = mericarp$island,
                           pointsize = 3.2,
                           stroke = 0.8,
                           fill.ind = data_plot_filter$treatment,
                           col.ind = "black",
                           # Color variable by groups
                           legend.title = "Groups",
                           repel = T,
                           col.var = "black", 
                           labelsize = 5,
                           addEllipses = T,
                           palette = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73"),
                           
) + theme_transparent() + 
  scale_color_manual(values = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73")) +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(fill = NA, size = 0))

biplot2


# Individual trait figures ####
# The main plot now is exploratory using length and comparing between
# Treatments and years (start and end of experiment)

## Theme ####

plot_theme <-     theme(axis.line = element_line(linetype = "solid", size = 1), 
                        axis.title = element_text(size = 12),
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

## Density plots ####
### Length ####
length <- data_plot_filter %>%
 filter(!(treatment %in% "Two_spines")) %>%
 ggplot() +
 aes(x = length, fill = treatment) +
 geom_density(adjust = 1L, size = 1) +
 scale_fill_brewer(palette = "Dark2", 
 direction = 1) +
 labs(x = "Length (mm)", y = "Density", title = "Mericarp Length", fill = "Groups") +
 plot_theme +
 #theme(strip.text.y = element_blank()) +
 theme(plot.title = element_text(size = 15L, face = "bold"), axis.title.y = element_text(size = 15L, 
 face = "bold"), axis.title.x = element_text(size = 15L, face = "bold")) +
 facet_grid(vars(year), 
 vars())

### Width ####
width <- data_plot_filter %>%
  filter(!(treatment %in% "Two_spines")) %>%
  ggplot() +
  aes(x = width, fill = treatment) +
  geom_density(adjust = 1L, size = 1) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Width (mm)", y = "Density", title = "Mericarp Width (mm)", fill = "Groups") +
  plot_theme +
  theme(strip.text.y = element_blank()) +
  theme(plot.title = element_text(size = 15L, face = "bold"), axis.title.y = element_text(size = 15L,                                                                            face = "bold"), axis.title.x = element_text(size = 15L, face = "bold")) +
  facet_grid(vars(year), 
             vars())

### Depth ####
depth <- data_plot_filter %>%
  filter(!(treatment %in% "Two_spines")) %>%
  ggplot() +
  aes(x = depth, fill = treatment) +
  geom_density(adjust = 1L, size = 1) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Depth (mm)", y = "Density", title = "Mericarp Depth (mm)", fill = "Groups") +
  plot_theme +
  theme(plot.title = element_text(size = 15L, face = "bold"), axis.title.y = element_text(size = 15L,                                                                            face = "bold"), axis.title.x = element_text(size = 15L, face = "bold")) +
  facet_grid(vars(year), 
             vars())

## Combined density plots ####

Ind_traits <- ggarrange(#length + rremove("ylab") + rremove("xlab"),
                        width + rremove("ylab") + rremove("xlab"),
                        depth + rremove("ylab") + rremove("xlab"),
                        common.legend = T,
                        legend = "right",
                        labels = c("A", "B"),
                        ncol = 2,
                        nrow = 1)

annotate_figure(Ind_traits, left = textGrob("Density", rot = 90, vjust = 1, gp = gpar(cex = 1.5)),
                bottom = textGrob("Trait", gp = gpar(cex = 1.5)))

### Lower spines ####
lower <- ggplot(data_plot_filter) +
 aes(x = lower_spine, fill = spine_treatment) +
 geom_bar(position = "fill", color = "black", size = 0.8) +
 scale_fill_brewer(palette = "Dark2", direction = 1) +
 labs(x = "Lower Spines", 
 y = "Proportion (%)", title = "Spine Treatments", fill = "Spine Treatments") +
 plot_theme +
 theme(plot.title = element_text(size = 19L, 
 face = "bold"), axis.title.y = element_text(size = 14L, face = "bold"), axis.title.x = element_text(size = 14L, 
 face = "bold")) +
 facet_grid(vars(year), vars())



## PC1 Density Mericarp Size
PC1 <- data_plot_filter %>%
  #filter(!(treatment %in% "Two_spines")) %>%
  ggplot() +
  aes(x = PC1, fill = treatment) +
  geom_density(adjust = 1L, size = 1) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Size (PC1)", y = "Density", title = "Mericarp Size (PC1)", fill = "Groups") +
  plot_theme +
  theme(strip.text.y = element_blank()) +
  theme(plot.title = element_text(size = 18L, face = "bold"), axis.title.y = element_text(size = 15L,                                                                            face = "bold"), axis.title.x = element_text(size = 15L, face = "bold")) +
  facet_grid(vars(year), 
             vars())

## PC2 Lower spines (NEG)
PC2 <- data_plot_filter %>%
  #filter(!(treatment %in% "Two_spines")) %>%
  ggplot() +
  aes(x = PC2, fill = treatment) +
  geom_density(adjust = 1L, size = 1) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Lower Spines (PC2)", y = "Density", title = "Lower Spines (PC2)", fill = "Groups") +
  plot_theme +
  theme(strip.text.y = element_blank()) +
  theme(plot.title = element_text(size = 18L, face = "bold"), axis.title.y = element_text(size = 15L,                                                                            face = "bold"), axis.title.x = element_text(size = 15L, face = "bold")) +
  facet_grid(vars(year), 
             vars())
# PC2 is mostly associated with the presence of lower spines towards the negative end of the axis.
# In this one I've included the two spine treatment that lacks lower spines and you can see it still holds
# 
## PC3 Spine position and Defense
PC3 <- data_plot_filter %>%
  #filter(!(treatment %in% "Two_spines")) %>%
  ggplot() +
  aes(x = PC3, fill = treatment) +
  geom_density(adjust = 1L, size = 1) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Spine Position and Defense (PC3)", y = "Density", title = "Spine Position and Defense", fill = "Groups") +
  plot_theme +
  theme(plot.title = element_text(size = 18L, face = "bold"), axis.title.y = element_text(size = 15L,                                                                            face = "bold"), axis.title.x = element_text(size = 15L, face = "bold")) +
  facet_grid(vars(year), 
             vars())
# PC3 is associated with spine position and spine length (POS and NEG ends respectively)
# 
# 
# 

## Combined PCA plots ####
PCA_density <- ggarrange(PC1 + rremove("ylab") + rremove("xlab"),
                        PC2 + rremove("ylab") + rremove("xlab"),
                        PC3 + rremove("ylab") + rremove("xlab"),
                        common.legend = T,
                        legend = "right",
                        labels = c("A", "B", "C"),
                        ncol = 3,
                        nrow = 1)
annotate_figure(PCA_density, left = textGrob("Density", rot = 90, vjust = 1, gp = gpar(cex = 1.5)),
                bottom = textGrob("PC Axis", gp = gpar(cex = 1.5)))
