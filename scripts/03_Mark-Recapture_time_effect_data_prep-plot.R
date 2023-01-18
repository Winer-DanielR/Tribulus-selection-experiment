# Mericarps time effect. Mark recapture ####
# 
# Goal: Prepare the data to compare start of the experiment and the end. ####
# This was done using mericarps recovered from Santa Cruz island. 
# I missed the information on the other two.
# This analysis was to check if there was any difference between the mericarps 
# at the beginning of the mark recapture experiment and at the end. In terms of
# trait changes, are they reduced due to age?

# The series of plots shown are boxplots comparing between the two main treatment groups:
# Large and small, between the start of the experiment 2018 and the end 2019.

# There is no model of this to check for significance. ####

## Load dataset ####
# The time effect measurements dataset was a small dataset from Santa Cruz, where I measured the mericarps at the end of the experiment
# Unfortunately I did not measure the mericarps on Floreana and Isabela. We discarted the mericarps before collecting that data.

time_effect <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Time effect measurements.csv")
time_effect <- as_tibble(time_effect)

## Data preparations ####
# Changed variables to factors
time_effect <- time_effect %>% mutate_at(vars(year, island, treatment, 
                                              size, color, `mark position`,
                                              lower_spine, plate), list(factor))
str(time_effect)

# Changed the name of the columns to match the start and end of the experiment
time_effect <- time_effect %>% transmute(year, treatment,
                                         size, color, mark_position = `mark position`,
                                         mericarps, plate, length, width,
                                         depth, spine_tip_distance, longest_spine,
                                         lower_spine, spine_position)
head(time_effect)
names(time_effect)

## Plots per trait #####

#Length
length_plot <- ggplot(time_effect) +
 aes(x = year, y = length, fill = size) +
 geom_boxplot() +
 scale_fill_hue() +
 labs(x = "Year", y = "Size (mm)", title = "Time effect. Length") +
 theme_minimal() +
 facet_wrap(vars(size)) + theme(axis.title = element_text(size = 10), 
    axis.text = element_text(size = 5), 
    axis.text.y = element_text(size = 5))

#Width
width_plot <- ggplot(time_effect) +
 aes(x = year, y = width, fill = size) +
 geom_boxplot() +
 scale_fill_hue() +
 theme_minimal() +
 facet_wrap(vars(size)) + theme(axis.title = element_text(size = 10), 
    axis.text = element_text(size = 5)) +labs(title = "Time effect. Width", x = "Year", 
    y = "Width (mm)")

#Depth
depth_plot <- ggplot(time_effect) +
  aes(x = year, y = depth, fill = size) +
  geom_boxplot() + 
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(size)) + theme(axis.title = element_text(size = 10),
                                 axis.text = element_text(size = 5)) +
  labs(title = "Time effect. Depth", x = "Year", y = "Depth (mm)")

#Spine tip distance
spine_plot <- ggplot(time_effect) +
  aes(x = year, y = spine_tip_distance, fill = size) +
  geom_boxplot() + 
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(size)) + theme(axis.title = element_text(size = 10),
                                 axis.text = element_text(size = 5)) +
  labs(title = "Time effect. Spine tip distance", x = "Year", y = "Spine tip distance (mm)")

#Longest spine
longest_plot <- ggplot(time_effect) +
  aes(x = year, y = longest_spine, fill = size) +
  geom_boxplot() + 
  scale_fill_hue() +
  theme_minimal() +
  facet_wrap(vars(size)) + theme(axis.title = element_text(size = 10),
                                 axis.text = element_text(size = 5)) +
  labs(title = "Time effect. Longest spine", x = "Year", y = "Longest spine (mm)")

# Mericarps time effect. Mark recapture Plot ####
# The dataset used is a summary of the dataset loaded above.

mericarp_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Time effect summary.csv")
str(mericarp_time)
# Changed variables to factors
mericarp_time <- mericarp_time %>% mutate_at(vars(year, trait), list(factor))

#Separated 2018 and 2019 values
# mericarp_time_final <- pivot_wider(mericarp_time, names_from = year, values_from = c(mean, sd, se))

time_plot <- ggplot(mericarp_time) +
  aes(x = year, y = mean, group = trait, colour = trait) + geom_point(size = 4) + geom_line(size = 1.5) + 
  labs(x = "Year", y = "Size (mm)", title = "Size differences of mericarps", subtitle = "Selection experiment Santa Cruz") + theme(legend.key = element_rect(fill = "gray100", 
                                                                                                                                                             size = 0, linetype = "solid")) + theme(axis.title = element_text(size = 15), 
# This plot shows that there are some changes, for example spine tip distance is reduced
# Longest spine too. This is evident of the wear and tear of the spines over time, manipulation, dispersal, predation
# Other traits did not changed at all.                                                                                                                                                                                                    axis.text = element_text(size = 15), 
                                                                                                                                                                                                    axis.text.x = element_text(size = 15), 
                                                                                                                                                                                                    axis.text.y = element_text(size = 15), 
                                                                                                                                                                                                    plot.title = element_text(size = 15))
