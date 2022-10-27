################Tribulus Selection experiments Dataset##############

####### Goal: Prepare the data to compare start of the experiment and the end. ####
# This was done using mericarps recovered from Santa Cruz island. I missed the information on the other two.





time_effect <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Time effect measurements.csv")
time_effect <- as_tibble(time_effect)

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

##### Plots per trait #####

#Length
length_plot <- ggplot(time_effect) +
 aes(x = year, y = length, fill = size) +
 geom_boxplot() +
 scale_fill_hue() +
 labs(x = "Year", y = "Size (mm)", title = "Time effect. Lenght") +
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


