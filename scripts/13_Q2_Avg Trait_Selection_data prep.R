# Q2 Data loading ####


# These datasets show the trait mean of all mericarps and the mericarps that survived.
# These means were estimated in script 07, from the point in time dataset.
# The difference between these means is selection.
# This plot compares the mean difference and creates a vector showing the direction and magnitude of selection

# Load datasets where mean trait is found ####
depth <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/depth survival mean.csv")
length <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/length survival mean.csv")
longest_spine <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/longest spine survival mean.csv")
spine_position <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/spine position survival mean.csv")
tip_distance <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/tip distance survival mean.csv")
width <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/width survival mean.csv")

# Load datasets of Q1 where Selection estimates are found (uneaten - eaten) ####
depth_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/depth_means_island.csv")
depth_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/depth_means_pop.csv")
length_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/length_means_island.csv")
length_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/length_means_pop.csv")
longest_spine_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/longest_spine_means_island.csv")
longest_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/longest_spine_means_pop.csv")
lower_spine_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/lower_spines_island.csv")
lower_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/lower_spines_pop.csv")
spine_position_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/spine_position_island.csv")
spine_position_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/spine_position_pop.csv")
tip_distance_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/spine_tip_distance_means_island.csv")
tip_distance_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/spine_tip_distance_means_pop.csv")
width_island <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/width_means_island.csv")
width_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Q1 trait datasets/width_means_pop.csv")

# Data prepping ####
# This is converting populations and islands into factors.
## Mean traits ####
depth <- depth %>% mutate_at(vars(year, island, population, survival), list(factor))
length <- length %>% mutate_at(vars(year, island, population, survival), list(factor))
longest_spine <- longest_spine %>% mutate_at(vars(year, island, population, survival), list(factor))
spine_position <- spine_position %>% mutate_at(vars(year, island, population, survival), list(factor))
tip_distance <- tip_distance %>% mutate_at(vars(year, island, population, survival), list(factor))
width <- width %>% mutate_at(vars(year, island, population, survival), list(factor))

## Q1 datasets ####
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


# Extract trait means and add them to the datasets ####
# 
# First I need to extract the trait means from the  first datasets.
# The process of extracting the mean traits from this dataset includes:
# creating a dataset that separates survival (all and eaten) into columns
# Then, group de dataset into island for island analysis and populations
# Sumarize the "all" columns which is the mean triats
# Add these mean traits into the other dataset that includes de S estimates diferences
# of uneaten-eaten.

 
## depth ####
str(depth)
depth <- ungroup(depth) # I am not sure if this does anything but just in case ungroup the dataset

depth_pivot <- pivot_wider(depth, names_from = survival,
                                 values_from = c(5))
# This turns the table and separates the mean of all mericarps and the mean of survived mericarps
# we need the mean of all mericarps from this set.

depth_island_mean <- depth_pivot %>% group_by(island) %>%
  summarize(mean_all = mean(all))
# This dataset was previously grouped by year as well, we don't do that for this question
# so we group the dataset again and take the means per island

### depth island dataset merging ####
### Now that we have the mean of all mericarps per island, we merge this with the
### island dataset.

depth_island <- right_join(depth_island, depth_island_mean,
                           depth_island, by = "island")

depth_pop_mean <- depth_pivot %>% group_by(island, population) %>%
  summarize(mean_all = mean(all))

depth_pop <- right_join(depth_pop, depth_pop_mean,
                        depth_pop, by = c("island", "population"))

### Export depth datasets ####
### I think is useful to export these datasets for future analysis.
 # write_csv(depth_island, "depth_island_Q2.csv")
 # write_csv(depth_pop, "depth_population_Q2.csv")
# 


## length ####
str(length)
length <- ungroup(length) # I am not sure if this does anything but just in case ungroup the dataset

length_pivot <- pivot_wider(length, names_from = survival,
                           values_from = c(5))
# This turns the table and separates the mean of all mericarps and the mean of survived mericarps
# we need the mean of all mericarps from this set.

length_island_mean <- length_pivot %>% group_by(island) %>%
  summarize(mean_all = mean(all))
# This dataset was previously grouped by year as well, we don't do that for this question
# so we group the dataset again and take the means per island

### length island dataset merging ####
### Now that we have the mean of all mericarps per island, we merge this with the
### island dataset.

length_island <- right_join(length_island, length_island_mean,
                           length_island, by = "island")

length_pop_mean <- length_pivot %>% group_by(island, population) %>%
  summarize(mean_all = mean(all))

length_pop <- right_join(length_pop, length_pop_mean,
                        length_pop, by = c("island", "population"))

### Export length datasets ####
### I think is useful to export these datasets for future analysis.
# write_csv(length_island, "length_island_Q2.csv")
# write_csv(length_pop, "length_population_Q2.csv")
# 
# 
# 
## longest_spine ####
str(longest_spine)
longest_spine <- ungroup(longest_spine) # I am not sure if this does anything but just in case ungroup the dataset

longest_spine_pivot <- pivot_wider(longest_spine, names_from = survival,
                            values_from = c(5))
# This turns the table and separates the mean of all mericarps and the mean of survived mericarps
# we need the mean of all mericarps from this set.

longest_spine_island_mean <- longest_spine_pivot %>% group_by(island) %>%
  summarize(mean_all = mean(all))
# This dataset was previously grouped by year as well, we don't do that for this question
# so we group the dataset again and take the means per island

### longest_spine island dataset merging ####
### Now that we have the mean of all mericarps per island, we merge this with the
### island dataset.

longest_spine_island <- right_join(longest_spine_island, longest_spine_island_mean,
                            longest_spine_island, by = "island")

longest_spine_pop_mean <- longest_spine_pivot %>% group_by(island, population) %>%
  summarize(mean_all = mean(all))

longest_spine_pop <- right_join(longest_spine_pop, longest_spine_pop_mean,
                         longest_spine_pop, by = c("island", "population"))

### Export longest_spine datasets ####
### I think is useful to export these datasets for future analysis.
 # write_csv(longest_spine_island, "longest_spine_island_Q2.csv")
 # write_csv(longest_spine_pop, "longest_spine_population_Q2.csv")
 # 
 # 
 # 


## tip_distance ####
str(tip_distance)
tip_distance <- ungroup(tip_distance) # I am not sure if this does anything but just in case ungroup the dataset

tip_distance_pivot <- pivot_wider(tip_distance, names_from = survival,
                                   values_from = c(5))
# This turns the table and separates the mean of all mericarps and the mean of survived mericarps
# we need the mean of all mericarps from this set.

tip_distance_island_mean <- tip_distance_pivot %>% group_by(island) %>%
  summarize(mean_all = mean(all))
# This dataset was previously grouped by year as well, we don't do that for this question
# so we group the dataset again and take the means per island

### tip_distance island dataset merging ####
### Now that we have the mean of all mericarps per island, we merge this with the
### island dataset.

tip_distance_island <- right_join(tip_distance_island, tip_distance_island_mean,
                                   tip_distance_island, by = "island")

tip_distance_pop_mean <- tip_distance_pivot %>% group_by(island, population) %>%
  summarize(mean_all = mean(all))

tip_distance_pop <- right_join(tip_distance_pop, tip_distance_pop_mean,
                                tip_distance_pop, by = c("island", "population"))

### Export tip_distance datasets ####
### I think is useful to export these datasets for future analysis.
# write_csv(tip_distance_island, "tip_distance_island_Q2.csv")
# write_csv(tip_distance_pop, "tip_distance_population_Q2.csv")
# 
# 
# 
# 
# 
## width ####
str(width)
width <- ungroup(width) # I am not sure if this does anything but just in case ungroup the dataset

width_pivot <- pivot_wider(width, names_from = survival,
                                  values_from = c(5))
# This turns the table and separates the mean of all mericarps and the mean of survived mericarps
# we need the mean of all mericarps from this set.

width_island_mean <- width_pivot %>% group_by(island) %>%
  summarize(mean_all = mean(all))
# This dataset was previously grouped by year as well, we don't do that for this question
# so we group the dataset again and take the means per island

### width island dataset merging ####
### Now that we have the mean of all mericarps per island, we merge this with the
### island dataset.

width_island <- right_join(width_island, width_island_mean,
                                  width_island, by = "island")

width_pop_mean <- width_pivot %>% group_by(island, population) %>%
  summarize(mean_all = mean(all))

width_pop <- right_join(width_pop, width_pop_mean,
                               width_pop, by = c("island", "population"))

### Export width datasets ####
### I think is useful to export these datasets for future analysis.
# write_csv(width_island, "width_island_Q2.csv")
# write_csv(width_pop, "width_population_Q2.csv")

# The remaining analysis is lower spine and spine position.
# I think we can take the difference of frequencies vs the total proportion of 
# mericarps with lower spines, not counting eaten vs uneaten.
# Check this with Andrew.