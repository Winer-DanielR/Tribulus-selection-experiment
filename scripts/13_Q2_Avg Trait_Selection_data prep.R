# Q2 Data loading ####


# These datasets show the trait mean of all mericarps and the mericarps that survived.
# These means were estimated in script 07, from the point in time dataset.
# The difference between these means is selection.
# This plot compares the mean difference and creates a vector showing the direction and magnitude of selection

depth <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/depth survival mean.csv")
length <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/length survival mean.csv")
longest_spine <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/longest spine survival mean.csv")
spine_position <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/spine position survival mean.csv")
tip_distance <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/tip distance survival mean.csv")
width <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time trait means/width survival mean.csv")

# Data prepping ####
# Year, island, population and survival are going to be grouping factors
depth <- depth %>% mutate_at(vars(year, island, population, survival), list(factor))
length <- length %>% mutate_at(vars(year, island, population, survival), list(factor))
longest_spine <- longest_spine %>% mutate_at(vars(year, island, population, survival), list(factor))
spine_position <- spine_position %>% mutate_at(vars(year, island, population, survival), list(factor))
tip_distance <- tip_distance %>% mutate_at(vars(year, island, population, survival), list(factor))
width <- width %>% mutate_at(vars(year, island, population, survival), list(factor))

# Data groups by island and populations ####
# First I need to extract the trait means from the dataset.
# The process of extracting the mean traits from this dataset includes:
# creating a dataset that separates survival (all and eaten) into columns
# Then, group de dataset into island for island analysis and populations
# Sumarize the "all" columns which is the mean triats
# Add these mean traits into the other dataset that includes de S estimates diferences
# of uneaten-eaten.
# Now, I need to make sure that there's no NAs on my datasets.
# 
## Depth ####
str(depth)
depth <- ungroup(depth)

depth_island_pop <- pivot_wider(depth, names_from = survival,
                                 values_from = c(5))

depth_island_mean <- depth_island_pop %>% group_by(island) %>%
  summarize(mean_all = mean(all))

depth_pop_mean <- depth_island_pop %>% group_by(island, population) %>%
  summarize(mean_all = mean(all))
