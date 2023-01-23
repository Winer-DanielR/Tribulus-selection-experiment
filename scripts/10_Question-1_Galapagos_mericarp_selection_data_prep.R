# Question 1: Are Galápagos mericarps generally under selection? ####

# Date: 02 January, 2023
# By: Daniel Reyes

# Description ####
# There are two approaches to this question. The first one is model testing.
# Model testing datasets are named after their traits: (Length, depth, etc)
# 
# The second one, is to build the main figure for this question. The figure is
# a 1:1 comparison of eaten and uneaten mericarps grouped by island or population.
# Year is not considered for this question.

# We used a GLMM with mericarp traits 
# (size traits: length, depth, width and depth; 
# and spine traits: longest spine, spine tip distance, lower spines) 
# as predictor of survival (eaten/uneaten mericarps). 
# The datasets are exported to the Processed Data folder.

# We used Year, Island, and Population (nested within Island) as random factors ####

# Load the point in time dataset ####
# This dataset is the point in time dataset for all years

point_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors
point_time <- point_time %>% mutate_at(vars(year,
                                      island, population, lower_spine, 
                                      spine_position, 
                                      eaten, eaten_insects,
                                      `year island`, year_pop,
                                      seed_position_1, seed_position_2,
                                      seed_position_3, seed_position_4,
                                      seed_position_5, seed_position_6,
                                      germinated, germinated_position_1,
                                      germinated_position_2, germinated_position_3,
                                      germinated_position_4, germinated_position_5,
                                      germinated_position_6), list(factor))
str(point_time)

# Data preparation ####
## Select per traits to check and remove NAs
## Length ####
length <- select(point_time, c(1:8), eaten)
length <- na.omit(length)
length <- filter(length, !is.na(length)) 

# The length dataset can be used for model testing between eaten and uneaten mericarps (See script 11)

# The datasets below are for building the figure.

### Group for mean estimates ####
#### Group by island ####
length <- group_by(length, island, eaten)

length_means_island <- length %>%  
  summarise_each(funs(length_mean = mean,
                      length_var = var,
                      length_se = sd(.)/sqrt(n()),
                      length_n = length,
  ), length)


# Pivot table
## To compare eaten and uneaten mericarps
length_means_island <- pivot_wider(length_means_island, names_from = eaten,
                                   values_from = c(3:6))

length_means_island$S_length <- (length_means_island$length_mean_0 - 
                                   length_means_island$length_mean_1)
# I can estimate the difference of uneaten and eaten mericarps (selection) in this dataset.

#### Group by island and population ####
length <- group_by(length, island, population, eaten)

length_means_pop <- length %>%  
  summarise_each(funs(length_mean = mean,
                      length_var = var,
                      length_se = sd(.)/sqrt(n()),
                      length_n = length,
  ), length)

#### Pivot table 
## To compare eaten and uneaten mericarps
length_means_pop <- pivot_wider(length_means_pop, names_from = eaten,
                                   values_from = c(4:7))

length_means_pop$S_length <- (length_means_pop$length_mean_0 - 
                                length_means_pop$length_mean_1)

# write_csv(length_means_island, "length_means_island.csv")
# write_csv(length_means_pop, "length_means_pop.csv")

# These new datasets are for the figure (See script 12)

## Width ####
width <- select(point_time, c(1:7), width, eaten)
width <- na.omit(width)
width <- filter(width, !is.na(width)) 

### Group for mean estimates ####
#### Group by island ####
width <- group_by(width, island, eaten)

width_means_island <- width %>%  
  summarise_each(funs(width_mean = mean,
                      width_var = var,
                      width_se = sd(.)/sqrt(n()),
                      width_n = length,
  ), width)


## Pivot table
## To compare eaten and uneaten mericarps
width_means_island <- pivot_wider(width_means_island, names_from = eaten,
                                   values_from = c(3:6))

width_means_island$S_width <- (width_means_island$width_mean_0 - 
                                 width_means_island$width_mean_1)
# I can estimate the difference of uneaten and eaten mericarps (selection) in this dataset.

#### Group by island and population ####
width <- group_by(width, island, population, eaten)

width_means_pop <- width %>%  
  summarise_each(funs(width_mean = mean,
                      width_var = var,
                      width_se = sd(.)/sqrt(n()),
                      width_n = length,
  ), width)

## Pivot table
## To compare eaten and uneaten mericarps
width_means_pop <- pivot_wider(width_means_pop, names_from = eaten,
                                values_from = c(4:7))

width_means_pop$S_width <- (width_means_pop$width_mean_0 - 
                              width_means_pop$width_mean_1)

# write_csv(width_means_island, "width_means_island.csv")
# write_csv(width_means_pop, "width_means_pop.csv")



## Depth ####
depth <- select(point_time, c(1:7), depth, eaten)
depth <- na.omit(depth)


### Group for mean estimates ####
#### Group by island ####
depth <- group_by(depth, island, eaten)

depth_means_island <- depth %>%  
  summarise_each(funs(depth_mean = mean,
                      depth_var = var,
                      depth_se = sd(.)/sqrt(n()),
                      depth_n = length,
  ), depth)


## Pivot table
## To compare eaten and uneaten mericarps
depth_means_island <- pivot_wider(depth_means_island, names_from = eaten,
                                  values_from = c(3:6))

depth_means_island$S_depth <- (depth_means_island$depth_mean_0 - 
                                 depth_means_island$depth_mean_1)


#### Group by population ####
depth <- group_by(depth, island, population, eaten)

depth_means_pop <- depth %>%  
  summarise_each(funs(depth_mean = mean,
                      depth_var = var,
                      depth_se = sd(.)/sqrt(n()),
                      depth_n = length,
  ), depth)


## Pivot table
## To compare eaten and uneaten mericarps
depth_means_pop <- pivot_wider(depth_means_pop, names_from = eaten,
                                  values_from = c(4:7))

depth_means_pop$S_depth <- (depth_means_pop$depth_mean_0 - 
                              depth_means_pop$depth_mean_1)


# write_csv(depth_means_island, "depth_means_island.csv")
# write_csv(depth_means_pop, "depth_means_pop.csv")


## Longest spine ####
longest_spine <- select(point_time, c(1:7), longest_spine, eaten)
longest_spine <- na.omit(longest_spine)


### Group for mean estimates ####
#### Group by island ####
longest_spine <- group_by(longest_spine, island, eaten)

longest_spine_means_island <- longest_spine %>%  
  summarise_each(funs(longest_spine_mean = mean,
                      longest_spine_var = var,
                      longest_spine_se = sd(.)/sqrt(n()),
                      longest_spine_n = length,
  ), longest_spine)


## Pivot table
## To compare eaten and uneaten mericarps
longest_spine_means_island <- pivot_wider(longest_spine_means_island, names_from = eaten,
                                  values_from = c(3:6))

longest_spine_means_island$S_longest_spine <- (longest_spine_means_island$longest_spine_mean_0 - 
                                 longest_spine_means_island$longest_spine_mean_1)


#### Group by population ####
longest_spine <- group_by(longest_spine, island, population, eaten)

longest_spine_means_pop <- longest_spine %>%  
  summarise_each(funs(longest_spine_mean = mean,
                      longest_spine_var = var,
                      longest_spine_se = sd(.)/sqrt(n()),
                      longest_spine_n = length,
  ), longest_spine)


## Pivot table
## To compare eaten and uneaten mericarps
longest_spine_means_pop <- pivot_wider(longest_spine_means_pop, names_from = eaten,
                               values_from = c(4:7))

longest_spine_means_pop$S_longest_spine <- (longest_spine_means_pop$longest_spine_mean_0 - 
                              longest_spine_means_pop$longest_spine_mean_1)


# write_csv(longest_spine_means_island, "longest_spine_means_island.csv")
# write_csv(longest_spine_means_pop, "longest_spine_means_pop.csv")

# ## Longest spine without zero ####
longest_spine_wozero <- dplyr::filter(longest_spine, !longest_spine == 0)

## Spine tip distance ####
tip_distance <- select(point_time, c(1:7), spine_tip_distance, eaten)
tip_distance <- na.omit(tip_distance)

10007-9928
# 79 mericarps without upper spines.


### Group for mean estimates ####
#### Group by island ####
tip_distance <- group_by(tip_distance, island, eaten)

spine_tip_distance_means_island <- tip_distance %>%  
  summarise_each(funs(spine_tip_distance_mean = mean,
                      spine_tip_distance_var = var,
                      spine_tip_distance_se = sd(.)/sqrt(n()),
                      spine_tip_distance_n = length,
  ), spine_tip_distance)


## Pivot table
## To compare eaten and uneaten mericarps
spine_tip_distance_means_island <- pivot_wider(spine_tip_distance_means_island, names_from = eaten,
                                  values_from = c(3:6))

spine_tip_distance_means_island$S_spine_tip_distance <- (spine_tip_distance_means_island$spine_tip_distance_mean_0 - 
                                 spine_tip_distance_means_island$spine_tip_distance_mean_1)


#### Group by population ####
tip_distance <- group_by(tip_distance, island, population, eaten)

spine_tip_distance_means_pop <- tip_distance %>%  
  summarise_each(funs(spine_tip_distance_mean = mean,
                      spine_tip_distance_var = var,
                      spine_tip_distance_se = sd(.)/sqrt(n()),
                      spine_tip_distance_n = length,
  ), spine_tip_distance)


## Pivot table
## To compare eaten and uneaten mericarps
spine_tip_distance_means_pop <- pivot_wider(spine_tip_distance_means_pop, names_from = eaten,
                               values_from = c(4:7))

spine_tip_distance_means_pop$S_spine_tip_distance <- (spine_tip_distance_means_pop$spine_tip_distance_mean_0 - 
                              spine_tip_distance_means_pop$spine_tip_distance_mean_1)


# write_csv(spine_tip_distance_means_island, "spine_tip_distance_means_island.csv")
# write_csv(spine_tip_distance_means_pop, "spine_tip_distance_means_pop.csv")



### Spine tip distance without zero #####
# We removed mericarps without upper spines from analysis.These mericarps had a tip distance of 0.
tip_distance_wozero <- dplyr::filter(tip_distance, !spine_tip_distance == 0)

4003-3982
# 21 mericarps without upper spines.

## Lower spines ####
## Lower spines is a binomial trait
lower_spines <- select(point_time, c(1:7), lower_spine, eaten)
lower_spines <- na.omit(lower_spines)

#Extract the frequencies of total lower spines by island
lower_spines <- group_by(lower_spines, island)
lower_spines_count_island <- dplyr::count(lower_spines, lower_spine)
lower_spines_count_island <- lower_spines_count_island %>%  
  group_by(island) %>% mutate(freq_all = n/sum(n))

# Extract the frequencies of total lower spines by population
lower_spines <- group_by(lower_spines, island, population)
lower_spines_count_population <- dplyr::count(lower_spines, lower_spine)
lower_spines_count_population <- lower_spines_count_population %>%  
  group_by(population) %>% mutate(freq_all = n/sum(n))


### Group for count estimates ####
### One way to estimate this for lower spines to count the frequency of eaten
### and uneaten mericarps. So, instead of using a mean I am using the number of 
### mericarps that have lower spines and the number witouth lower spines.

#### Group by island ####
lower_spines <- group_by(lower_spines, island, eaten)


# Count the frequency of lower spines
lower_spines_island <- dplyr::count(lower_spines, lower_spine)
# This counts the number of mericarps that were eaten (or not) that have lower spines (or not)

# Frequencies of lower spines
lower_spines_island <- lower_spines_island %>%  
  group_by(island) %>% mutate(freq = n/sum(n))
# I think this way you estimate the frequency of mericarps with either lower spines
# or not lower spines that were eaten or not per island.
# The total sum of mericarps per island and the frequency of mericarps with (or not)]
# lower spines that were eaten (or not).

## Pivot table
## To compare eaten and uneaten mericarps
lower_spines_island <- pivot_wider(lower_spines_island, names_from = eaten,
                                               values_from = c(4:5))

# Replace NAs for 0 frequencies
lower_spines_island[is.na(lower_spines_island)] = 0

## Join lower spines by island with total lower spine frequency
lower_spines_island <- right_join(lower_spines_island, 
                                  lower_spines_count_island,
                                  lower_spines_island,
                                  by = c("island", "lower_spine"))

#### Group by populations ####
lower_spines <- group_by(lower_spines, island, population, eaten)

lower_spines_pop <- dplyr::count(lower_spines, lower_spine)

# Frequencies estimations. Per population
lower_spines_pop <- lower_spines_pop %>%  
  group_by(island, population) %>% mutate(freq = n/sum(n))

## Pivot table
## To compare eaten and uneaten mericarps
lower_spines_pop <- pivot_wider(lower_spines_pop, names_from = eaten,
                                  values_from = c(5:6))

# Replace NAs for 0 frequencies
lower_spines_pop[is.na(lower_spines_pop)] = 0

## Join lower spines by population with total lower spine frequency

lower_spines_pop <- right_join(lower_spines_pop, 
                                  lower_spines_count_population,
                                  lower_spines_pop,
                                  by = c("island", "population","lower_spine"))


# Calculate S estimates
lower_spines_island$S_lower_spine <- (lower_spines_island$freq_0 - lower_spines_island$freq_1)
lower_spines_pop$S_lower_spine <- (lower_spines_pop$freq_0 - lower_spines_pop$freq_1)


## Lower spine ####
### S*estimates ####
lower_spines_island$S_lower_spine <- (lower_spines_island$freq_0 - lower_spine_island$freq_1)
lower_spines_pop$S_lower_spine <- (lower_spines_pop$freq_0 - lower_spines_pop$freq_1)


#write_csv(lower_spines_island, "lower_spines_island.csv")
#write_csv(lower_spines_pop, "lower_spines_pop.csv")


## Spine position (as factor) ####
spine_position <- select(point_time, c(1:7), spine_position, eaten)
spine_position <- na.omit(spine_position)


#Extract the frequencies of total spine position by island
spine_position <- group_by(spine_position, island)
spine_position_count_island <- dplyr::count(spine_position, spine_position)
spine_position_count_island <- spine_position_count_island %>%  
  group_by(island) %>% mutate(freq_all = n/sum(n))

# Extract the frequencies of total spine position by population
spine_position <- group_by(spine_position, island, population)
spine_position_count_population <- dplyr::count(spine_position, spine_position)
spine_position_count_population <- spine_position_count_population %>%  
  group_by(population) %>% mutate(freq_all = n/sum(n))



### Group for mean estimates ####
#### Group by island ####
spine_position <- group_by(spine_position, island, eaten)

# If is a factor use the count function to count the frequency of mericarps
# with a determined angle.
# 
spine_position_island <- dplyr::count(spine_position, spine_position)


spine_position_island <- spine_position_island %>%  
  group_by(island) %>% mutate(freq = n/sum(n))
# Sum of all mericarps frequencies per island and the frequency of positions


# If is a continuous variable use this summary function
# spine_position_means_island <- spine_position %>%  
#   summarise_each(funs(spine_position_mean = mean,
#                       spine_position_var = var,
#                       spine_position_se = sd(.)/sqrt(n()),
#                       spine_position_n = length,
#   ), spine_position)


## Pivot table
## To compare eaten and uneaten mericarps
## 

spine_position_island <- pivot_wider(spine_position_island, names_from = eaten,
                                               values_from = c(4,5))

# Replace NAs for 0s
spine_position_island[is.na(spine_position_island)] = 0


## Join lower spines by island with total lower spine frequency
spine_position_island <- right_join(spine_position_island, 
                                  spine_position_count_island,
                                  spine_position_island,
                                  by = c("island", "spine_position"))


# spine_position_means_island <- pivot_wider(spine_position_means_island, names_from = eaten,
#                                                values_from = c(3:6))
# 
# spine_position_means_island$S_spine_position <- (spine_position_means_island$spine_position_mean_0 - 
#                                                            spine_position_means_island$spine_position_mean_1)


#### Group by population ####
spine_position <- group_by(spine_position, island, population, eaten)

spine_position_pop <- dplyr::count(spine_position, spine_position)

# Frequencies estimations. Per population
spine_position_pop <- spine_position_pop %>%  
  group_by(island, population) %>% mutate(freq = n/sum(n))

## Pivot table
## To compare eaten and uneaten mericarps
spine_position_pop <- pivot_wider(spine_position_pop, names_from = eaten,
                                            values_from = c(5,6))

# Replace NAs as 0 frequencies
spine_position_pop[is.na(spine_position_pop)] = 0

## Join lower spines by population with total lower spine frequency
spine_position_pop <- right_join(spine_position_pop, 
                               spine_position_count_population,
                               spine_position_pop,
                               by = c("island", "population","spine_position"))


# Calculate S estimates
spine_position_island$S_spine_position <- (spine_position_island$freq_0 - spine_position_island$freq_1)
spine_position_pop$S_spine_position <- (spine_position_pop$freq_0 - spine_position_pop$freq_1)


## Spine position wihtout zero ####
spine_position_wozero <- dplyr::filter(spine_position, !spine_position == 0)

