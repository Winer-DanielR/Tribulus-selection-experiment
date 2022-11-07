#### Plot comparing selection estimates per trait: Data preparation ####
# By: Daniel Reyes Corral
# Date: 06/10/2021
# In this script I want to group mericarps per year, island and population
# Then calculate the mean of all mericarps and calculate the mean of surviving mericarps
# PLot these estimates between two traits.
# The dataset that I am using here is the point in time dataset
# Caveat: Lower spines is not included yet. 


# Load the point in time dataset
point_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors
point_time <- point_time %>% mutate_at(vars(year,
                                            island, population, lower_spine, eaten, eaten_insects,
                                            `year island`, year_pop,
                                            seed_position_1, seed_position_2,
                                            seed_position_3, seed_position_4,
                                            seed_position_5, seed_position_6,
                                            germinated, germinated_position_1,
                                            germinated_position_2, germinated_position_3,
                                            germinated_position_4, germinated_position_5,
                                            germinated_position_6), list(factor))
str(point_time)

# Group by year, island and populations
pt_mean_survived <- group_by(point_time, year, island, population, eaten) # This groups the dataset up to eaten. Use for estimates of surviving mericarps
pt_mean_all <- group_by(point_time, year, island, population) # This groups them by population. Use for estimates of all mericarps


# Select per traits to check and remove NAs
# I am going to select the mean summary first, then add the other ones if needed

# Survived mericarps (also eaten) ####
## Length ####
length_eaten <- select(pt_mean_survived, c(1:8), eaten)
length_eaten <- na.omit(length_eaten)
length_eaten <- length_eaten %>%  
  summarise_each(funs(survived = mean,
                      #var_surv = var,
                      #se_surv = sd(.)/sqrt(n()),
                      #n_surv = length,
  ), length)

## Width ####
width_eaten <- select(pt_mean_survived, c(1:7,9), eaten)
width_eaten <- na.omit(width_eaten)
width_eaten <- width_eaten %>%  
  summarise_each(funs(survived = mean,
                      #var_surv = var,
                      #se_surv = sd(.)/sqrt(n()),
                      #n_surv = length,
  ), width)

## Depth ####
depth_eaten <- select(pt_mean_survived, c(1:7,10), eaten)
depth_eaten <- na.omit(depth_eaten)
depth_eaten <- depth_eaten %>%  
  summarise_each(funs(survived = mean,
                      #var_surv = var,
                      #se_surv = sd(.)/sqrt(n()),
                      #n_surv = length,
  ), depth)

## Longest spine ####
longest_spine_eaten <- select(pt_mean_survived, c(1:7,11), eaten)
longest_spine_eaten <- na.omit(longest_spine_eaten)
longest_spine_eaten <- longest_spine_eaten %>%  
  summarise_each(funs(survived = mean,
                      #var_surv = var,
                      #se_surv = sd(.)/sqrt(n()),
                      #n_surv = length,
  ), longest_spine)

## Tip distance ####
tip_distance_eaten <- select(pt_mean_survived, c(1:7,12), eaten)
tip_distance_eaten <- na.omit(tip_distance_eaten)
tip_distance_eaten <- tip_distance_eaten %>%  
  summarise_each(funs(survived = mean,
                      #var_surv = var,
                      #se_surv = sd(.)/sqrt(n()),
                      #n_surv = length,
  ), spine_tip_distance)

## Spine_position ####
spine_position_eaten <- select(pt_mean_survived, c(1:7,14), eaten)
spine_position_eaten <- na.omit(spine_position_eaten)
spine_position_eaten <- spine_position_eaten %>%  
  summarise_each(funs(survived = mean,
                      #var_surv = var,
                      #se_surv = sd(.)/sqrt(n()),
                      #n_surv = length,
  ), spine_position)

# The eaten datasets have the survived and eaten mericarp means.
# The eaten datasets differ in the number of observations this means that
# across years there are many NAs.
# Tip distance for example have only 87

# Extract only surviving mericarps
survived_length <- filter(length_eaten, eaten == "0")
survived_width <- filter(width_eaten, eaten == "0")
survived_depth <- filter(depth_eaten, eaten == "0")
survived_longest_spine <- filter(longest_spine_eaten, eaten == "0")
survived_tip_distance <- filter(tip_distance_eaten, eaten == "0")
survived_spine_position <- filter(spine_position_eaten, eaten == "0")


# All mericarps mean ####
## Length ####
all_length <- select(pt_mean_all, c(1:8))
all_length <- na.omit(all_length)
all_length <- all_length %>%
  summarise_each(funs(all = mean,
                      #length_var_all = var,
                      #length_se_all = sd(.)/sqrt(n()),
                      #length_n_all = length,
  ), length)

## Width ####
all_width <- select(pt_mean_all, c(1:7, 9))
all_width <- na.omit(all_width)
all_width <- all_width %>%
  summarise_each(funs(all = mean,
                      #depth_var_all = var,
                      #depth_se_all = sd(.)/sqrt(n()),
                      #depth_n_all = length,
  ), width)

## Depth ####
all_depth <- select(pt_mean_all, c(1:7, 10))
all_depth <- na.omit(all_depth)
all_depth <- all_depth %>%
  summarise_each(funs(all = mean,
                      #depth_var_all = var,
                      #depth_se_all = sd(.)/sqrt(n()),
                      #depth_n_all = length,
  ), depth)

## Longest spine ####
all_longest_spine <- select(pt_mean_all, c(1:7, 11))
all_longest_spine <- na.omit(all_longest_spine)
all_longest_spine <- all_longest_spine %>%
  summarise_each(funs(all = mean,
                      #depth_var_all = var,
                      #depth_se_all = sd(.)/sqrt(n()),
                      #depth_n_all = length,
  ), longest_spine)

## Tip distance ####
all_tip_distance <- select(pt_mean_all, c(1:7, 12))
all_tip_distance <- na.omit(all_tip_distance)
all_tip_distance <- all_tip_distance %>%
  summarise_each(funs(all = mean,
                      #depth_var_all = var,
                      #depth_se_all = sd(.)/sqrt(n()),
                      #depth_n_all = length,
  ), spine_tip_distance)

## Spine_position ####
all_spine_position <- select(pt_mean_all, c(1:7, 14))
all_spine_position <- na.omit(all_spine_position)
all_spine_position <- all_spine_position %>%
  summarise_each(funs(all = mean,
                      #depth_var_all = var,
                      #depth_se_all = sd(.)/sqrt(n()),
                      #depth_n_all = length,
  ), spine_position)


### Join datasets ####
## Length ####
length <- left_join(all_length, survived_length)
length <- select(length, !c(5))
## Width ####
width <- left_join(all_width, survived_width)
width <- select(width, !c(5))
## Depth ####
depth <- left_join(all_depth, survived_depth)
depth <- select(depth, !c(5))
## Longest spine ####
longest_spine <- left_join(all_longest_spine, survived_longest_spine)
longest_spine <- select(longest_spine, !c(5))
## Tip distance ####
tip_distance <- left_join(all_tip_distance, survived_tip_distance)
tip_distance <- select(tip_distance, !c(5))
## Spine position ####
spine_position <- left_join(all_spine_position, survived_spine_position)
spine_position <- select(spine_position, !c(5))

# Trait pivots ####
# The trait pivots prepares the datasets for the plots
# Creates a column named survival which are the mericarps that survived
# and all mericarps means. The value column is the mean of the trait
# which is specified.
# These datasets are the ones used for the plot comparisons

## Length ####
length_pivot <- pivot_longer(length, cols = c(4,5),
                             names_to = "survival",
                             values_to = "length_mean")

## Width ####
width_pivot <- pivot_longer(width, cols = c(4,5),
                             names_to = "survival",
                             values_to = "width_mean")

## Depth ####
depth_pivot <- pivot_longer(depth, cols = c(4,5),
                             names_to = "survival",
                             values_to = "depth_mean")
## Longest spine ####
longest_spine_pivot <- pivot_longer(longest_spine, cols = c(4,5),
                                    names_to = "survival",
                                    values_to = "longest_spine_mean")

## Tip distance ####
tip_distance_pivot <- pivot_longer(tip_distance, cols = c(4,5),
                                    names_to = "survival",
                                    values_to = "tip_distance_mean")

## Spine position ####
spine_position_pivot <- pivot_longer(spine_position, cols = c(4,5),
                                     names_to = "survival",
                                     values_to = "spine_position_mean")

# Trait comparisons datasets ####
# These datasets are going to be used to create the plot
# and the trait comparisons. Find them in the processed data folder
# under the trait comparison plot folder.

## Length ####
write_csv(length_pivot, "length survival mean.csv")
## Width ####
write_csv(width_pivot, "width survival mean.csv")
## Depth ####
write_csv(depth_pivot, "depth survival mean.csv")
## Longest spine ####
write_csv(longest_spine_pivot, "longest spine survival mean.csv")
## Tip distance ####
write_csv(tip_distance_pivot, "tip distance survival mean.csv")
## Spine position ####
write_csv(spine_position_pivot, "spine position survival mean.csv")

  

