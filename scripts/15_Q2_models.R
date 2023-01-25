# Question 2: Models ####
# 
# Date: 24 January, 2023
# By: Daniel Reyes
# 
# Goal: ####
# This script contains the models for question 2.
# 
# Description: #### 
# We used a GLMM with the mean trait value per island as a predictor 
# of mericarp selection as the difference in trait value between 
# eaten and uneaten mericarps per island.

# Data loading ####
## Populations ####
depth_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_population.csv")
length_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_population.csv")
longest_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_population.csv")
tip_distance_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_population.csv")
width_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_population.csv")
lower_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_pop.csv")
spine_position_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_pop.csv")


# Data preparation ####
# Making char into factors (island, populations)
# 
### Populations #####
depth_pop <- depth_pop %>% mutate_at(vars(island, population), list(factor))
length_pop <- length_pop %>% mutate_at(vars(island, population), list(factor))
longest_spine_pop  <- longest_spine_pop %>% mutate_at(vars(island, population), list(factor))
lower_spine_pop <- lower_spine_pop %>% mutate_at(vars(island, population, lower_spine), list(factor))
spine_position_pop <- spine_position_pop %>% mutate_at(vars(island, population, spine_position), list(factor))
tip_distance_pop <- tip_distance_pop %>% mutate_at(vars(island, population), list(factor))
width_pop <- width_pop %>% mutate_at(vars(island, population), list(factor))


# Models ####
# The structure of this model is to test the effect of mean trait values
# and selection estimates.
## Depth ####
