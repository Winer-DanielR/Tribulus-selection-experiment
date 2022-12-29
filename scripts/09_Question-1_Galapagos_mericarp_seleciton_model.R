# Question 1: Are Galápagos mericarps generally under selection? ####

# We used a GLMM with mericarp traits 
# (size traits: length, depth, width and depth; 
# and spine traits: longest spine, spine tip distance, lower spines) 
# as predictor of survival (eaten/uneaten mericarps). 
# We used Year, Island, and Population (nested within Island) as random factors ####

# By: Daniel Reyes Corral

# Load the point in time dataset ####
# This dataset is the point in time dataset for all years

point_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors
point_time <- point_time %>% mutate_at(vars(year,
                                      island, population, lower_spine, spine_position, eaten, eaten_insects,
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
# I want to separate each trait for the models and remove NAs ####


## Length ####
length <- select(point_time, c(1:8), eaten)
length <- na.omit(length)
length <- filter(length, !is.na(length))

# This is a summarize code that estimates means, variance, sd, etc.
# length <- length %>%  
#   summarise_each(funs(length_mean = mean,
#                       length_var = var,
#                       length_se = sd(.)/sqrt(n()),
#                       length_n = length,
#   ), length)
#   

## Width ####
width <- select(point_time, c(1:7), width, eaten)
width <- na.omit(width)



## Depth ####
depth <- select(point_time, c(1:7), depth, eaten)
depth <- na.omit(depth)

## Longest spine ####
longest_spine <- select(point_time, c(1:7), longest_spine, eaten)
longest_spine <- na.omit(longest_spine)

## Longest spine without zero ####
longest_spine_wozero <- dplyr::filter(longest_spine, !longest_spine == 0)

## Spine tip distance ####
tip_distance <- select(point_time, c(1:7), spine_tip_distance, eaten)
tip_distance <- na.omit(tip_distance)

10007-9928
# 79 mericarps without upper spines.

### Spine tip distance without zero #####
# We removed mericarps without upper spines from analysis.These mericarps had a tip distance of 0.
tip_distance_wozero <- dplyr::filter(tip_distance, !spine_tip_distance == 0)

4003-3982
# 21 mericarps without upper spines.


## Lower spines ####
lower_spines <- select(point_time, c(1:7), lower_spine, eaten)
lower_spines <- na.omit(lower_spines)

## Spine position (as factor) ####
spine_position <- select(point_time, c(1:7), spine_position, eaten)
spine_position <- na.omit(spine_position)

## Spine position wihtout zero ####
spine_position_wozero <- dplyr::filter(spine_position, !spine_position == 0)

6198-6183
# 15 mericarps without spine angle