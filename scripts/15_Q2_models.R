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

## Populations per year ####
## This could give us more observations per population
depth <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/depth_mean_year.csv")
length <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/length_mean_year.csv")
longest_spine <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/longest_spine_mean_year.csv")
tip_distance <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/tip_distance_mean_year.csv")
width <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/width_mean_year.csv")
lower_spine <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/lower_spines_year.csv")
spine_position <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/spine_position_year.csv")

## PCA Populations ####
pca_means_Q2 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_population_NAs.csv")

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

### Populations per year ####
depth <- depth %>% mutate_at(vars(island, population), list(factor))
length <- length %>% mutate_at(vars(island, population), list(factor))
longest_spine  <- longest_spine %>% mutate_at(vars(island, population), list(factor))
lower_spine <- lower_spine %>% mutate_at(vars(island, population, lower_spine), list(factor))
spine_position <- spine_position %>% mutate_at(vars(island, population), list(factor))
tip_distance <- tip_distance %>% mutate_at(vars(island, population), list(factor))
width <- width %>% mutate_at(vars(island, population), list(factor))

### PCA populations ####
pca_means_Q2 <- pca_means_Q2 %>% mutate_at(vars(island, population), list(factor))
str(pca_means_Q2)
pca_means_Q2 <- na.omit(pca_means_Q2)

# Models ####
# The structure of this model is to test the effect of mean trait values
# and selection estimates.
# mean trait value ~ mericarp selection
# 

# PCA ####
## Size ####
## This is tricky because we are using the mean traits per population
## to estimate the selection per population.
hist(pca_means_Q2$Size_mean, breaks = 30)

sizeQ2 <- glmmTMB(S_Size ~ Size_mean +
                    (1|island/population),
                  data = pca_means_Q2,
                  REML = F)



### Model Diagnostics ####
# Residual histograms
diagnostic(resid(sizeQ2))

# DHARMa
testResiduals(sizeQ2)

### Results ####
summary(sizeQ2)
Anova(sizeQ2, type = "III")

pR2(sizeQ2)["McFadden"]
# Estimated R2 0.2146158 

r.squaredGLMM(sizeQ2)

## Defense ####
## This is tricky because we are using the mean traits per population
## to estimate the selection per population.
hist(pca_means_Q2$Defense_mean, breaks = 30)

defenseQ2 <- glmmTMB(S_Defense ~ Defense_mean +
                    (1|island/population),
                  data = pca_means_Q2,
                  REML = F)



### Model Diagnostics ####
# Residual histograms
diagnostic(resid(defenseQ2))

# DHARMa
testResiduals(defenseQ2)

### Results ####
summary(defenseQ2)
Anova(defenseQ2, type = "III")

pR2(defenseQ2)["McFadden"]
# Estimated R2 0.3582375  

r.squaredGLMM(defenseQ2)

## Position ####
## This is tricky because we are using the mean traits per population
## to estimate the selection per population.
hist(pca_means_Q2$Position_mean, breaks = 30)

PositionQ2 <- glmmTMB(S_Position ~ Position_mean +
                       (1|island/population),
                     data = pca_means_Q2,
                     REML = F)



### Model Diagnostics ####
# Residual histograms
diagnostic(resid(PositionQ2))

# DHARMa
testResiduals(PositionQ2)

### Results ####
summary(PositionQ2)
Anova(PositionQ2, type = "III")

pR2(PositionQ2)["McFadden"]
# Estimated R2 0.1810015 

r.squaredGLMM(PositionQ2)


## Depth ####
# Remove NAs
hist(depth$mean_all, breaks = 30)
depth_pop <- na.omit(depth_pop)

depth_m2 <- glmmTMB(S_depth ~ mean_all + 
                      (1|island/population),
                    data = depth_pop,
                    REML = F)
# I am not sure if this makes sense using this database because each 
# observation is for a population, there are not various population obs
depth_m3 <- glmmTMB(S_depth ~ mean_all + 
                      (1|island/population),
                    data = depth,
                    REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(depth_m2))
diagnostic(resid(depth_m3))
# DHARMa
testResiduals(depth_m2)
testResiduals(depth_m3)
### Results ####
summary(depth_m2)
summary(depth_m3)
Anova(depth_m2, type = "III")
Anova(depth_m3, type = "III")

# I think is better to use the year dataset because there is at least 3-5 observations per population in this case
# this justifies using the island nested in population model
# I will use that dataset for the following models.
# 
## Length ####
hist(length$mean_all, breaks = 30)

length_m2 <- glmmTMB(S_length ~ mean_all  + 
                      (1|island/population),
                    data = length,
                    REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(length_m2))

# DHARMa
testResiduals(length_m2)
hist(resid(length_m2), breaks = 50)
# I think I need to leave it because we have so few observations.

### Results ####
summary(length_m2)
Anova(length_m2, type = "III")

## Longest spine ####

longest_spine_m2 <- glmmTMB(mean_all ~ S_longest_spine + 
                       (1|island/population),
                     data = longest_spine,
                     REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(longest_spine_m2))

### Data adjustment Longest spine ####
# Remove earlier years, because there is an difference on how the trait
# was measured
longest_spine_filter <- filter(longest_spine, !year == "2015")
longest_spine_filter <- filter(longest_spine_filter, !year == "2016")

### Filter model ####
longest_spine_m3 <- glmmTMB(S_longest_spine  ~ mean_all  + 
                              (1|island/population),
                            data = longest_spine_filter,
                            REML = F)

### Model diagnostics ####
diagnostic(resid(longest_spine_m3))
# DHARMa
testResiduals(longest_spine_m3)

### Results ####
summary(longest_spine_m3)
Anova(longest_spine_m3, type = "III")

## Lower spines ####
lower_spine_m2 <- glmmTMB( S_lower_spine ~ freq_all + 
                       (1|island/population),
                     data = lower_spine,
                     REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(lower_spine_m2))
# DHARMa
testResiduals(lower_spine_m2)
# I think this model needs another tpye of distribution but I need to 
# check with someone else.
# In fact this is something that I need to check with either Andrew or
# Marc

### Results ####
summary(lower_spine_m2)
Anova(lower_spine_m2, type = "III")

## Spine Position ####
hist(spine_position$mean_all, breaks = 30)
# I think this is a poisson distribution?
#
spine_position_m2 <- glmmTMB(S_spine_position ~ mean_all  + 
                            (1|island/population),
                          data = spine_position,
                          REML = F)


# Filter outliers
hist(resid(spine_position_m2), breaks = 30)
spine_position$resid <- resid(spine_position_m2)

spine_position <- filter(spine_position, !resid >= 29)
# 4 samples removed

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(spine_position_m2))
# DHARMa
testResiduals(spine_position_m2)
# I think this model needs another tpye of distribution but I need to 
# check with someone else.
# In fact this is something that I need to check with either Andrew or
# Marc

### Results ####
summary(spine_position_m2)
Anova(spine_position_m2, type = "III")

pR2(spine_position_m2)["McFadden"]
# Estimated R2 0.1810015

## Tip distance ####
hist(tip_distance$mean_all, breaks = 30)

tip_distance_m2 <- glmmTMB(S_spine_tip_distance ~ mean_all + 
                       (1|island/population),
                     data = tip_distance,
                     REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(tip_distance_m2))

# DHARMa
testResiduals(tip_distance_m2)

### Results ####
summary(tip_distance_m2)
Anova(tip_distance_m2, type = "III")

## Width ####
hist(width$mean_all, breaks = 30)
width_filter <- filter(width, !(mean_all >= 3.8))

hist(width_filter$mean_all, breaks = 30)

# With regular dataset there is a convergence problem with tme model. 
# I filter out potential outliers

width_m2 <- glmmTMB(S_width ~ mean_all + 
                       (1|island/population),
                     data = width_filter,
                     REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(width_m2))

# DHARMa
testResiduals(width_m2)
hist(resid(width_m2), breaks = 50)
# I think I need to leave it because we have so few observations.

### Results ####
summary(width_m2)
Anova(width_m2, type = "III")
