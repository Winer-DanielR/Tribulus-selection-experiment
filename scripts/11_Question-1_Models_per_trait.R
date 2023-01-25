# Question 1. Model preparation script ####
# By: Daniel Reyes Corral

# This script uses the point in time dataset per trait from script 10
# to run the model. Are Galápagos mericarps generally under selection?
# We used a GLMM with mericarp traits 
# (size traits: length, depth, width and depth; 
# and spine traits: longest spine, spine tip distance, lower spines) 
# as predictor of survival (eaten/uneaten mericarps). 
# We used Year, Island, and Population (nested within Island) as random factors

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
# Preparing datasets for univariate analysis.
## Select per traits to check and remove NAs
## Length ####
length <- select(point_time, c(1:8), eaten)
length <- na.omit(length)
length <- filter(length, !is.na(length)) 
## Width ####
width <- select(point_time, c(1:7), width, eaten)
width <- na.omit(width)
width <- filter(width, !is.na(width)) 
## Depth ####
depth <- select(point_time, c(1:7), depth, eaten)
depth <- na.omit(depth)
## Longest spine ####
longest_spine <- select(point_time, c(1:7), longest_spine, eaten)
longest_spine <- na.omit(longest_spine)
### Longest spine remove 0s ####
longest_spine_wozero <- dplyr::filter(longest_spine, !longest_spine == 0)
## Spine tip distance ####
tip_distance <- select(point_time, c(1:7), spine_tip_distance, eaten)
tip_distance <- na.omit(tip_distance)
### Spine tip distance without zero #####
# We removed mericarps without upper spines from analysis.These mericarps had a tip distance of 0.
tip_distance_wozero <- dplyr::filter(tip_distance, !spine_tip_distance == 0)
## Lower spines ####
## Lower spines is a binomial trait
lower_spines <- select(point_time, c(1:7), lower_spine, eaten)
lower_spines <- na.omit(lower_spines)
## Spine position (as factor) ####
spine_position <- select(point_time, c(1:7), spine_position, eaten)
spine_position <- na.omit(spine_position)

# ======== Models ===========

## Length ####
# All of the covariates are random effects. For now this is the simplest model.

# Check the distribution of the trait.
histogram(length$length, breaks = 50)

length_m1 <- glmmTMB(length ~ eaten + 
                    (1|island/population), #This is population nested with island
                  data = length,
                  REML = F)

# When islands are included we get a warning message for convergence problems.

### Model diagnostics ####
# diagnostic(resid(length_m1)) # The untransformed data seem to work fine.
testResiduals(length_m1)

### Results ####
summary(length_m1)
Anova(length_m1) # It seems is significant.

### Emmeans: Length ####
EM_length <- emmeans(length_m1, ~ eaten)

#### Emmean plot: Length ####
plot(EM_length, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length)

## Width ####
## Trait distributions
histogram(width$width, breaks = 50)
# It seems there are some outliers, we could remove the ones larger than 6.
width <- filter(width, !width > 6) # 10 mericarps removed
 
width_m1 <- glmmTMB(width ~ eaten + 
                    (1|island/population),
                  data = width,
                  REML = F)

### Model diagnostics ####
# diagnostic(resid(width_m1))
# testResiduals(width_m1)

### Width data adjustments ####
# Check residual distributions.
hist(resid(width_m1), breaks = 50) # It seems that mericarps with 5 mm of with are outliers

# Using the residuals shows more outliers, so I can filter them further.
# Include residuals on the data set.
width$residuals <- resid(width_m1)

# I filter residuals that are lower than -5 and larger than 5.
width_filter <- filter(width, !residuals > 2)
width_filter <- filter(width_filter, !residuals < -2)

hist(width_filter$residuals, breaks = 50)

width_m1 <- glmmTMB(width ~ eaten + 
                      (1|island/population),
                    data = width_filter,
                    REML = F)

# RUN again with width_filter!
### 
### Model diagnostics ####
 # diagnostic(resid(width_m1))
 # testResiduals(width_m1)

### Results ####
summary(width_m1)
Anova(width_m1) # Not significant.
 
## Emmean: Width ####
EM_width <- emmeans(width_m1, ~ eaten, type = "response")
### Emmean plot: Width ####
plot(EM_width, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width)

## Depth ####
# Check distribution
histogram(depth$depth, breaks = 50)
# It seems there are outliers above 8 mm that I could remove
depth <- filter(depth, !depth > 8) # 7 mericarps removed

depth_m1 <- glmmTMB(depth ~ eaten + 
                   (1|island/population),
                 data = depth,
                 REML = F)

#### Model Diagnostics ####
hist(resid(depth_m1), breaks = 50)

# Residual histograms
diagnostic(resid(depth_m1))

# DHARMa
 testResiduals(depth_m1)

# Results
summary(depth_m1)
Anova(depth_m1)

## Emmeans estimates: Depth ####
EM_depth <- emmeans(depth_m1, ~ eaten)
### Emmeans plot: Depth ####
plot(EM_depth, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth)

## Removed Zero longest spine ####
# Check trait distributions
histogram(longest_spine_wozero$longest_spine, breaks = 50)
# It seems the distribution is not totally normal?
# I need to transform it

longest_spine_m1 <- glmmTMB(longest_spine ~ eaten +
                                     (1|island/population),
                                     data=longest_spine_wozero,REML=F)

# Model Diagnostics
 diagnostic(resid(longest_spine_m1))
 # It seems that there is a difference in the way this trait was measured

### Longest spine data adjustments #### 
 ggplot(longest_spine_wozero) +
   aes(x = longest_spine, y = year) +
   geom_boxplot(fill = "#112446") +
   theme_minimal()
# There is a difference between 2015 - 2016 to 2017 - 2019. 
# I will filter 2015 - 2016 from the model

 longest_spine_wozero <- filter(longest_spine_wozero, !year == "2015")
 longest_spine_wozero <- filter(longest_spine_wozero, !year == "2016")
 
# After removing these years, I check the distribution of the trait again

 histogram(longest_spine_wozero$longest_spine, breaks = 50)
# Now the distribution looks more normal
# And run the model again
  longest_spine_m1 <- glmmTMB(longest_spine ~ eaten +
                               (1|island/population),
                             data=longest_spine_wozero,REML=F)

 # Model Diagnostics
 diagnostic(resid(longest_spine_m1))
# # DHARMa
 testResiduals(longest_spine_m1)

# Results
summary(longest_spine_m1)
Anova(longest_spine_m1)

## Emmeans estimates: Longest spine ####
# Zero filter data
EM_long_spine <- emmeans(longest_spine_m1, ~ eaten)
### Emmeans plot: Spine tip distance ####
plot(EM_long_spine, comparisons = T) + labs(title = "Longest Spine")
pwpp(EM_long_spine)



## Lower spines ####
# I tried to fit a glm
# For lower spines I used a glmm model and removed year as a covariate.
# meri_lower_spines_m1 <- glm(lower_spines ~ mainland_island + 
#                                    year_collected, 
#                              data = meri_lower_spines, 
#                              family = "binomial")

lower_spine_m1 <- glmmTMB(factor(lower_spine) ~ eaten +
                                       (1|island/population),
                                     data = lower_spines,
                                     family = binomial)

spine_position_m1 <- glmmTMB(factor(spine_position) ~ eaten +
                            (1|island/population),
                          data = spine_position,
                          family = binomial(link = "logit"))

# str(meri_lower_spines)
### ANOVA Type II test ####
# Anova(meri_lower_spines_m1)
Anova(lower_spine_m1)

### Model Diagnostics ####
# # Residual histograms
#diagnostic(resid(meri_lower_spines_m1_glmm))

## Emmeans estimates: Lower spines ####
#Glmm
EM_lower <- emmeans(meri_lower_spines_m1_glmm, ~ mainland_island, type = "response")
### Emmeans plot: Lower spines ####
plot(EM_lower, comparisons = T) + labs(title = "Mericarp Lower Spines")
pwpp(EM_lower)

# Other traits models (removed from analysis) ####
## Spine length (removed from analysis) ####
# # Spine lenght was not used for the current main analysis
# # Spine lenght was only collected for natural populations but not from herbarium
# # Herbarium samples were collected before considering including this trait
# 
# # I think the untransformed works best but the 0s may affect the results
# meri_spine_length_m1 <-lmer(spine_length ~ mainland_island +
#                                     year_collected +
#                                     (1|ID),
#                             data=meri_spine_length,
#                             REML=F)
# 
# meri_spine_length_m2 <-lmer(log(spine_length + 1) ~ mainland_island +
#                                     year_collected +
#                                     (1|ID),
#                             data=meri_spine_length,
#                             REML=F)
# 
# meri_spine_length_m3 <-lmer(sqrt(spine_length) ~ mainland_island +
#                                     year_collected +
#                                     (1|ID),
#                             data=meri_spine_length,
#                             REML=F)
# 
# # type III test
# # Anova(meri_spine_length_m1)
# 
# # Diagnostic
# # # Residual histograms
# # diagnostic(resid(meri_spine_length_m1))
# # diagnostic(resid(meri_spine_length_m2))
# # diagnostic(resid(meri_spine_length_m3))
# # hist(resid(meri_spine_length_m1), breaks = 20)
# 
# # # DHARMa
# # testResiduals(meri_spine_length_m1)
# # testResiduals(meri_spine_length_m2)
# # testResiduals(meri_spine_length_m3)
# # testZeroInflation(meri_depth_m1)
# 
# Emmeans
# Zero filter data
# EM_spine <- emmeans(meri_spine_length_wozero_m3, ~ mainland_island)
# plot(EM_spine, comparisons = T) + labs(title = "Mericarp Spine Length")
# pwpp(EM_spine)
# ((2.12/2.08 - 1)* 100) # Upper spines are 1.92% longer on islands than on the mainland 

# ##### Removed Zero Spine Length
# # Square transformed data seems works best
# meri_spine_length_wozero_m1 <-lmer(spine_length ~ mainland_island +
#                                            year_collected +
#                                            (1|ID),
#                                    data=meri_spine_length_wozero,
#                                    REML=F)
# 
# meri_spine_length_wozero_m2 <-lmer(log(spine_length + 1) ~ mainland_island +
#                                            year_collected +
#                                            (1|ID),
#                                    data=meri_spine_length_wozero,
#                                    REML=F)
# 
# meri_spine_length_wozero_m3 <-lmer(sqrt(spine_length) ~ mainland_island +
#                                            year_collected +
#                                            (1|ID),
#                                    data=meri_spine_length_wozero,
#                                    REML=F)
# 
# # type III test
# Anova(meri_spine_length_wozero_m3)
# 
# # Diagnostic
# 
# # # Residual histograms
# # diagnostic(resid(meri_spine_length_wozero_m1))
# # diagnostic(resid(meri_spine_length_wozero_m2))
# # diagnostic(resid(meri_spine_length_wozero_m3))
# 
# # # DHARMa
# # testResiduals(meri_spine_length_wozero_m1)
# # testResiduals(meri_spine_length_wozero_m2)
# # testResiduals(meri_spine_length_wozero_m3)
# # plotResiduals(meri_spine_length_wozero_m3)
# 
# # Filter Spine Lenght without zero
# # Removed values > 10
# # Untransformed data seems to work best but either model without zeros or filtered
# 
# spine_length_filter <- filter(meri_spine_length_wozero, !(spine_length >= 10))
# hist(spine_length_filter$spine_length, breaks = 20)
# 
# spine_length_filter_m1 <-lmer(spine_length ~ mainland_island +
#                                       year_collected +
#                                       (1|ID),
#                               data=spine_length_filter,
#                               REML=F)
# 
# spine_length_filter_m2 <-lmer(log(spine_length + 1) ~ mainland_island +
#                                       year_collected +
#                                       (1|ID),
#                               data=spine_length_filter,
#                               REML=F)
# 
# spine_length_filter_m3 <-lmer(sqrt(spine_length) ~ mainland_island +
#                                       year_collected +
#                                       (1|ID),
#                               data=spine_length_filter,
#                               REML=F)
# 
# # type III test
# Anova(spine_length_filter_m3)
# 
# # Diagnostic
# 
# # # Residual histograms
# #diagnostic(resid(spine_length_filter_m1))
# #diagnostic(resid(spine_length_filter_m2))
# #diagnostic(resid(spine_length_filter_m3))
# 
# # # DHARMa
# #testResiduals(spine_length_filter_m1)
# # testResiduals(spine_length_filter_m2)
# # testResiduals(spine_length_filter_m3)
# # # plotResiduals(spine_length_filter_m1)

## Upper spines (removed from analysis) ####
# Not used in the analysis
# # meri_upper_spines_m1 <- glm(upper_spines ~ mainland_island + 
# #                                year_collected, 
# #                              data = meri_upper_spines, 
# #                              family = "binomial")
# 
# meri_upper_spines_m1_glmm <- glmmTMB(upper_spines ~ mainland_island +
#                                              (1|ID),
#                                      data = meri_upper_spines,
#                                      family = binomial)
# 
# # # Type III test
# #Anova(meri_upper_spines_m1)
# Anova(meri_upper_spines_m1_glmm)
# 
# # # Diagnostic
# # 
# # # Residual histograms
# #diagnostic(resid(meri_upper_spines_m1_glmm))
# 
# # # DHARMa
# # testResiduals(meri_lower_spines_m1)

## Emmean estimates
# EM_upper <- emmeans(meri_upper_spines_m1_glmm, ~ mainland_island)
# plot(EM_lower) + labs(title = "Mericarp Lower Spines")

# 03_01_02 Flower dataset ####
## Petal length ####
# The raw data works best
### Raw data ####
flower_m1 <- lmer(petal_length ~ mainland_island +
                          year_collected +
                          (1|ID),
                  data=flower,
                  na.action = na.exclude,
                  REML=F)
### Log transformed ####
# flower_m2 <- lmer(log(petal_length) ~ mainland_island +
#                           year_collected +
#                           (1|ID),
#                   data=flower,
#                   REML=F)
### Square-root transformed ####
# flower_m3 <- lmer(sqrt(petal_length) ~ mainland_island +
#                           year_collected +
#                           (1|ID),
#                   data=flower,
#                   REML=F)

### ANOVA type II test ####
Anova(flower_m1)

### Model diagnostics ####
# hist(flower$petal_length, breaks = 20)
# The distribution of petal length have some outliers.

# Create a column with model residuals to filter the dataset
flower$residuals <- resid(flower_m1) 
hist(resid(flower_m1), breaks = 20)
# Filter residuals outside -5 and 5
flower_filter <- filter(flower, !is.na(residuals))
flower_filter <- filter(flower_filter, !residuals >=5)
flower_filter <- filter(flower_filter, !residuals <= -5)
hist(flower_filter$residuals, breaks = 20)

# This filter removed specimens 240, 351, 320 (<-5) and
# 454, 319, 207, 249, 340 (>5)

# Filtered model
# Raw data works best in the filter model
flower_m4 <- lmer(petal_length ~ mainland_island +
                          year_collected +
                          (1|ID),
                  data=flower_filter,
                  na.action = na.exclude,
                  REML=F)

# flower_m5 <- lmer(log(petal_length) ~ mainland_island +
#                           year_collected +
#                           (1|ID),
#                   data=flower_filter,
#                   na.action = na.exclude,
#                   REML=F)
# 
# flower_m6 <- lmer(sqrt(petal_length) ~ mainland_island +
#                           year_collected +
#                           (1|ID),
#                   data=flower_filter,
#                   na.action = na.exclude,
#                   REML=F)

# #type III test using lmertest
Anova(flower_m4)

# # Residual histograms
# diagnostic(resid(flower_m1))
# diagnostic(resid(flower_m2))
# diagnostic(resid(flower_m3))
#diagnostic(resid(flower_m4))
# diagnostic(resid(flower_m5))
# diagnostic(resid(flower_m6))

# # DHARMa
# testResiduals(flower_m1)
# testResiduals(flower_m4)
# testResiduals(flower_m3)
#testResiduals(flower_m4)

## Emmean estimates: Petal length ####
EM_flower <- emmeans(flower_m4, ~ mainland_island)
### Emmean plot: Petal length ####
plot(EM_flower, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower)
### Percentange difference ####
((16.3/16.7 - 1)*100) # Flowers are -2% smaller on islands

# 03_02 Model 2: Galapagos - Other Islands ####
# Is there an effect between tribulus in galapagos compared to other island systems
# This comparison is possible with flower and leaf datasets
# 03_02_01 Flower dataset ####
## Petal length data ####
# I used the flower filter data from before
# The squared data seems to work best
### Raw data ####
# flower_m7 <- lmer(petal_length ~ galapagos_other +
#                     year_collected +
#                     (1|ID),
#                   data=flower_filter,
#                   REML=F)
### Log transformed ####
# flower_m8 <- lmer(log(petal_length) ~ galapagos_other +
#                     year_collected +
#                     (1|ID),
#                   data=flower_filter,
#                   REML=F)
### Square root transformed ####
flower_m9 <- lmer(sqrt(petal_length) ~ galapagos_other +
                    year_collected +
                    (1|ID),
                  data=flower_filter,
                  REML=F)

### ANOVA type II test ####
Anova(flower_m9)

### Model Diagnostics ####
# # Residual histograms
# diagnostic(resid(flower_m7))
# diagnostic(resid(flower_m8))
#diagnostic(resid(flower_m9))
# 
# # DHARMa
# testResiduals(flower_m7)
# testResiduals(flower_m8)
# testResiduals(flower_m9)

## Emmean estimates: Petal length ####
EM_flower2 <- emmeans(flower_m9, ~ galapagos_other, type = "response")
### Emmean plot: Petal length ####
plot(EM_flower2, comparisons = T) + labs(title = "Flower Length")
pwpp(EM_flower2)
### Percentage difference ####
((9.18/17.06 - 1)*100) #FLowers in Galapagos are 46% shorter than other islands

# 03_04 Spine tip distance compared between lower spines ####
# I think we can include this as a way to explain the
# potential outcomes of lower spines?
# This is still significant even with the bioclimate variables

## Spine Tip distance ####
# For spine tip distance I ran models with all the samples.
# This includes mericarps without upper spines (Spine tip distance=0)

##### Models with all mericarps ####
# Raw data looks the best
meri_tip_distance_lower1 <- lmer(tip_distance ~ lower_spines +
                               year_collected +
                               (1|ID),
                             data=mericarp,REML=F)

meri_tip_distance_lower2 <- lmer(log(tip_distance + 1) ~ lower_spines +
                               year_collected +
                               (1|ID),
                             data=mericarp,REML=F)

meri_tip_distance_lower3 <- lmer(sqrt(tip_distance) ~ lower_spines +
                               year_collected +
                               (1|ID),
                             data=mericarp,REML=F)


# ANOVA type II test
Anova(meri_tip_distance_lower1)
Anova(meri_tip_distance_lower2)
Anova(meri_tip_distance_lower3)

# Model Diagnostics
# Residual distributions
hist(resid(meri_tip_distance_lower1), breaks = 20)
# Spine tip distance distribution
hist(meri_tip_distance$tip_distance, breaks = 20)

# # Residual histograms
diagnostic(resid(meri_tip_distance_lower1))
diagnostic(resid(meri_tip_distance_lower2))
diagnostic(resid(meri_tip_distance_lower3))

# # DHARMa
testResiduals(meri_tip_distance_lower1)
testResiduals(meri_tip_distance_lower2)
testResiduals(meri_tip_distance_lower3)


##### Removed Zero Tip distance ####
# Then, I tried the models again. This time, removing the mericarps with
# spine tip distance of 0.
mericarp_wozero <- dplyr::filter(mericarp, !tip_distance == 0)
# This model did not converged well. There are some outliers.
# Raw data looks the best
meri_tip_distance_lower4 <- lmer(tip_distance ~ lower_spines +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=mericarp_wozero,REML=F)

meri_tip_distance_lower5 <- lmer(log(tip_distance + 1) ~ lower_spines +
                               year_collected +
                               (1|ID),
                             data=mericarp_wozero,REML=F)

meri_tip_distance_lower6 <- lmer(sqrt(tip_distance) ~ lower_spines +
                               year_collected +
                               (1|ID),
                             data=mericarp_wozero,REML=F)


# ANOVA type II test
Anova(meri_tip_distance_lower4)
Anova(meri_tip_distance_lower5)
Anova(meri_tip_distance_lower6)

# Model Diagnostics
diagnostic(resid(meri_tip_distance_lower4))
diagnostic(resid(meri_tip_distance_lower5))
diagnostic(resid(meri_tip_distance_lower6))

# The residual distribution showed some outliers. 
# Check residual distributions after removing mericarps without spines.
# I included the residual column into the dataset
mericarp_wozero$tip_dist_residuals <- resid(meri_tip_distance_lower4)
hist(resid(meri_tip_distance_lower4), breaks = 20)


# # DHARMa
 testResiduals(meri_tip_distance_lower4)
 testResiduals(meri_tip_distance_lower5)
 testResiduals(meri_tip_distance_lower6)


# Based on the residual distributions and the trait distributions I filter the data

##### Filter residuals (used in analysis) ####
# I filter residuals that are lower than -5 and larger than 5.
meri_tip_distance_lower_filter <- filter(mericarp_wozero,
                                          !tip_dist_residuals < -5)
meri_tip_distance_lower_filter <- filter(mericarp_wozero,
                                          !tip_dist_residuals > 5)

hist(meri_tip_distance_lower_filter$tip_dist_residuals, breaks = 20)
# Ran the models with this filter data and raw data works best
###### Raw data ####
meri_tip_distance_lower7 <- lmer(tip_distance ~ lower_spines +
                               year_collected +
                                 Herbarium +
                                 #Temp +
                                 #Temp_S + 
                                 #Prec +
                                 #varP +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_lower_filter,REML=F)
###### Log transformed data ####
meri_tip_distance_lower8 <- lmer(log(tip_distance) ~ lower_spines +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_lower_filter,REML=F)
###### Squared-root data ####
meri_tip_distance_lower9 <- lmer(sqrt(tip_distance) ~ lower_spines +
                               year_collected +
                               (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_lower_filter,REML=F)


# Diagnostic
 diagnostic(resid(meri_tip_distance_lower7))
 diagnostic(resid(meri_tip_distance_lower8))
 diagnostic(resid(meri_tip_distance_lower9))

#Anova
Anova(meri_tip_distance_lower7)

# DHARMa
 testResiduals(meri_tip_distance_lower7)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist_lower <- emmeans(meri_tip_distance_lower7, ~ lower_spines)
### Emmeans plot: Spine tip distance ####
plot(EM_tip_dist_lower, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist_lower)
### Percentage difference ####
((9.16/8.38 - 1)*100) # Mericarps ~ 9% more separated on mericarps with lower spines than without.















