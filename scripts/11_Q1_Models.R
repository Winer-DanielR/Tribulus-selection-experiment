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

length_m1 <- glmmTMB(eaten ~ length + 
                    (1|island/population), #This is population nested with island
                  data = length,
                  family = binomial(link = "logit"),
                  REML = F)

# When islands are included we get a warning message for convergence problems.

### Model diagnostics ####
# diagnostic(resid(length_m1)) # The untransformed data seem to work fine.
testResiduals(length_m1)

### Results ####
summary(length_m1)
Anova(length_m1) # It seems is significant.

# ### Emmeans: Length ####
# EM_length <- emmeans(length_m1, ~ length)
# 
# #### Emmean plot: Length ####
# plot(EM_length, comparisons = TRUE) + labs(title = "Mericarp Length")
# pwpp(EM_length)

## Width ####
## Trait distributions
histogram(width$width, breaks = 50)
# It seems there are some outliers, we could remove the ones larger than 6.
#width <- filter(width, !width > 6) # 10 mericarps removed

width_m1 <- glmmTMB(eaten ~ width + 
                    (1|island/population),
                  data = width,
                  family = binomial(link = "logit"),
                  REML = F)

### Model diagnostics ####
# diagnostic(resid(width_m1))
 testResiduals(width_m1)

# ### Width data adjustments ####
# # Check residual distributions.
# hist(resid(width_m1), breaks = 50) # It seems that mericarps with 5 mm of with are outliers
# 
# # Using the residuals shows more outliers, so I can filter them further.
# # Include residuals on the data set.
# width$residuals <- resid(width_m1)
# 
# # I filter residuals that are lower than -5 and larger than 5.
# width_filter <- filter(width, !residuals > 2)
# width_filter <- filter(width_filter, !residuals < -2)
# 
# hist(width_filter$residuals, breaks = 50)
# 
# width_m1 <- glmmTMB(width ~ eaten + 
#                       (1|island/population),
#                     data = width_filter,
#                     REML = F)
# 
# # RUN again with width_filter!
# ### 
# ### Model diagnostics ####
#  # diagnostic(resid(width_m1))
#  # testResiduals(width_m1)

### Results ####
summary(width_m1)
Anova(width_m1) # Not significant.
 
# ## Emmean: Width ####
# EM_width <- emmeans(width_m1, ~ eaten, type = "response")
# ### Emmean plot: Width ####
# plot(EM_width, comparisons = T) + labs(title = "Mericarp Width")
# pwpp(EM_width)

## Depth ####
# Check distribution
histogram(depth$depth, breaks = 50)
# # It seems there are outliers above 8 mm that I could remove
# depth <- filter(depth, !depth > 8) # 7 mericarps removed

depth_m1 <- glmmTMB(eaten ~ depth + 
                   (1|island/population),
                 data = depth,
                 family = binomial(link = "logit"),
                 REML = F)

# #### Model Diagnostics ####
# hist(resid(depth_m1), breaks = 50)

# # Residual histograms
# diagnostic(resid(depth_m1))
# DHARMa
 testResiduals(depth_m1)

# Results
summary(depth_m1)
Anova(depth_m1)

# ## Emmeans estimates: Depth ####
# EM_depth <- emmeans(depth_m1, ~ eaten)
# ### Emmeans plot: Depth ####
# plot(EM_depth, comparisons = T) + labs(title = "Mericarp Depth")
# pwpp(EM_depth)

## Longest spine - Removed 0 ####
# Check trait distributions
histogram(longest_spine_wozero$longest_spine, breaks = 50)
# It seems the distribution is not totally normal?
# I need to transform it

longest_spine_m1 <- glmmTMB(eaten ~ longest_spine +
                            (1|island/population),
                            data=longest_spine_wozero,
                            family = binomial(link = "logit"),
                            REML=F)

### Model Diagnostics ####
# diagnostic(resid(longest_spine_m1))
# # DHARMa
 testResiduals(longest_spine_m1)

### Results ####
summary(longest_spine_m1)
Anova(longest_spine_m1)

# ## Emmeans estimates: Longest spine ####
# # Zero filter data
# EM_long_spine <- emmeans(longest_spine_m1, ~ eaten)
# ### Emmeans plot: Longest spine ####
# plot(EM_long_spine, comparisons = T) + labs(title = "Longest Spine")
# pwpp(EM_long_spine)

## Tip distance - Removed 0 ####
# Check distributions
histogram(tip_distance_wozero$spine_tip_distance, breaks = 50)
# This trait may have outliers below 5

tip_distance_m1 <- glmmTMB(eaten ~ spine_tip_distance +
                              (1|island/population),
                           family = binomial(link = "logit"),
                            data=tip_distance_wozero,REML=F)

### Model diagnostics ####
#diagnostic(resid(tip_distance_m1))
testResiduals(tip_distance_m1)

### Results ####
summary(tip_distance_m1)
Anova(tip_distance_m1)

# ## Emmeans estimates: Tip distance ####
# # Zero filter data
# EM_tip_distance <- emmeans(tip_distance_m1, ~ eaten)
# ### Emmeans plot: Longest spine ####
# plot(EM_tip_distance, comparisons = T) + labs(title = "Tip Distance")
# pwpp(EM_tip_distance)


## Lower spines ####
# Lower spines are binomial

lower_spine_m1 <- glmmTMB(eaten ~ factor(lower_spine) +
                                       (1|island/population),
                                     data = lower_spines,
                                     family = binomial(link = "logit"))

### Model Diagnostics ####
#diagnostic(resid(lower_spine_m1))
testResiduals(lower_spine_m1)

### Results ####
summary(lower_spine_m1)
Anova(lower_spine_m1)


# ## Emmeans estimates: Lower spines ####
# #Glmm
# EM_lower <- emmeans(lower_spine_m1, ~ lower_spine, type = "response")
# ### Emmeans plot: Lower spines ####
# plot(EM_lower, comparisons = T) + labs(title = "Mericarp Lower Spines")
# pwpp(EM_lower)

## Spine position ####
### Spine position is categorical
spine_position_m1 <- glmmTMB(eaten ~ factor(spine_position) +
                            (1|island/population),
                          data = spine_position,
                          family = binomial(link = "logit"))

### Model Diagnostics ####
#diagnostic(resid(spine_position_m1))
testResiduals(spine_position_m1)

### Results ####
summary(spine_position_m1)
Anova(spine_position_m1, type = "III", contrasts=list(topic=contr.sum, sys=contr.sum))

## Emmeans estimates: Lower spines ####
#Glmm
EM_spine_position <- emmeans(spine_position_m1, ~ eaten, type = "response")
### Emmeans plot: Lower spines ####
plot(EM_spine_position, comparisons = T) + labs(title = "Spine Position")
pwpp(EM_spine_position)
