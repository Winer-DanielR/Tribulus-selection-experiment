# Script 03.Univariate models with year as covariate ####
# By: Daniel Reyes Corral

# This script uses the individual datasets per trait from script 02 to run the models
# Models are organized by group comparisons and datasets.
# Model 1 compares mainland and island populations (used for mericarps and flowers)
# Model 2 compares Galapagos with the other island systems (used for flowers)
# For each trait I test model assumptions and transform data accordingly for convergence
# Data transformations are either log or squared roots.
# I made some of the outputs comments because I do need the models for making the plots
# but model comparisons were done before.
# Emmeans estimates:
# Use emmeans package for this_ Using "response" allows me to compare on the original scale for transformed data_
# This gives me an idea of how much change there is between the factors of interest_
# Using the that that worked best for each trait and model
# the following estimates LS means for the mainland_island effect for each response

# 03_01 Model 1: Mainland - Island ####
# 03_01_01 Mericarp data ####
## Length ####
# For length, the untransformed data seems the best
#### Raw data ####
meri_length_m1<- lmer(length ~ mainland_island +
                       year_collected +
                        Herbarium +
                        # Temp +
                        # Temp_S +
                        # Prec +
                        # varP +
                      (1|ID),
                      data = meri_length,
                      REML = F)
# #### Log transformed ####
# meri_length_m2<- lmer(log(length) ~ mainland_island +
#                         year_collected +
#                         Herbarium +
#                         (1|ID),
#                       data = meri_length,
#                       REML = F)
# #### Square-root transformed ####
# meri_length_m3<- lmer(sqrt(length) ~ mainland_island +
#                         year_collected +
#                         Herbarium +
#                         (1|ID),
#                       data = meri_length,
#                       REML = F)

#### ANOVA type II test ####
# Use the Anova function from the car package

Anova(meri_length_m1)

#summary(meri_length_m1)

#### Model Diagnostics ####
# I used a custom diagnostic functionfrom script 01 and the testResiduals function
# from the DHARMa package. This function shows the QQ residuals, dispersion test
# and outlier tests. 

# Diagnostic custom function
#par(mfrow = c(1, 3))
# Residual histograms distributions
#diagnostic(resid(meri_length_m1))
#diagnostic(resid(meri_length_m2))
#diagnostic(resid(meri_length_m3))

# Diagnostics with DHARMA
#testResiduals(meri_length_m1)
#testResiduals(meri_length_m2)
#testResiduals(meri_length_m3)

# After selecting the data transformation that converged the most I decided to
# only show the output of that model.

## Emmeans estimates: Length ####
EM_length <- emmeans(meri_length_m1, ~ mainland_island)

### Emmean plot: Length ####
plot(EM_length, comparisons = TRUE) + labs(title = "Mericarp Length")
pwpp(EM_length)
### Percentage difference ####
# ((island mean/mainland mean)-1)*100%
((6.16/5.72 - 1)* 100) # Mericarps ~ 7% longer on islands

## Width ####
# For width, squared transformed seemed the best
# #### Raw data ####
# meri_width_m1 <- lmer(width ~ mainland_island +
#                               year_collected +
#                               (1|ID),
#                       data = meri_width, 
#                       REML = F)
# #### Log transformed ####
# meri_width_m2 <- lmer(log(width) ~ mainland_island +
#                               year_collected +
#                               (1|ID),
#                       data = meri_width, 
#                       REML = F)
#### Square-root transformed ####
meri_width_m3 <- lmer(sqrt(width) ~ mainland_island +
                              year_collected +
                        Herbarium +
                              (1|ID),
                      data = meri_width, 
                      REML = F)
#### ANOVA type II test ####
Anova(meri_width_m3)

#### Model Diagnostics ####
# Residual histogram distributions
# diagnostic(resid(meri_width_m1))
# diagnostic(resid(meri_width_m2))
# diagnostic(resid(meri_width_m3))

# # DHARMa
# testResiduals(meri_width_m1)
# testResiduals(meri_width_m2)
# testResiduals(meri_width_m3)

## Emmean estimates: Width ####
EM_width <- emmeans(meri_width_m3, ~ mainland_island, type = "response")
### Emmean plot: Width ####
plot(EM_width, comparisons = T) + labs(title = "Mericarp Width")
pwpp(EM_width)
### Percentage difference ####
((3.15/2.96 - 1)*100) # Mericarps ~ 6.41% wider on islands


## Depth ####
# For depth untransformed data seems to work the best
#### Raw data ####
meri_depth_m1 <- lmer(depth ~ mainland_island +
                              year_collected +
                        Herbarium +
                              (1|ID),
                      data=meri_depth, 
                      REML = F)
#### Log transformed data ####
# meri_depth_m2 <- lmer(log(depth) ~ mainland_island +
#                               year_collected +
#                               (1|ID),
#                       data=meri_depth, 
#                       REML = F)
#### Square-root transformed data ####
# meri_depth_m3 <- lmer(sqrt(depth) ~ mainland_island +
#                               year_collected +
#                               (1|ID),
#                       data=meri_depth, 
#                       REML = F)
#### ANOVA type II test ####
Anova(meri_depth_m1)

#### Model Diagnostics ####
# hist(resid(meri_depth_m1), breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_depth_m1))
# diagnostic(resid(meri_depth_m2))
# diagnostic(resid(meri_depth_m3))
# 
# # DHARMa
# testResiduals(meri_depth_m1)
# testResiduals(meri_depth_m2)
# testResiduals(meri_depth_m3)

## Emmeans estimates: Depth ####
EM_depth <- emmeans(meri_depth_m1, ~ mainland_island)
### Emmeans plot: Depth ####
plot(EM_depth, comparisons = T) + labs(title = "Mericarp Depth")
pwpp(EM_depth)
### Percentage difference ####
((4.76/4.24 - 1)*100) # Mericarps ~ 12% deeper on islands

## Spine Tip distance ####
# For spine tip distance I ran models with all the samples.
# This includes mericarps without upper spines (Spine tip distance=0)

##### Models with all mericarps ####
# # Raw data looks the best
# meri_tip_distance_m1 <- lmer(tip_distance ~ mainland_island +
#                                      year_collected +
#                                Herbarium +
#                                      (1|ID),
#                              data=meri_tip_distance,REML=F)
# 
# meri_tip_distance_m2 <- lmer(log(tip_distance + 1) ~ mainland_island +
#                                      year_collected +
#                                      (1|ID),
#                              data=meri_tip_distance,REML=F)
# 
# meri_tip_distance_m3 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                      year_collected +
#                                      (1|ID),
#                              data=meri_tip_distance,REML=F)
# 
# 
# # ANOVA type II test
# Anova(meri_tip_distance_m1)

# Model Diagnostics
# Residual distributions
# hist(resid(meri_spine_length_m1), breaks = 20)
# Spine tip distance distribution
# hist(meri_tip_distance$tip_distance, breaks = 20)

# # Residual histograms
# diagnostic(resid(meri_tip_distance_m1))
# diagnostic(resid(meri_tip_distance_m2))
# diagnostic(resid(meri_tip_distance_m3))

# # DHARMa
# testResiduals(meri_tip_distance_m1)
# testResiduals(meri_tip_distance_m2)
# testResiduals(meri_tip_distance_m3)


##### Removed Zero Tip distance ####
# Then, I tried the models again. This time, removing the mericarps with
# spine tip distance of 0.
# This model did not converged well. There are some outliers.
# Raw data looks the best
meri_tip_distance_m4 <- lmer(tip_distance ~ mainland_island +
                                     year_collected +
                                     (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero,REML=F)

meri_tip_distance_m5 <- lmer(log(tip_distance + 1) ~ mainland_island +
                                     year_collected +
                                     (1|ID),
                             data=meri_tip_distance_wozero,REML=F)

meri_tip_distance_m6 <- lmer(sqrt(tip_distance) ~ mainland_island +
                                     year_collected +
                                     (1|ID),
                             data=meri_tip_distance_wozero,REML=F)


# ANOVA type II test
Anova(meri_tip_distance_m4)

# Model Diagnostics
# diagnostic(resid(meri_tip_distance_m4))
# diagnostic(resid(meri_tip_distance_m5))
# diagnostic(resid(meri_tip_distance_m6))

# The residual distribution showed some outliers. 
# Check residual distributions after removing mericarps without spines.
# I included the residual column into the dataset
meri_tip_distance_wozero$residuals <- resid(meri_tip_distance_m4)
# hist(resid(meri_tip_distance_m4), breaks = 20)
# hist(meri_tip_distance_wozero$tip_distance, breaks = 20)

# # DHARMa
# testResiduals(meri_tip_distance_m1)
# testResiduals(meri_tip_distance_m2)
# testResiduals(meri_tip_distance_m3)


# Based on the residual distributions and the trait distributions I filter the data

##### Filter residuals (used in analysis) ####
# I filter residuals that are lower than -5 and larger than 5.
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero,
                                          !residuals < -5)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter,
                                          !residuals > 5)

# This removes specimen no.383

# Create a new filtered dataset for spine tip distance
# hist(meri_tip_distance_wozero_filter$residuals, breaks = 20)
meri_tip_distance_wozero_filter <- filter(meri_tip_distance_wozero_filter, !is.na(residuals))

# Ran the models with this filter data and raw data works best
###### Raw data ####
meri_tip_distance_m7 <- lmer(tip_distance ~ mainland_island +
                                     year_collected +
                               Herbarium +
                                     (1|ID),
                             na.action = na.exclude,
                             data=meri_tip_distance_wozero_filter,REML=F)
###### Log transformed data ####
# meri_tip_distance_m8 <- lmer(log(tip_distance) ~ mainland_island +
#                                      year_collected +
#                                      (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_filter,REML=F)
###### Squared-root data ####
# meri_tip_distance_m9 <- lmer(sqrt(tip_distance) ~ mainland_island +
#                                      year_collected +
#                                      (1|ID),
#                              na.action = na.exclude,
#                              data=meri_tip_distance_wozero_filter,REML=F)


# Diagnostic
# diagnostic(resid(meri_tip_distance_m7))
# diagnostic(resid(meri_tip_distance_m8))
# diagnostic(resid(meri_tip_distance_m9))

#Anova
Anova(meri_tip_distance_m7)

# DHARMa
# testResiduals(meri_tip_distance_m7)

## Emmeans estimates: Spine tip distance ####
# Zero filter data
EM_tip_dist <- emmeans(meri_tip_distance_m7, ~ mainland_island)
### Emmeans plot: Spine tip distance ####
plot(EM_tip_dist, comparisons = T) + labs(title = "Mericarp Tip distance")
pwpp(EM_tip_dist)
### Percentage difference ####
((8.86/8.30 - 1)*100) # Mericarps ~ 6.66% more separated on islands

## Lower spines ####
# I tried to fit a glm
# For lower spines I used a glmm model and removed year as a covariate.
# meri_lower_spines_m1 <- glm(lower_spines ~ mainland_island + 
#                                    year_collected, 
#                              data = meri_lower_spines, 
#                              family = "binomial")

meri_lower_spines_m1_glmm <- glmmTMB(factor(lower_spines) ~ mainland_island +
                                       Herbarium +
                                             (1|ID),
                                     data = meri_lower_spines,
                                     family = binomial)
# str(meri_lower_spines)
### ANOVA Type II test ####
# Anova(meri_lower_spines_m1)
Anova(meri_lower_spines_m1_glmm)

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















