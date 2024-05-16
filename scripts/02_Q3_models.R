# Data loading ####
## Individual traits ####
### Mean Populations ####
depth_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_population.csv")
length_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_population.csv")
longest_spine_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_population.csv")
tip_distance_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_population.csv")
width_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_population.csv")
lower_spine_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_pop.csv")
spine_position_pop <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_pop.csv")

### PCA Populations ####
pca_means_Q3 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_populations_bioclimate.csv")


# Data preparation ####
# Making char into factors (island, populations)
# 
## Individual traits #####

depth_pop <- depth_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
length_pop <- length_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
longest_spine_pop  <- longest_spine_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
lower_spine_pop <- lower_spine_pop %>% mutate_at(vars(island, population, lower_spine, finch_beak), list(factor))
spine_position_pop <- spine_position_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
tip_distance_pop <- tip_distance_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
width_pop <- width_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))

## PCA traits ####
pca_means_Q3 <- pca_means_Q3 %>% mutate_at(vars(island, population, finch_beak), list(factor))
str(pca_means_Q3)
pca_means_Q3 <- na.omit(pca_means_Q3)


# Models ####
# The structure of this model is to test the effect of mean trait values
# The structure of the model is based on the question that bioclimate variables are
# predictors of size trait and selection

## Size (PC1) ####
### null models for estimatd R2 ####
null_Mean_Size <- glmmTMB(Size_mean ~ 1 + (1|island), data = pca_means_Q3, REML = F)
null_S_Size <- glmmTMB(S_Size ~ 1 + (1|island), data = pca_means_Q3, REML = F)


### Q3 Models Size ####
S_sizeQ3 <- glmmTMB(S_Size ~ PC1_bioclimate +
                    PC2_bioclimate +
                    PC3_bioclimate +
                    PC4_bioclimate +
                    finch_beak +
                    (1|island),
                  data = pca_means_Q3,
                  REML = F) 


Size_meanQ3 <- glmmTMB(Size_mean ~ PC1_bioclimate +
                      PC2_bioclimate +
                      PC3_bioclimate +
                      PC4_bioclimate +
                      finch_beak +
                      (1|island),
                    data = pca_means_Q3,
                    REML = F) 

### Model Diagnostics ####
#### Custom function ####
# Custom diagnostic function (see script 04) that test models
# Estimate Kurtosis and Skewness
diagnostic(resid(S_sizeQ3))
diagnostic(resid(Size_meanQ3))

#### DHARMa ####
# Test residuals, QQplot, dispersion test and outliers
testResiduals(S_sizeQ3)
testResiduals(Size_meanQ3)

## Results ####
### Mean Size ####
summary(Size_meanQ3)
Anova(Size_meanQ3, type = "2")
### Mean Selection Size ####
summary(S_sizeQ3)
Anova(S_sizeQ3, type = "2")

### R estimates ####
#### Mean trait ####
r.squaredGLMM(Size_meanQ3, null = null_Mean_Size)

#### Mean Selection Estimate ####
r.squaredGLMM(S_sizeQ3, null = null_S_Size)

## Defense (PC2) ####
### null models for estimated R2 ####
null_Mean_Defense <- glmmTMB(Defense_mean ~ 1 + (1|island), data = pca_means_Q3, REML = F)
null_S_Defense <- glmmTMB(S_Defense ~ 1 + (1|island), data = pca_means_Q3, REML = F)

### Q3 models defense ####
Mean_DefenseQ3 <- glmmTMB(Defense_mean ~ PC1_bioclimate +
                         PC2_bioclimate +
                         PC3_bioclimate +
                         PC4_bioclimate +
                         finch_beak +
                         (1|island),
                       data = pca_means_Q3,
                       REML = F)

S_DefenseQ3 <- glmmTMB(S_Defense ~ PC1_bioclimate +
                         PC2_bioclimate +
                         PC3_bioclimate +
                         PC4_bioclimate +
                         finch_beak +
                         (1|island),
                       data = pca_means_Q3,
                       REML = F)


## Model Diagnostics ####
### Custom function ####
diagnostic(resid(Mean_DefenseQ3))
diagnostic(resid(S_DefenseQ3))

### DHARMa ####
testResiduals(Mean_DefenseQ3)
testResiduals(S_DefenseQ3)


### Results ####
summary(Mean_DefenseQ3)
summary(S_DefenseQ3)

#### Mean Defense ####
Anova(Mean_DefenseQ3, type = "2")
#### Mean Selection Estimate ####
Anova(S_DefenseQ3, type = "2")

### R estimates ####
r.squaredGLMM(Mean_DefenseQ3, null = null_Mean_Defense)
r.squaredGLMM(S_DefenseQ3, null = null_S_Defense)

## Position (PC3) ####
### null models for estimated R2 ####
null_Mean_Position <- glmmTMB(Position_mean ~ 1 + (1|island), data = pca_means_Q3, REML = F)
null_S_Position <- glmmTMB(S_Position ~ 1 + (1|island), data = pca_means_Q3, REML = F)

### Q3 MOdels Position ####
Mean_PositionQ3 <- glmmTMB(Position_mean ~ PC1_bioclimate +
                          PC2_bioclimate +
                          PC3_bioclimate +
                          PC4_bioclimate +
                          finch_beak +
                          (1|island),
                        data = pca_means_Q3,
                        REML = F)

S_PositionQ3 <- glmmTMB(S_Position ~ PC1_bioclimate +
                          PC2_bioclimate +
                          PC3_bioclimate +
                          PC4_bioclimate +
                          finch_beak +
                          (1|island),
                        data = pca_means_Q3,
                        REML = F)

 
### Model Diagnostics ####
# Residual histograms
diagnostic(resid(Mean_PositionQ3))
diagnostic(resid(S_PositionQ3))

# DHARMa
testResiduals(Mean_PositionQ3)
testResiduals(S_PositionQ3)

### Results ####
summary(Mean_PositionQ3)
summary(S_PositionQ3)

#### Mean trait Position ####
Anova(Mean_PositionQ3, type = "2")
#### Mean selection estimate ####
Anova(S_PositionQ3, type = "2")

#### R estimates ####
r.squaredGLMM(Mean_PositionQ3)
r.squaredGLMM(S_PositionQ3)


# Individual Trait Models ####
## Depth ####
hist(depth_pop$S_depth, breaks = 10)
depth_pop <- na.omit(depth_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_depth_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                        PC2_bioclimate +
                        PC3_bioclimate +
                        PC4_bioclimate +
                        finch_beak +
                        (1|island),
                      data = depth_pop,
                      REML = F)

S_depth_m1 <- glmmTMB(S_depth ~ PC1_bioclimate +
                           PC2_bioclimate +
                           PC3_bioclimate +
                           PC4_bioclimate +
                           finch_beak +
                           (1|island),
                         data = depth_pop,
                         REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(mean_depth_m1))
diagnostic(resid(S_depth_m1))


# DHARMa
testResiduals(mean_depth_m1)
testResiduals(S_depth_m1)


### Results ####
summary(S_depth_m1)
summary(mean_depth_m1)

Anova(S_depth_m1, type = "II")
Anova(mean_depth_m1, type = "II")
# For depth, two biolcimate variables did not converged

## Length ####
hist(length_pop$S_length, breaks = 10)
length_pop <- na.omit(length_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_length_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                           PC2_bioclimate +
                           PC3_bioclimate +
                           PC4_bioclimate +
                           finch_beak +
                           (1|island),
                         data = length_pop,
                         REML = F)

S_length_m1 <- glmmTMB(S_length ~ PC1_bioclimate +
                        PC2_bioclimate +
                        PC3_bioclimate +
                        PC4_bioclimate +
                        finch_beak +
                        (1|island),
                      data = length_pop,
                      REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_length_m1))
diagnostic(resid(mean_length_m1))

# DHARMa
testResiduals(S_length_m1)
testResiduals(mean_length_m1)

### Results ####
summary(S_length_m1)
summary(mean_length_m1)


Anova(S_length_m1)
Anova(mean_length_m1)


## Longest spine ####
hist(longest_spine_pop$S_longest_spine, breaks = 10)
longest_spine_pop <- na.omit(longest_spine_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_longest_spine_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                            PC2_bioclimate +
                            PC3_bioclimate +
                            PC4_bioclimate +
                            finch_beak +
                            (1|island),
                          data = longest_spine_pop,
                          REML = F)

S_longest_spine_m1 <- glmmTMB(S_longest_spine ~ PC1_bioclimate +
                         PC2_bioclimate +
                         PC3_bioclimate +
                         PC4_bioclimate +
                         finch_beak +
                         (1|island),
                       data = longest_spine_pop,
                       REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_longest_spine_m1))
diagnostic(resid(mean_longest_spine_m1))

# DHARMa
testResiduals(S_longest_spine_m1)
testResiduals(mean_longest_spine_m1)

### Results ####
summary(S_longest_spine_m1)
summary(mean_longest_spine_m1)


Anova(S_longest_spine_m1)
Anova(mean_longest_spine_m1)

## Lower spines ####
hist(lower_spine_pop$S_lower_spine, breaks = 10)
lower_spine_pop <- na.omit(lower_spine_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_lower_spine_m1 <- glmmTMB(freq_all ~ PC1_bioclimate +
                                   PC2_bioclimate +
                                   PC3_bioclimate +
                                   PC4_bioclimate +
                                   finch_beak +
                                   (1|island),
                                 data = lower_spine_pop,
                                 REML = F)

S_lower_spine_m1 <- glmmTMB(S_lower_spine ~ 
                                #PC1_bioclimate +
                                PC2_bioclimate +
                                PC3_bioclimate +
                                PC4_bioclimate +
                                finch_beak +
                                (1|island),
                              data = lower_spine_pop,
                              REML = F)

# We removed PC1 from the model because it would not converge otherwise.

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_lower_spine_m1))
diagnostic(resid(mean_lower_spine_m1))

# DHARMa
testResiduals(S_lower_spine_m1)
testResiduals(mean_lower_spine_m1)

### Results ####
summary(S_lower_spine_m1)
summary(mean_lower_spine_m1)


Anova(S_lower_spine_m1)
Anova(mean_lower_spine_m1)


## Spine position ####
hist(spine_position_pop$S_length, breaks = 10)
spine_position_pop <- na.omit(spine_position_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_spine_position_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                                   PC2_bioclimate +
                                   PC3_bioclimate +
                                   PC4_bioclimate +
                                   finch_beak +
                                   (1|island),
                                 data = spine_position_pop,
                                 REML = F)

S_spine_position_m1 <- glmmTMB(S_length ~ PC1_bioclimate +
                                PC2_bioclimate +
                                PC3_bioclimate +
                                PC4_bioclimate +
                                finch_beak +
                                (1|island),
                              data = spine_position_pop,
                              REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_spine_position_m1))
diagnostic(resid(mean_spine_position_m1))

# DHARMa
testResiduals(S_spine_position_m1)
testResiduals(mean_spine_position_m1)

### Results ####
summary(S_spine_position_m1)
summary(mean_spine_position_m1)


Anova(S_spine_position_m1)
Anova(mean_spine_position_m1)

## Tip distance ####
hist(tip_distance_pop$S_spine_tip_distance, breaks = 10)
tip_distance_pop <- na.omit(tip_distance_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_tip_distance_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                                    PC2_bioclimate +
                                    PC3_bioclimate +
                                    PC4_bioclimate +
                                    finch_beak +
                                    (1|island),
                                  data = tip_distance_pop,
                                  REML = F)

S_tip_distance_m1 <- glmmTMB(S_spine_tip_distance ~ PC1_bioclimate +
                                 PC2_bioclimate +
                                 PC3_bioclimate +
                                 PC4_bioclimate +
                                 finch_beak +
                                 (1|island),
                               data = tip_distance_pop,
                               REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_tip_distance_m1))
diagnostic(resid(mean_tip_distance_m1))

# DHARMa
testResiduals(S_tip_distance_m1)
testResiduals(mean_tip_distance_m1)

### Results ####
summary(S_tip_distance_m1)
summary(mean_tip_distance_m1)


Anova(S_tip_distance_m1)
Anova(mean_tip_distance_m1)

## Width ####
hist(width_pop$S_width, breaks = 10)
width_pop <- na.omit(width_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_width_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                                  PC2_bioclimate +
                                  PC3_bioclimate +
                                  PC4_bioclimate +
                                  finch_beak +
                                  (1|island),
                                data = width_pop,
                                REML = F)

S_width_m1 <- glmmTMB(S_width ~ PC1_bioclimate +
                               PC2_bioclimate +
                               PC3_bioclimate +
                               PC4_bioclimate +
                               finch_beak +
                               (1|island),
                             data = width_pop,
                             REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_width_m1))
diagnostic(resid(mean_width_m1))

# DHARMa
testResiduals(S_width_m1)
testResiduals(mean_width_m1)

### Results ####
summary(S_width_m1)
summary(mean_width_m1)


Anova(S_width_m1)
Anova(mean_width_m1)
