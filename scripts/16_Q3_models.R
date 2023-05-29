# Data loading ####
## Populations ####
depth_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/depth_population.csv")
length_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/length_population.csv")
longest_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/longest_spine_population.csv")
tip_distance_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/tip_distance_population.csv")
width_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/width_population.csv")
lower_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/lower_spines_pop.csv")
spine_position_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Questions_1-3_trait_datasets/spine_position_pop.csv")

## PCA Populations ####
pca_means_Q3 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_populations_bioclimate.csv")


# Data preparation ####
# Making char into factors (island, populations)
# 
### Populations #####
depth_pop <- depth_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
length_pop <- length_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
longest_spine_pop  <- longest_spine_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
lower_spine_pop <- lower_spine_pop %>% mutate_at(vars(island, population, lower_spine, finch_beak), list(factor))
spine_position_pop <- spine_position_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
tip_distance_pop <- tip_distance_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
width_pop <- width_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))

### PCA populations ####
pca_means_Q3 <- pca_means_Q3 %>% mutate_at(vars(island, population, finch_beak), list(factor))
str(pca_means_Q3)
pca_means_Q3 <- na.omit(pca_means_Q3)


# Models ####
## Size ####
# The structure of this model is to test the effect of mean trait values
# The structure of the model is based on the question that bioclimate variables are
# predictors of size trait and selection

S_sizeQ3 <- glmmTMB(S_Size ~ PC1_bioclimate +
                    PC2_bioclimate +
                    PC3_bioclimate +
                    PC4_bioclimate +
                    finch_beak +
                    (1|island/population),
                  data = pca_means_Q3,
                  REML = F) 

Size_meanQ3 <- glmmTMB(Size_mean ~ PC1_bioclimate +
                      PC2_bioclimate +
                      PC3_bioclimate +
                      PC4_bioclimate +
                      finch_beak +
                      (1|island/population),
                    data = pca_means_Q3,
                    REML = F) 

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_sizeQ3))
diagnostic(resid(Size_meanQ3))
# DHARMa
testResiduals(S_sizeQ3)
testResiduals(Size_meanQ3)

### Results ####
summary(S_sizeQ3)
Anova(S_sizeQ3, type = "3")

summary(Size_meanQ3)
Anova(Size_meanQ3, type = "3")

r.squaredGLMM(S_sizeQ3)
r.squaredGLMM(Size_meanQ3)

## Defense ####
S_DefenseQ3 <- glmmTMB(S_Defense ~ PC1_bioclimate +
                       PC2_bioclimate +
                       PC3_bioclimate +
                       PC4_bioclimate +
                       finch_beak +
                       (1|island/population),
                     data = pca_means_Q3,
                     REML = F)

Mean_DefenseQ3 <- glmmTMB(Defense_mean ~ PC1_bioclimate +
                         PC2_bioclimate +
                         PC3_bioclimate +
                         PC4_bioclimate +
                         finch_beak +
                         (1|island/population),
                       data = pca_means_Q3,
                       REML = F)



### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_DefenseQ3))
diagnostic(resid(Mean_DefenseQ3))

# DHARMa
testResiduals(S_DefenseQ3)
testResiduals(Mean_DefenseQ3)


### Results ####
summary(S_DefenseQ3)
summary(Mean_DefenseQ3)

Anova(S_DefenseQ3, type = "3")
Anova(Mean_DefenseQ3, type = "3")

r.squaredGLMM(S_DefenseQ3)
r.squaredGLMM(Mean_DefenseQ3)

## Position ####
S_PositionQ3 <- glmmTMB(S_Position ~ PC1_bioclimate +
                        PC2_bioclimate +
                        PC3_bioclimate +
                        PC4_bioclimate +
                        finch_beak +
                        (1|island/population),
                      data = pca_means_Q3,
                      REML = F)

Mean_PositionQ3 <- glmmTMB(Position_mean ~ PC1_bioclimate +
                          PC2_bioclimate +
                          PC3_bioclimate +
                          PC4_bioclimate +
                          finch_beak +
                          (1|island/population),
                        data = pca_means_Q3,
                        REML = F)

 
### Model Diagnostics ####
# Residual histograms
diagnostic(resid(S_PositionQ3))
diagnostic(resid(Mean_PositionQ3))

# DHARMa
testResiduals(S_PositionQ3)
testResiduals(Mean_PositionQ3)

### Results ####
summary(S_PositionQ3)
summary(Mean_PositionQ3)


Anova(S_PositionQ3, type = "3")
Anova(Mean_PositionQ3, type = "3")

r.squaredGLMM(S_PositionQ3)
r.squaredGLMM(Mean_PositionQ3)

## Depth ####
hist(depth_pop$S_depth, breaks = 10)
depth_pop <- na.omit(depth_pop) # It removes all the islands with NAs for the PC bioclimate scores

mean_depth_m1 <- glmmTMB(mean_all ~ PC1_bioclimate +
                        PC2_bioclimate +
                        PC3_bioclimate +
                        PC4_bioclimate +
                        finch_beak +
                        (1|island/population),
                      data = depth_pop,
                      REML = F)

S_depth_m1 <- glmmTMB(S_depth ~ PC1_bioclimate +
                           PC2_bioclimate +
                           PC3_bioclimate +
                           PC4_bioclimate +
                           finch_beak +
                           (1|island/population),
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
                           (1|island/population),
                         data = length_pop,
                         REML = F)

S_length_m1 <- glmmTMB(S_length ~ PC1_bioclimate +
                        PC2_bioclimate +
                        PC3_bioclimate +
                        PC4_bioclimate +
                        finch_beak +
                        (1|island/population),
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
                            (1|island/population),
                          data = longest_spine_pop,
                          REML = F)

S_longest_spine_m1 <- glmmTMB(S_longest_spine ~ PC1_bioclimate +
                         PC2_bioclimate +
                         PC3_bioclimate +
                         PC4_bioclimate +
                         finch_beak +
                         (1|island/population),
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
                                   (1|island/population),
                                 data = lower_spine_pop,
                                 REML = F)

S_lower_spine_m1 <- glmmTMB(S_lower_spine ~ 
                                #PC1_bioclimate +
                                PC2_bioclimate +
                                PC3_bioclimate +
                                PC4_bioclimate +
                                finch_beak +
                                (1|island/population),
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
                                   (1|island/population),
                                 data = spine_position_pop,
                                 REML = F)

S_spine_position_m1 <- glmmTMB(S_length ~ PC1_bioclimate +
                                PC2_bioclimate +
                                PC3_bioclimate +
                                PC4_bioclimate +
                                finch_beak +
                                (1|island/population),
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
                                    (1|island/population),
                                  data = tip_distance_pop,
                                  REML = F)

S_tip_distance_m1 <- glmmTMB(S_spine_tip_distance ~ PC1_bioclimate +
                                 PC2_bioclimate +
                                 PC3_bioclimate +
                                 PC4_bioclimate +
                                 finch_beak +
                                 (1|island/population),
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
                                  (1|island/population),
                                data = width_pop,
                                REML = F)

S_width_m1 <- glmmTMB(S_width ~ PC1_bioclimate +
                               PC2_bioclimate +
                               PC3_bioclimate +
                               PC4_bioclimate +
                               finch_beak +
                               (1|island/population),
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
