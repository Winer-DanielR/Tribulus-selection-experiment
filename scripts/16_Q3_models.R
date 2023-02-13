# Data loading ####
## Populations ####
depth_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/depth_mean_year.csv")
length_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/length_mean_year.csv")
longest_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/longest_spine_mean_year.csv")
tip_distance_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/tip_distance_mean_year.csv")
width_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/width_mean_year.csv")
lower_spine_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/lower_spines_year.csv")
spine_position_pop <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time means per year/spine_position_year.csv")

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
spine_position_pop <- spine_position_pop %>% mutate_at(vars(island, population, spine_position, finch_beak), list(factor))
tip_distance_pop <- tip_distance_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))
width_pop <- width_pop %>% mutate_at(vars(island, population, finch_beak), list(factor))

### PCA populations ####
pca_means_Q3 <- pca_means_Q3 %>% mutate_at(vars(island, population, finch_beak), list(factor))
str(pca_means_Q3)
pca_means_Q3 <- na.omit(pca_means_Q3)


# Models ####
# The structure of this model is to test the effect of mean trait values
# and selection estimates.
## Size ####
# sizeQ3 <- glmmTMB(S_Size ~ Bio_1 +
#                     Bio_4 +
#                     Bio_12 +
#                     Bio_15 +
#                     (1|island/population),
#                   data = pca_means_Q3,
#                   REML = F) 

sizeQ3 <- glmmTMB(S_Size ~ PC1_bioclimate +
                    PC2_bioclimate +
                    PC3_bioclimate +
                    PC4_bioclimate +
                    finch_beak +
                    (1|island/population),
                  data = pca_means_Q3,
                  REML = F) 

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(sizeQ3))
# DHARMa
testResiduals(sizeQ3)

### Results ####
summary(sizeQ3)
Anova(sizeQ3, type = "3")

## Defense ####
# DefenseQ3 <- glmmTMB(S_Defense ~ Bio_1 +
#                     #Bio_4 + # Temperature seasonality removed
#                     Bio_12 +
#                     Bio_15 +
#                     (1|island/population),
#                   data = pca_means_Q3,
#                   REML = F) 

DefenseQ3 <- glmmTMB(S_Defense ~ PC1_bioclimate +
                       PC2_bioclimate +
                       PC3_bioclimate +
                       PC4_bioclimate +
                       finch_beak +
                       (1|island/population),
                     data = pca_means_Q3,
                     REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(DefenseQ3))
# DHARMa
testResiduals(DefenseQ3)

### Results ####
summary(DefenseQ3)
Anova(DefenseQ3, type = "3")

## Position ####
# PositionQ3 <- glmmTMB(S_Position ~ Bio_1 +
#                     Bio_4 +
#                     #Bio_12 + # Annual precipitation removed
#                     Bio_15 +
#                     (1|island/population),
#                   data = pca_means_Q3,
#                   REML = F) 

PositionQ3 <- glmmTMB(S_Position ~ PC1_bioclimate +
                        PC2_bioclimate +
                        PC3_bioclimate +
                        PC4_bioclimate +
                        finch_beak +
                        (1|island/population),
                      data = pca_means_Q3,
                      REML = F)

 
### Model Diagnostics ####
# Residual histograms
diagnostic(resid(PositionQ3))
# DHARMa
testResiduals(PositionQ3)

### Results ####
summary(PositionQ3)
Anova(PositionQ3, type = "3")

## Depth ####
hist(depth_pop$S_depth, breaks = 10)
depth_pop <- na.omit(depth_pop)

depth_m1 <- glmmTMB(S_depth ~ 
                        Bio_1 + 
                        #Bio_4 + 
                        #Bio_12 + 
                        Bio_15 +
                      (1|island/population),
                    data = depth_pop,
                    REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(depth_m1))
# DHARMa
testResiduals(depth_m1)

### Results ####
summary(depth_m1)
Anova(depth_m1)

# For depth, two biolcimate variables did not converged

## Length ####
length_m1 <- glmmTMB(S_length ~ 
                      Bio_1 + 
                      Bio_4 + 
                      Bio_12 + 
                      Bio_15 +
                      (1|island/population),
                    data = length_pop,
                    REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(length_m1))
# DHARMa
testResiduals(length_m1)

### Results ####
summary(length_m1)
Anova(length_m1)

## Longest spine ####
longest_spine_m1 <- glmmTMB(S_longest_spine ~ 
                       Bio_1 + 
                       Bio_4 + 
                       Bio_12 + 
                       Bio_15 +
                       (1|island/population),
                     data = longest_spine_pop,
                     REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(longest_spine_m1))
# DHARMa
testResiduals(longest_spine_m1)

### Results ####
summary(longest_spine_m1)
Anova(longest_spine_m1)

## Lower spines ####
lower_spine_m1 <- glmmTMB(S_lower_spine ~ 
                              #Bio_1 + 
                              #Bio_4 + 
                              #Bio_12 + 
                              Bio_15 +
                              (1|island/population),
                            data = lower_spine_pop,
                            REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(lower_spine_m1))
# DHARMa
testResiduals(lower_spine_m1)

### Results ####
summary(lower_spine_m1)
Anova(lower_spine_m1)

## Spine position ####
spine_position_m1 <- glmmTMB(S_spine_position ~ 
                            Bio_1 + 
                            Bio_4 + 
                            #Bio_12 + 
                            Bio_15 +
                            (1|island/population),
                          data = spine_position_pop,
                          REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(spine_position_m1))
# DHARMa
testResiduals(spine_position_m1)

### Results ####
summary(spine_position_m1)
Anova(spine_position_m1)

## Tip distance ####
tip_distance_m1 <- glmmTMB(S_spine_tip_distance ~ 
                               #Bio_1 + 
                               Bio_4 + 
                               Bio_12 + 
                               Bio_15 +
                               (1|island/population),
                             data = tip_distance_pop,
                             REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(tip_distance_m1))
# DHARMa
testResiduals(tip_distance_m1)

### Results ####
summary(tip_distance_m1)
Anova(tip_distance_m1)

## Tip distance ####
width_pop <- na.omit(width_pop)
width_m1 <- glmmTMB(S_width ~ 
                             Bio_1 + 
                             #Bio_4 + 
                             Bio_12 + 
                             Bio_15 +
                             (1|island/population),
                           data = width_pop,
                           REML = F)

### Model Diagnostics ####
# Residual histograms
diagnostic(resid(width_m1))
# DHARMa
testResiduals(width_m1)

### Results ####
summary(width_m1)
Anova(width_m1)
