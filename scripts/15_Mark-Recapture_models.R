# Date: 5/1/2023
# Script by: Daniel Reyes
# Question 4 Model analysis
# Goal: Model analysis for question 4 using the mark recapture data.
# I will use the ternary dataset, which has the estimate frequencies of eaten, uneaten, and missing
# mericarps per category and also per island and time of the experiment.
# This is a tentative analysis, perhaps we could use something else, like the PC scores.

# Loading ternary dataset ####
# MR_ternary <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/Island_ternary_dataset.csv")
MR_ternary <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/Island_ternary_filter.csv")
# I am testing now the dataset with estimated days of survival. I am using now,
# survived mericarps per categories and the days they survived which are within each time measured.

# MR_survival <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/MR_survival.csv")
MR_survival_filter <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/MR_life_span_filter.csv")
MR_survival <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/MR_life_span.csv")
# Data preparation ####
# Categories <- factor
# Time <- factor
# Island <- factor

str(MR_ternary)
str(MR_survival_filter)

MR_ternary <- MR_ternary %>% mutate_at(vars(time,
                                              Categories,
                                              Island), list(factor))

MR_survival <- MR_survival %>% mutate_at(vars(island,
                                              treatment,
                                              size,
                                              color,
                                              mark_position,
                                              plate,
                                              Categories), list(factor))

MR_survival_filter <- MR_survival_filter %>% mutate_at(vars(island,
                                              treatment,
                                              size,
                                              color,
                                              mark_position,
                                              plate,
                                              Categories), list(factor))

# The model structure using this dataset would be to predict the eaten, uneaten and missing mericarp frequency
# based on categories, time, and island as a random factor. 
# Since time 0 and 4 are mostly absolute values, I think I would filter them out.

MR_ternary_filter <- filter(MR_ternary, !time == "0")
MR_ternary_filter <- filter(MR_ternary_filter, !time == "4")

# MR_survival_filter <- filter(MR_survival, !Categories == "Large_All_Spines")

# This is not used for the survival data and model

# Models ####

## Survival models ####

Days_survivedQ4 <- glmmTMB(Life_span ~ Categories + color +
                         (1|plate/island),
                       data = MR_survival_filter,
                       REML = F)

## Model diagnostics ####
# Residual histograms
hist(resid(Days_survivedQ4), breaks = 50)

diagnostic(resid(Days_survivedQ4))

# DHARMa
testResiduals(Days_survivedQ4)

### Results ####
summary(Days_survivedQ4)

Anova(Days_survivedQ4)

# ANOVA with afex package aov_ez function
#aov_ez(id = "island", dv = "Days_survived", within = "Categories", na.rm = T, data = MR_survival, fun_aggregate = mean)

EM_Survived <- emmeans::emmeans(Days_survivedQ4, ~ Categories|color, type = "response")

#plot(EM_Survived, comparisons = T)
emmip(Days_survivedQ4, ~ Categories|color, CIs = TRUE)


# ## Eaten mericarps ####
# 
eaten_Q4 <- glmmTMB(n_eaten ~ Categories + time + (1|Island),
                    data = MR_ternary_filter,
                    REML = F,
                    family = "poisson")
# 
# ## Model diagnostics ####
# # Residual histograms
diagnostic(resid(eaten_Q4))
# 
# 
# # DHARMa
testResiduals(eaten_Q4)

# ### Results ####
 summary(eaten_Q4)
# 
 Anova(eaten_Q4)

EM_eatenQ4 <- emmeans(eaten_Q4, ~ Categories|time, type="response") 
#plot(EM_eatenQ4, comparisons = T)
emmip(EM_eatenQ4, ~ Categories, CIs = TRUE)

# ## Uneaten mericarps ####
# 
uneaten_Q4 <- glmmTMB(n_uneaten ~ Categories + time + (1|Island),
                    data = MR_ternary_filter,
                    REML = F,
                    family = "poisson")
# 
# ## Model diagnostics ####
# # Residual histograms
diagnostic(resid(uneaten_Q4))
# 
# 
# # DHARMa
testResiduals(uneaten_Q4)
# 
# 
# ### Results ####
summary(uneaten_Q4)
# 
Anova(uneaten_Q4)

EM_uneatenQ4 <- emmeans(uneaten_Q4, ~ Categories|time, type="response") 
plot(EM_uneatenQ4, comparisons = T)  
emmip(EM_uneatenQ4, ~ Categories|time, CIs = TRUE)
# ## Missing mericarps ####
# 
Missing_Q4 <- glmmTMB(n_missing ~ Categories + time + (1|Island),
                      data = MR_ternary_filter,
                      REML = F,
                      family = "poisson")
# 
# ## Model diagnostics ####
# # Residual histograms
diagnostic(resid(Missing_Q4))
# 
# 
# # DHARMa
testResiduals(Missing_Q4)
# 
# 
# ### Results ####
summary(Missing_Q4)

Anova(Missing_Q4)

EM_missingQ4 <- emmeans(Missing_Q4, ~ Categories|time, type="response") 
#plot(EM_missingQ4, comparisons = T)  
emmip(EM_missingQ4, ~ Categories, CIs = TRUE)
