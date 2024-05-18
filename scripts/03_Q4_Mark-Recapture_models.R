# Date: 5/1/2023
# Script by: Daniel Reyes
# Question 4 Model analysis
# Goal: Model analysis for question 4 using the mark recapture data.
# I have two datasets. The first one is the ternary dataset, which has the estimate frequencies of eaten, uneaten, and missing
# mericarps per category and also per island and time of the experiment.
# 
# The second dataset is the life span data, where we estimated the life span of survived individual mericarps and follow their
# survival over time. Larger lifespan scores means this individual mericarps survived more. I also have days passed as another response variable
# 

# Dataset loading ####
## Mark recapture two main approaches, the estimated survival days and the relative frequency of eaten, present and missing mericarps

## Loading survival dataset ####
MR_survival <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/MR_life_span.csv")
MR_survival_filter <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/MR_life_span_filter.csv")

# Loading relative frequency dataset ####

MR_frequency <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/Island_ternary_dataset.csv")
# This ternary dataset uses all categories. The dataset combines 2018 and 2019 mericarps
MR_frequency_filter <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/Island_ternary_filter.csv")
# This is the filtered dataset using only four treatments large and small, all spines and no spines.

# Data preparation ####
## Frequency dataset ####
# Categories <- factor
# Time <- factor
# Island <- factor

str(MR_frequency)
str(MR_frequency_filter)

MR_frequency <- MR_frequency %>% mutate_at(vars(time,
                                            Categories,
                                            Island), list(factor))
MR_frequency_filter <- MR_frequency_filter %>% mutate_at(vars(time,
                                            Categories,
                                            Island), list(factor))

# Filter out time 0 and time 4 being the most absolute values of mericarp number.

MR_frequency <- filter(MR_frequency, !time == "0")
MR_frequency <- filter(MR_frequency, !time == "4")

MR_frequency_filter <- filter(MR_frequency_filter, !time == "0")
MR_frequency_filter <- filter(MR_frequency_filter, !time == "4")

## Survival dataset ####
## Categories
## Year = potentially separated analysis
## Island
## Treatment
## Size 
## Color
## Mark Position
## Plate
## Categories
str(MR_survival)
str(MR_survival_filter)

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

# Days Survived is a log normal distribution. Keep that in mind for the models.
hist(MR_survival$Days_survived, breaks = 10)


# Filter out per year. Days survived differ between years, but perhaps tendencies are the 
# same. However, they are two different sets of mericarps.

MR_survival_2018 <- filter(MR_survival, year == "2018")
# This dataset uses all categories from 2018 which are 8
MR_survival_2018_filter <- filter(MR_survival_filter, year == "2018")
# This dataset uses the categories that are reflected in both years which are 4
MR_survival_2019 <- filter(MR_survival, year == "2019")
# 2019 only used 4 categories large and small, all spines and no spines.

# Models ####

## Survival models ####
# I am testing between years I used plate within islands as random factors,
# color is a random factor because the same color is used for multiple categories across islands
# mark position is based on size but is also random, and depends on color too.
# 
# If you incorporate islands as a covariate, then you find out that Mericarps on Santa
# Cruz surive way less than on other islands, even if they are larger compared to Isabela.
# That is super interesting. 

### 2018 all treatments ####

Days_survived_2018 <- glmmTMB(Days_survived ~ Categories + island +
                         (1|plate/island) + (1|color) + (1|mark_position/color),
                       data = MR_survival_2018,
                       family = "gaussian",
                       REML = F)

# When including the interaction variable the model does not converge


#### Model diagnostics ####
# Residual histograms
hist(resid(Days_survived_2018), breaks = 50)

# DHARMa
testResiduals(Days_survived_2018)
# Using years separated actually helps converging the model!

# Model diagnostic function
diagnostic(resid(Days_survived_2018))

#### Results ####
summary(Days_survived_2018)

Anova(Days_survived_2018)

#### Emmeans ####
EM_Days_survived_2018 <- emmeans::emmeans(Days_survived_2018, ~ Categories|island, type = "response")
# The emmeans helps me to estimate the projected mean days survived per category.
# Useful for describing the results. Also it helps me plot the predicted values:
emmip(EM_Days_survived_2018, ~ Categories, CIs = TRUE)


### 2018 filtered treatments ####
# Filtering the treatments would be useful to describe survival across 2018 and 2019.

Days_survived_2018_filter <- glmmTMB(Days_survived ~ Categories + island + island*Categories +
                                (1|plate/island) + (1|color),
                              data = MR_survival_2018_filter,
                              REML = F)

# Model converged after removing spine position and color random effect

#### Model diagnostics ####
# Residual histograms
hist(resid(Days_survived_2018_filter), breaks = 50)

# DHARMa
testResiduals(Days_survived_2018_filter)
# Using years separated actually helps converging the model!

# Model diagnostic function
diagnostic(resid(Days_survived_2018_filter))

#### Results #####
summary(Days_survived_2018_filter)

Anova(Days_survived_2018_filter)

#### Emmeans ####
EM_Days_survived_2018_filter <- emmeans::emmeans(Days_survived_2018_filter, ~ island, type = "response")
# The emmeans helps me to estimate the projected mean days survived per category.
# Useful for describing the results. Also it helps me plot the predicted values:
emmip(EM_Days_survived_2018_filter, ~ island, CIs = TRUE)


### 2018 and 2019 filter treatments ####
# # Using the filtered data to have the same categories
# # Using the datasets of both years detriments the effects altough still significant and still the same trend.
# Days_survived_years <- glmmTMB(Days_survived ~ Categories + island +
#                                 (1|plate/island) + (1|color) + (1|mark_position/color),
#                               data = MR_survival_filter,
#                               REML = F)
# 
#### Model diagnostics ####
# # Residual histograms
# hist(resid(Days_survived_years), breaks = 50)
# 
# # DHARMa
# testResiduals(Days_survived_years)
# # Using years separated actually helps converging the model!
# 
# # Model diagnostic function
# diagnostic(resid(Days_survived_years))
# 
#### Results ####
# summary(Days_survived_years)
# 
# Anova(Days_survived_years)
# 
#### Emmeans ####
# EM_Days_survived_years <- emmeans::emmeans(Days_survived_years, ~ Categories|island, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(EM_Days_survived_years, ~ Categories|island, CIs = TRUE)

## Ternary models ####
## These models uses the number of eaten, uneaten, missing mericarps

### Ternary all categories #####
#### Eaten mericarps ####
# # 
# eaten_Q4 <- glmmTMB(n_eaten ~ Categories + time + Island + Categories*Island,
#                     data = MR_frequency,
#                     REML = F,
#                     family = "poisson")
#  
# # The model dont converge if we include the interaction term
# 
# ##### Model diagnostics ####
# # DHARMa
# testResiduals(eaten_Q4)
# 
# # Residual histograms
# diagnostic(resid(eaten_Q4))
# 
# # ### Results ####
#  summary(eaten_Q4)
# # 
#  Anova(eaten_Q4)
# 
# EM_eatenQ4 <- emmeans(eaten_Q4, ~ Categories|Island, type="response") 
# emmip(EM_eatenQ4, ~ Island, CIs = TRUE)

# #### Missing mericarps ####
# # Check if Island in this model should be random. I don't think so.
# Missing_Q4 <- glmmTMB(n_missing ~ Categories + time + Island + Categories*Island,
#                       data = MR_frequency,
#                       REML = F,
#                       family = "poisson")
#  
# ##### Model diagnostics ####
# # DHARMa
# testResiduals(Missing_Q4)
# 
# # Residual histograms
# diagnostic(resid(Missing_Q4))
# 
# ##### Results ####
# summary(Missing_Q4)
# 
# Anova(Missing_Q4)
# 
# EM_missingQ4 <- emmeans(Missing_Q4, ~ Categories|Island, type="response") 
# emmip(EM_missingQ4, ~ Island, CIs = TRUE)

#### Uneaten mericarps ####
#  Uneaten mericarps are the same as survived mericarps that are being tested in the 
#  first model above.
# uneaten_Q4 <- glmmTMB(n_uneaten ~ Categories + time + Island,
#                     data = MR_frequency,
#                     REML = F,
#                     family = "poisson")
#  
##### Model diagnostics ####
# # DHARMa
# testResiduals(uneaten_Q4)
#  
# # Diagnostic function
# diagnostic(resid(uneaten_Q4))
#  
#  
# ##### Results ####
# summary(uneaten_Q4)
# 
# Anova(uneaten_Q4)
# 
# EM_uneatenQ4 <- emmeans(uneaten_Q4, ~ Categories|Island, type="response") 
# emmip(EM_uneatenQ4, ~ Categories|Island, CIs = TRUE)

### Ternary filtered categories #####
# We use filtered caetgories to match 2018 and 2019 experiments.
#### Eaten mericarps ####

eaten_Q4_filter <- glmmTMB(n_eaten ~ Categories + time + Island + Categories*Island,
                    data = MR_frequency_filter,
                    REML = F,
                    family = "poisson")

##### Model diagnostics ####
# DHARMa
testResiduals(eaten_Q4_filter)

# Residual histograms
diagnostic(resid(eaten_Q4_filter))

# ### Results ####
summary(eaten_Q4_filter)
# 
Anova(eaten_Q4_filter)

EM_eatenQ4_filter <- emmeans(eaten_Q4_filter, ~ Island, type="response") 
emmip(eaten_Q4_filter, ~ Island, CIs = TRUE)

#### Missing mericarps ####
# Check if Island in this model should be random. I don't think so.
Missing_Q4 <- glmmTMB(n_missing ~ Categories + time + Island,
                      data = MR_frequency,
                      REML = F,
                      family = "poisson")

##### Model diagnostics ####
# DHARMa
testResiduals(Missing_Q4)

# Residual histograms
diagnostic(resid(Missing_Q4))

##### Results ####
summary(Missing_Q4)

Anova(Missing_Q4)

EM_missingQ4 <- emmeans(Missing_Q4, ~ Categories|Island, type="response") 
emmip(EM_missingQ4, ~ Island, CIs = TRUE)