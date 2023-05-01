# Date: 5/1/2023
# Script by: Daniel Reyes
# Question 4 Model analysis
# Goal: Model analysis for question 4 using the mark recapture data.
# I will use the ternary dataset, which has the estimate frequencies of eaten, uneaten, and missing
# mericarps per category and also per island and time of the experiment.
# This is a tentative analysis, perhaps we could use something else, like the PC scores.

# Loading ternary dataset ####
MR_ternary <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Question 4/Island_ternary_dataset.csv")

# Data preparation ####
# Categories <- factor
# Time <- factor
# Island <- factor

str(MR_ternary)

MR_ternary <- MR_ternary %>% mutate_at(vars(time,
                                              Categories,
                                              Island), list(factor))

# The model structure using this dataset would be to predict the eaten, uneaten and missing mericarp frequency
# based on categories, time, and island as a random factor. 
# Since time 0 and 4 are mostly absolute values, I think I would filter them out.

MR_ternary_filter <- filter(MR_ternary, !time == "0")
MR_ternary_filter <- filter(MR_ternary_filter, !time == "4")

# Models ####
## Eaten mericarps ####

eaten_Q4 <- glmmTMB(Eaten_freq ~ Categories + time + (1|Island),
                    data = MR_ternary_filter,
                    REML = F)

## Model diagnostics ####
# Residual histograms
diagnostic(resid(eaten_Q4))


# DHARMa
testResiduals(eaten_Q4)


### Results ####
summary(eaten_Q4)

Anova(eaten_Q4)

## Uneaten mericarps ####

uneaten_Q4 <- glmmTMB(Uneaten_freq ~ Categories + time + (1|Island),
                    data = MR_ternary_filter,
                    REML = F)

## Model diagnostics ####
# Residual histograms
diagnostic(resid(uneaten_Q4))


# DHARMa
testResiduals(uneaten_Q4)


### Results ####
summary(uneaten_Q4)

Anova(uneaten_Q4)

## Missing mericarps ####

Missing_Q4 <- glmmTMB(Missing_freq ~ Categories + time + (1|Island),
                      data = MR_ternary_filter,
                      REML = F)

## Model diagnostics ####
# Residual histograms
diagnostic(resid(Missing_Q4))


# DHARMa
testResiduals(Missing_Q4)


### Results ####
summary(Missing_Q4)

Anova(Missing_Q4)
