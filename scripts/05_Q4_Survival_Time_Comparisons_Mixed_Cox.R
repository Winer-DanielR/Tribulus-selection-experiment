# Survival Analysis tests ####
# Comparison between days passed and time
# Using time in days, rather than time 1, 2 or 3.
# Mixed Cox models including plate and color as random effects

# Mark recapture datasets ####
# Loading the Raw mark recapture datasets per year per island.

## Floreana ####
Floreana_2018 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2018.csv")
Floreana_2019 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2019.csv")

## Isabela ####
Isabela_2018 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2018.csv")
Isabela_2019 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2019.csv")

## Santa Cruz ####
Cruz_2018 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2018.csv")
Cruz_2019 <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2019.csv")

# Combining datasets per year ####
# Merging both years mericarps

# Merge the datasets per year
Floreana_MR <- bind_rows(Floreana_2018, Floreana_2019)
Isabela_MR <- bind_rows(Isabela_2018, Isabela_2019)
Cruz_MR <- bind_rows(Cruz_2018, Cruz_2019)

#### Creating a new column that groups all treatment combinations ####
#### This column will combine size and spine treatments, is named categories
#### Each island dataset will have the new Categories column

# This will create a new column for the categories
Floreana_MR$Categories <- paste(Floreana_MR$size, Floreana_MR$treatment, sep = "_")
Floreana_MR$Categories <- sub(" ", "_", Floreana_MR$Categories)

Isabela_MR$Categories <- paste(Isabela_MR$size, Isabela_MR$treatment, sep = "_")
Isabela_MR$Categories <- sub(" ", "_", Isabela_MR$Categories)

Cruz_MR$Categories <- paste(Cruz_MR$size, Cruz_MR$treatment, sep = "_")
Cruz_MR$Categories <- sub(" ", "_", Cruz_MR$Categories)

## Selecting columns for model analysis ####

Floreana_survival <- select(Floreana_MR, c(2:10, 18:21, 27))
Isabela_survival <- select(Isabela_MR, c(2:10, 18:21, 26))
Cruz_survival <- select(Cruz_MR, c(2:10, 18:21, 27))

# Datasets for analysis ####
## Single general dataset ####
# Combining island datasets into a single dataset for survival analysis
# 

MR_survival <- bind_rows(Floreana_survival,
                         Isabela_survival,
                         Cruz_survival)
MR_survival <- as_tibble(MR_survival)

## Dataset for upper an lower spines treatment ####
# Filter survival dataset to test all categories
MR_survival_2018 <- filter(MR_survival, year == "2018")
target1 <- c("Lower spines", "Upper spines")
MR_survival_lower_upper_spines <- filter(MR_survival_2018, treatment %in% target1)

## Dataset combining 2018 and 2019 experiments ####
# Filter dataset to test 2018 and 2019 categories
target <- c("All spines", "No spines")
MR_survival_all_no_spines <- filter(MR_survival, treatment %in% target)

Isabela_MR_all_no_spines <- filter(Isabela_MR, treatment %in% target)
Cruz_MR_all_no_spines <- filter(Cruz_MR, treatment %in% target)

## General comparisons ####
### 1. Time vs Days pass comparison ####
#### Using Eaten mericarps ####
KM_MR_time_eaten <- survfit( Surv(time, Eaten_Birds) ~ size + treatment + island,
                             data = MR_survival_filter)
# Using 2018 and 2019 categories

#### Days passed and Eaten mericarps ####
KM_MR_days_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment + island,
                            data = MR_survival_filter)

#### Survival plots ####
#ggsurvplot(KM_MR_time_eaten, legend = "right", surv.median.line = "hv")
#ggsurvplot(KM_MR_days_eaten, legend = "right", surv.median.line = "hv")

### Island facet plot ####
### Showing all islands and treatments in a single plot.
ggsurvplot_facet(KM_MR_days_eaten, MR_survival_filter, facet.by = "island", 
                 surv.median.line = "hv", 
                 legend.labs = c("Large - All spines", "Large - No spines", 
                                 "Small - All spines", "Small - No spines"),
                 legend.title = "Treatments",
                 legend = "right",
                 xlab = "Time (Days)",
                 short.panel.labs = T,
                 panel.labs.font = list(face = "bold"),
                 conf.int = T,
                 palette = colors,
                 ggtheme = plot_theme
)


#### Comparison conclusion ####
# Between times and days, days have more monitoring points than time.

### 2. Compare Eaten and Missing mericarps ####
KM_MR_days_missing <- survfit(Surv(days_pass, Present) ~ size + treatment + island,
                              data = MR_survival_filter)

#### Survival plots ####
ggsurvplot(KM_MR_days_eaten, legend = "right",
           surv.median.line = "hv")

ggsurvplot(KM_MR_days_missing, legend = "right",
           surv.median.line = "hv")

#### Comparison conclusion ####

# COx mixed models per island ####
## Floreana eaten ####
#### Floreana eaten mericarps mixed cox model ####
# Floreana_cox_eaten_mixed <- coxme(Surv(days_pass, Eaten_Birds) ~ size + treatment 
#                                   + (1|plate) 
#                                   #+ (1|color) 
#                                   + (1|mark_position/color),
#                                   data = Floreana_MR)
# 
# # Call the model
# summary(Floreana_cox_eaten_mixed)
# Anova(Floreana_cox_eaten_mixed)
# 
# #### Emmeans ###
# Floreana_cox_eaten_emmeans <- emmeans::emmeans(Floreana_cox_eaten_mixed, ~ treatment|size, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(Floreana_cox_eaten_emmeans, ~ treatment|size, CIs = TRUE)

## Floreana missing mericarps ####
# Floreana_cox_missing_mixed <- coxme(Surv(days_pass, Present) ~ size + treatment 
#                                   + (1|plate) 
#                                   #+ (1|color) 
#                                   + (1|mark_position/color)
#                                   , data = Floreana_MR)
# 
# # Call the model
# Floreana_cox_missing_mixed
# Anova(Floreana_cox_missing_mixed)
# 
# #### Emmeans ###
# Floreana_cox_missing_emmeans <- emmeans::emmeans(Floreana_cox_missing_mixed, ~ treatment|size, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(Floreana_cox_missing_emmeans, ~ treatment|size, CIs = TRUE)

## Isabela eaten ####
# Isabela_cox_eaten_mixed <- coxme(Surv(days_pass, Eaten_Birds) ~ size + treatment 
#                                  + (1|plate) 
#                                  #+ (1|color) 
#                                  + (1|mark_position/color),
#                                  data = Isabela_MR_filter)
# 
# # Call the model
# summary(Isabela_cox_eaten_mixed)
# Anova(Isabela_cox_eaten_mixed)
# 
# #### Emmeans ###
# Isabela_cox_eaten_emmeans <- emmeans::emmeans(Isabela_cox_eaten_mixed, ~ treatment|size, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(Isabela_cox_eaten_emmeans, ~ treatment|size, CIs = TRUE)

# Isabela_cox_missing_mixed <- coxme(Surv(days_pass, Present) ~ size + treatment 
#                                  + (1|plate) 
#                                  #+ (1|color) 
#                                  + (1|mark_position/color)
#                                  , data = Isabela_MR_filter)
# 
# # Call the model
# Isabela_cox_missing_mixed
# Anova(Isabela_cox_missing_mixed)
# 
# #### Emmeans ###
# Isabela_cox_missing_emmeans <- emmeans::emmeans(Isabela_cox_missing_mixed, ~ treatment|size, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(Isabela_cox_missing_emmeans, ~ treatment|size, CIs = TRUE)

## Santa Cruz eaten ####
## 
# Cruz_cox_eaten_mixed <- coxme(Surv(days_pass, Eaten_Birds) ~ size + treatment 
#                               + (1|plate) 
#                               #+ (1|color) 
#                               + (1|mark_position/color)
#                               , data = Cruz_MR_filter)
# 
# # Call the model
# summary(Cruz_cox_eaten_mixed)
# Anova(Cruz_cox_eaten_mixed)
# 
# #### Emmeans ###
# Cruz_cox_eaten_emmeans <- emmeans::emmeans(Cruz_cox_eaten_mixed, ~ treatment|size, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(Cruz_cox_eaten_emmeans, ~ treatment|size, CIs = TRUE)

## Santa Cruz missing ####
# Cruz_cox_missing_mixed <- coxme(Surv(days_pass, Present) ~ size + treatment 
#                                    + (1|island) 
#                                 #+ (1|color) 
#                                 + (1|mark_position)
#                                 , data = Cruz_MR_filter)
# 
# # Call the model
# Cruz_cox_missing_mixed
# Anova(Cruz_cox_missing_mixed)
# 
# #### Emmeans ###
# Cruz_cox_missing_emmeans <- emmeans::emmeans(Cruz_cox_missing_mixed, ~ treatment|size, type = "response")
# # The emmeans helps me to estimate the projected mean days survived per category.
# # Useful for describing the results. Also it helps me plot the predicted values:
# emmip(Cruz_cox_missing_emmeans, ~ treatment|size, CIs = TRUE)



