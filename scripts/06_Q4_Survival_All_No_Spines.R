# Survival Analysis test ####
# Eaten and Missing mericarps KM estimates
# and Cox models

# Code by: Daniel Reyes Corral
# Date: May 2024
# 
# Description ####
# Survival analysis using the mark recapture experiment. Using eaten or missing
# mericarps as the survival events and days as the time of the experiment
# The experiment had various categories of size and spine treatments for mericarps
# and the survival of the individual mericarps were tracked over a year in 2018 and over
# 3 months in 2019.
# 
#  
# # 1. The survival analysis can be done per island. Comparing eaten mericarps
# and missing mericarps results.
# 

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
Floreana_MR$Missing <- ifelse(Floreana_MR$Present == 0, 1, 0)


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
Isabela_MR_all_no_spines$Missing <- ifelse(Isabela_MR_all_no_spines$Present == 0, 1, 0)


Cruz_MR_all_no_spines <- filter(Cruz_MR, treatment %in% target)
Cruz_MR_all_no_spines$Missing <- ifelse(Cruz_MR_all_no_spines$Present == 0, 1, 0)

## Plot preparation ####
## Color scales and plot themes
colors <- c("#0072B2",
            "#009E73",
            "#D55E00",
            "#CC79A7",
            "#F0E442",
            "#000000",
            "#56B4E9",
            "#E69F00",
            "#999999"
            )

## Plot theme
plot_theme <-     theme(axis.line = element_line(linetype = "solid", size = 1), 
                        axis.title = element_text(size = 14, 
                                                  face = "bold"
                        ),
                        axis.text = element_text(size = 12), 
                        axis.text.x = element_text(size = 12), 
                        plot.title = element_text(size = 16, face = "bold", hjust = 0),
                        text = element_text(family = "Noto Sans"),
                        legend.text = element_text(size = 11), 
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.position = "right",
                        panel.background = element_rect(fill = NA),
                        legend.background = element_rect(fill = NA, size = 0),
                        strip.text = element_text(size = 10, face = "bold"),
                        strip.background = element_blank(),
                        panel.spacing = unit(1, "cm"))


# Individual island analysis ####
## We can use the respective Mark Recapture datasets above per island.
 
## Survival Analysis ####
# Run the Kaplan-Meier estimator with the
# surfit() and Surv() functions:

## KM estimator ####
# Most of the KM estimates will be done using the
# general survival dataset above.
# I will use it to compare multiple variables.

## KM estimates per island, survival plots and Cox models ####
### Here I am generating KM estimates and plots per island
### using the All spines and No spines treatments.
#### KM Floreana eaten ####
KM_Floreana_days_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                  data = Floreana_MR)

Floreana_eaten_diff <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                data = Floreana_MR)
#### Floreana eaten mericarps Cox model ####
# The Cox hazards model is a regression model. 
# Commonly used to investigate the association between
# the time to an event and a set of explanatory variables.
# 
# Cox proportional hazards regression can be performed using the function
# coxph() and finalfit(). The latter produces a table containing counts for factors,
# mean, SD for continuous variable and univariable and multivariable CPH regression.
# 
# The output of the function shows the number of mericarps and 
# the number of events, either eaten or missing. The coefficient can be
# exponentiated and interpreted as a hazard ratio exp(coef).


Floreana_cox_eaten <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment 
                            , data = Floreana_MR)

summary(Floreana_cox_eaten)

Anova(Floreana_cox_eaten)

Floreana_cox_emmeans <- emmeans::emmeans(Floreana_cox_eaten, ~ treatment|size, type = "response")
emmip(Floreana_cox_emmeans, ~ treatment|size, CIs = TRUE)


# Based on the emmeans in Floreana being smaller and without spines
# has the highest risk of being eaten. While being larger with spines provides
# better chances of survival.


#### Survival plot Floreana eaten ####
ggsurvplot(KM_Floreana_days_eaten, legend = "right",
           surv.median.line = "hv",
           pval = F,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - No spines",
                           "Small - All spines",
                           "Small - No spines"),
           title = "Floreana",
           ggtheme = plot_theme)

### KM Floreana missing ####
#### Using missing mericarps as survival estitames due to the low number of events for eaten mericarps

KM_Floreana_days_missing <- survfit(Surv(days_pass, Missing) ~ size + treatment,
                                    data = Floreana_MR)

#### Floreana Cox Missing ####
Floreana_cox_missing <- coxph(Surv(days_pass, Missing) ~ size + treatment
                              , data = Floreana_MR)

summary(Floreana_cox_missing)
(Anova(Floreana_cox_missing))

Floreana_cox_emmeans_miss <- emmeans::emmeans(Floreana_cox_missing, ~ treatment|size, type = "response")
emmip(Floreana_cox_emmeans_miss, ~ treatment|size, CIs = TRUE)


# Missing mericarps in Floreana show similar hazard ratios.
# which means that mericarps get lost around the same.

#### Survival plot Floreana Missing ####
Floreana_miss <- ggsurvplot(KM_Floreana_days_missing, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - No spines",
                           "Small - All spines",
                           "Small - No spines"),
           title = "Floreana",
           ggtheme = plot_theme)


#### Floreana finalfit function ####
#### Finalfit() function
dependent_eaten <- "Surv(days_pass, Eaten_Birds)"
dependent_missing <- "Surv(days_pass, Present)"
explanatory <- c("Categories", "mark_position")

Floreana_eaten_fit <- Floreana_MR %>% finalfit(dependent_eaten, explanatory)
Floreana_missing_fit <- Floreana_MR %>% finalfit(dependent_missing, explanatory)

# I think this shows specifically which categories are significant, even position marks.

# The output shows the hazard ratio exp(coef). with confidence intervals.

#### KM Isabela eaten####
KM_Isabela_days_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                  data = Isabela_MR_all_no_spines)

Isabela_eaten_diff <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                               data = Isabela_MR_all_no_spines)

#### Isabela Cox Eaten ####
Isabela_cox_eaten <- coxph(Surv(days_pass, Eaten_Birds) ~ treatment + size,
                           data = Isabela_MR_all_no_spines)

summary(Isabela_cox_eaten)
Anova(Isabela_cox_eaten)

Isabela_cox_emmeans <- emmeans::emmeans(Isabela_cox_eaten, ~ treatment|size, type = "response")
emmip(Isabela_cox_emmeans, ~ treatment|size, CIs = TRUE)

---
# The results of eaten mericarps in Isabela are not significant.
# However, the estimated hazard ratios show that smaller mericarps without
# spines have a higher risk of being eaten. Also larger mericarps without spines.
# Here in Isabela it seems that the spine presence overall improves survival.
---

#### Survival plot Isabela eaten ####
ggsurvplot(KM_Isabela_days_eaten, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - No spines",
                           "Small - All spines",
                           "Small - No spines"),
           title = "Isabela",
           ggtheme = plot_theme)

### KM Isabela missing mericarps ####
KM_Isabela_days_missing <- survfit(Surv(days_pass, Missing) ~ size + treatment,
                                    data = Isabela_MR_all_no_spines)

##### Isabela Cox Missing ####
Isabela_cox_missing <- coxph(Surv(days_pass, Missing) ~ size + treatment, data = Isabela_MR_all_no_spines)

summary(Isabela_cox_missing)
Anova(Isabela_cox_missing)

Isabela_cox_emmeans_miss <- emmeans::emmeans(Isabela_cox_missing, ~ treatment|size, type = "response")
emmip(Isabela_cox_emmeans_miss, ~ treatment|size, CIs = TRUE)


# Similar to Floreana, it seems that in Isabela mericarps have a similar
# risk of being missing.


#### Survival plot Isabela missing ####
Isabela_miss <- ggsurvplot(KM_Isabela_days_missing, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - No spines",
                           "Small - All spines",
                           "Small - No spines"),
           title = "Isabela",
           ggtheme = plot_theme)


####  Isabela Finalfit() function ####
dependent_eaten <- "Surv(days_pass, Eaten_Birds)"
dependent_missing <- "Surv(days_pass, Present)"
explanatory <- c("Categories", "mark_position")

Isabela_eaten_fit <- Isabela_MR_filter %>% finalfit(dependent_eaten, explanatory)
Isabela_missing_fit <- Isabela_MR_filter %>% finalfit(dependent_missing, explanatory)

# Eaten is not significant and missing mericarps are significant to mark position as well

#### KM Santa Cruz eaten ####
KM_Cruz_days_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                 data = Cruz_MR_all_no_spines)

Cruz_eaten_diff <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                              data = Cruz_MR_all_no_spines)

##### Cruz Cox Eaten ####
Cruz_cox_eaten <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment 
                        # + mark_position
                        , data = Cruz_MR_all_no_spines)

summary(Cruz_cox_eaten)
Anova(Cruz_cox_eaten)

Cruz_cox_emmeans <- emmeans::emmeans(Cruz_cox_eaten, ~ treatment|size, type = "response")
emmip(Cruz_cox_emmeans, ~ treatment|size, CIs = TRUE)

# In Santa Cruz, were he observed the highest predation.
# Again, smaller mericarps show the highest risk of being eaten,
# however, it is the ones with spines that show the highest risks
# This is also true for larger mericarps.



#### Survival plot Santa Cruz eaten ####
ggsurvplot(KM_Cruz_days_eaten, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - No spines",
                           "Small - All spines",
                           "Small - No spines"),
           title = "Santa Cruz",
           ggtheme = plot_theme)


#### KM Santa Cruz missing ####
KM_Cruz_days_missing <- survfit(Surv(days_pass, Missing) ~ size + treatment,
                                   data = Cruz_MR_all_no_spines)

##### Cruz Cox Missing ####
Cruz_cox_missing <- coxph(Surv(days_pass, Missing) ~ size + treatment
                              , data = Cruz_MR_all_no_spines)

summary(Cruz_cox_missing)
Anova(Cruz_cox_missing)

Cruz_cox_emmeans_miss <- emmeans::emmeans(Cruz_cox_missing, ~ treatment|size, type = "response")
emmip(Cruz_cox_emmeans_miss, ~ treatment|size, CIs = TRUE)

# Santa Cruz missing mericarps had the same chance of getting lost

#### Survival plot Santa Cruz missing ####
Santa_Cruz_miss <- ggsurvplot(KM_Cruz_days_missing, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - No spines",
                           "Small - All spines",
                           "Small - No spines"),
           title = "Santa Cruz",
           ggtheme = plot_theme)


#### Finalfit() function
dependent_eaten <- "Surv(days_pass, Eaten_Birds)"
dependent_missing <- "Surv(days_pass, Present)"
explanatory <- c("Categories", "mark_position")

Cruz_eaten_fit <- Cruz_MR_filter %>% finalfit(dependent_eaten, explanatory)
Cruz_missing_fit <- Cruz_MR_filter %>% finalfit(dependent_missing, explanatory)

# Island comparisons ####
### Island KM estimates ####
#### Comparing survival between islands ####
# Survival_filter is the All spine and No spines comparisons

KM_MR_islands_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ island
                               , data = MR_survival_all_no_spines)

#### Islands missing mericarps ####
KM_MR_islands_missing <- survfit(Surv(days_pass, Present) ~ island,
                                 data = MR_survival_all_no_spines)

# This KM summary could be included into the main text

# In general missing mericarps have much lower survival estimates but it seems that
# comparing between islands it stills holds some consistency.


#### Island survival plot ####
#### This is comparing general mericarp survival, between islands.
#### I can compare mericarp size perhaps.

ggsurvplot(KM_MR_islands_eaten, surv.median.line = "hv",
           ggtheme = plot_theme,
           palette = colors,
           conf.int = T,
           pval = T)

### Island facet plot eaten ####
### Showing all islands and treatments in a single plot.
ggsurvplot_facet(KM_MR_islands_eaten, MR_survival_all_no_spines, facet.by = "island", 
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

### Island facet plot missing ####
ggsurvplot_facet(KM_MR_islands_missing, MR_survival_lower_upper_spines, facet.by = "island", 
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




# General Survival observations ####
# Notice that the Surv() function accepts two
# arguments: 1. the time variable; 
# 2. the event variable.
# The ~1 in the survfit() function indicates that we
# estimate the KM without any grouping. But in this case we are grouping by categories
# which are the spine and size treatments
# 
# Then, display the results and draw the Kaplan - Meier plot

## Death ####
# Suppose the event of interest is death
# In this case eaten mericarps.
# - At the time zero, the survival probability is 1 (100% are uneaten)
# - The median indicates that the median survival (S)t is 50% 
# at which half of the subjects are expected to have died. 


