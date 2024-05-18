# Survival Analysis test ####
# Code by: Daniel Reyes Corral
# 
# Description ####
# Survival analysis using the mark recapture experiment.
# I need to create one with eaten mericarps, one with missing mericarps
# then I also need to create a filter dataset that contains only the four categories
# of 2018 and 2019.
# Also one with only 2018 with all categories.
# I can also do a dataset with all information of all islands and then create
# a single facet plot per island per category.


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

# Checking the column type for each dataset
# Floreana
str(Floreana_2018)
str(Floreana_2019)
# Isabela
str(Isabela_2018)
str(Isabela_2019)
# Santa Cruz
str(Cruz_2018)
str(Cruz_2019)

# Combining datasets per year ####
# Merging both years mericarps

# Merge the datasets per year
Floreana_MR <- bind_rows(Floreana_2018, Floreana_2019)
Isabela_MR <- bind_rows(Isabela_2018, Isabela_2019)
Cruz_MR <- bind_rows(Cruz_2018, Cruz_2019)

#### Variable tranformation into factors ####
# Year, time, island, treatment, size, color, position, etc.
# Transform some columns into factors for the plot for each island

Floreana_MR <- Floreana_MR %>% mutate_at(vars(year,
                                              time,
                                              island,
                                              treatment,
                                              size,
                                              color,
                                              mark_position,
                                              Bird_Survival,
                                              Insect_Survival,
                                              Categories,
                                              plate), list(factor))


Isabela_MR <- Isabela_MR %>% mutate_at(vars(year,
                                            time,
                                            island,
                                            treatment,
                                            size,
                                            color,
                                            mark_position,
                                            Bird_Survival,
                                            Insect_Survival,
                                            Categories,
                                            plate), list(factor))


Cruz_MR <- Cruz_MR %>% mutate_at(vars(year,
                                      time,
                                      island,
                                      treatment,
                                      size,
                                      color,
                                      mark_position,
                                      Bird_Survival,
                                      Insect_Survival,
                                      Categories,
                                      plate), list(factor))


str(Floreana_MR)
str(Isabela_MR)
str(Cruz_MR)
# Transform treatments into factors


#### Creating a new column that groups all tratment combinations ####
#### This column will combine size and spine treatments, is named categories
#### Each island dataset will have the new Categories column

# This will create a new column for the categories
Floreana_MR$Categories <- paste(Floreana_MR$size, Floreana_MR$treatment, sep = "_")
Floreana_MR$Categories <- sub(" ", "_", Floreana_MR$Categories)

Isabela_MR$Categories <- paste(Isabela_MR$size, Isabela_MR$treatment, sep = "_")
Isabela_MR$Categories <- sub(" ", "_", Isabela_MR$Categories)

Cruz_MR$Categories <- paste(Cruz_MR$size, Cruz_MR$treatment, sep = "_")
Cruz_MR$Categories <- sub(" ", "_", Cruz_MR$Categories)

# Selecting columns for model analysis ####

Floreana_survival <- select(Floreana_MR, c(2:10, 18:20, 27))
Isabela_survival <- select(Isabela_MR, c(2:10, 18:20, 26))
Cruz_survival <- select(Cruz_MR, c(2:10, 18:20, 27))

# Combining island datasets into a single dataset for survival analysis
# (this is preliminary)

MR_survival <- bind_rows(Floreana_survival,
                         Isabela_survival,
                         Cruz_survival)
MR_survival <- as_tibble(MR_survival)
# Survival Analysis ####
# Run the Kaplan-Meier estimator with the
# surfit() and Surv() functions:

## KM estimator ####
# This KM estimator is based on the single survival dataset above.
# I will use it to compare between islands

### Time (0,1,2,3,4) and Eaten mericarps
KM_MR <- survfit( Surv(days_pass, Eaten_Birds) ~ size + treatment + island,
                  data = MR_survival)
# Same thing as ~ Categories

summary(KM_MR)

a <- ggsurvplot(KM_MR, legend = "none",
           surv.median.line = "hv",
           # Change legends: title and labels
           #legend.title = "Treatments",
           # legend.labs = c("Large All Spimes",
           #                 "Large Lower Spines",
           #                 "Large No Spines",
           #                 "Large Upper Spines",
           #                 "Small All Spines",
           #                 "Small Lower Spines",
           #                 "Small No Spines",
           #                 "Small Upper Spines"),
           # # Add p-value and tervals
           pval = F,
           conf.int = F,
           # Add risk table
           risk.table = T,
           tables.height = 0.2,
           tables.theme = theme_cleantable(),
           ggtheme = theme_bw()
           )
require("survminer")
a$plot + facet_wrap(~island)

ggsurvplot_group_by(KM_MR, MR_survival, group.by = "island")

# =====
## This estimate is based on time, eaten birds and combined datasets of 2018 and 2019
KM_Isabela_time <- survival::survfit(Surv(time, Eaten_Birds) ~ Categories,
                        data = Isabela_survival)
# Time is monitoring time: 0, 1, 2, 3, 4

KM_Floreana_days <- survival::survfit(Surv(days_pass, Eaten_Birds) ~ Categories,
                                      data = Floreana_survival)

# Days is the days passed from the start until the end of the experiment

KM_Floreana_Present <- survfit(Surv(days_pass, Present) ~ Categories,
                               data = Floreana_survival)
# Present Mericarps assuming that missing were eaten.

KM_Floreana_size <- survfit(Surv(days_pass, Eaten_Birds) ~ size,
                            data = Floreana_survival)

KM_Floreana_year <- survfit(Surv(days_pass, Eaten_Birds) ~ year,
                            data = Floreana_survival)

KM_Cruz_year <- survfit(Surv(time, Present) ~ Categories,
                            data = Cruz_survival)


# Notice that the Surv() function accepts two
# arguments: 1. the time variable; 
# 2. the event variable.
# The ~1 in the survfit() function indicates that we
# estimate the KM without any grouping. But in this case we are grouping by categories
# which are the spine and size treatments
# 
# Then, display the results and draw the Kaplan - Meier plot

## Results ####
summary(KM_Floreana_time)

summary(KM_Floreana_days)
# I think either one shows similar survival probabilities however, I think days is better estimate
# in the sense that much similar than the example of the survival analysis.


summary(KM_Floreana_Present)
# This shows a lower survival probability than just eaten mericarps. I think
# because this was a more common event than having eaten mericarps.

summary(KM_Floreana_size)
# Size has a clear effect larger mericarps have a higher survival prob.

summary(KM_Floreana_year)

# The survival probabilities can be found in the survival column.

## Survival Plot ####
# We can use the ggsurvplot() function within the
# survminer package:

ggsurvplot(KM_Isabela_time, conf.int = F, legend = "right",
           surv.median.line = "hv")

ggsurvplot(KM_Floreana_days, conf.int = F, legend = "right",
           surv.median.line = "hv", title = "Eaten Mericarps Floreana",
           risk.table = T, fun = "cumhaz")

ggsurvplot(KM_Floreana_Present, conf.int = F, legend = "right",
           surv.median.line = "hv", title = "Missing Mericarps Floreana")

ggsurvplot(KM_Floreana_size, conf.int = F, legend = "right",
           surv.median.line = "hv")
# In this case, at the end of the experiment, larger mericarps with all spines
# have a lower survival prob. 

ggsurvplot(KM_Floreana_year, conf.int = F, legend = "right",
           surv.median.line = "hv")
# 2018 is mostly the one that carries all the estimates. However, I think 
# we can still combine the experiments in this case because there is not a lot
# of difference.

plot <- ggsurvplot(KM_Cruz_year, conf.int = F, legend = "right",
           surv.median.line = "hv")

ggsurvplot_facet(KM_Cruz_year, Cruz_survival, facet.by = "color",
                 palette = "jco", pval = T)

# Whereas with eaten mericarps the ones eaten the most are the small ones
# with no spines.


# Crosses are observations


# Suppose the event of interest is death ####
# In this case eaten mericarps.
# - At the time zero, the survival probability is 1 (100% are uneaten)
# - The median indicates that the median survival is 9.
# This is the time where survival (S)t is 50% 
# at which half of the subjects are expected to have died. 
# - From the plot, we also see that the probability of survival more than 5 years
# is 75%. 

# Another example with a larger dataset ####
## Load data ####
data("tongue")
head(tongue)

# Type: Categories
# Time
# Delta: Eaten/Uneaten

# Filter the dataset for category.
anaploid <- subset(tongue, type == 1)

### Results ####
fit <- survfit(Surv(time, delta) ~ type,
               data = tongue,
               conf.type = "log-log")

summary(fit)

ggsurvplot(fit, conf.int = T, surv.median.line = "hv")


# Hypothesis testing of survival data ####
# Comparing survival on one population in the overall population
# Comparison of two or more populations, are there differences in survival
# among different groups of subjects.
# - Examples:
# 2 groups: we are interested in comparing survival for female and male
# 3 groups or more: we are interested in comparing survival for mericarps with
# all spines, no spines, large and small treatments.
#
# The long-rank test for two groups 
