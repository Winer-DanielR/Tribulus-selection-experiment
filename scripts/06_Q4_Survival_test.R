# Survival Analysis test ####
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
# Potential analysis ####
# Using a combination of categories and groups to show the results of the experiment
# 
#  
# # 1. The survival analysis can be done per island. Comparing eaten mericarps
# and missing mericarps results.
# 
# 2. Filter categories to compare between years.
# 
# 3. Analysis of only 2018 to compare lower and upper spine treatments.
# 
# 4. Use time in days, rather than time 1, 2 or 3.

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
MR_survival_2018 <- filter(MR_survival_2018, treatment %in% target1)

## Dataset combining 2018 and 2019 experiments ####
# Filter dataset to test 2018 and 2019 categories
target <- c("All spines", "No spines")
MR_survival_filter <- filter(MR_survival, treatment %in% target)

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
                        axis.text = element_text(size = 12, face = "bold"), 
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


## Island analysis ####
## We can use the respective Mark Recapture datasets above per island.

# Survival Analysis ####
# Run the Kaplan-Meier estimator with the
# surfit() and Surv() functions:

## KM estimator ####
# Most of the KM estimates will be done using the
# general survival dataset above.
# I will use it to compare multiple variables.

### KM estimates per island and survival plots ####
### Here I am generating KM estimates and plots per island
### using the All spines and No spines treatments.
#### KM Floreana ####
KM_Floreana_days_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                  data = Floreana_MR)
# Using eaten mericarps as survival estimates

KM_Floreana_days_missing <- survfit(Surv(days_pass, Present) ~ size + treatment,
                                    data = Floreana_MR)

#### Floreana Survival plot ####
ggsurvplot(KM_Floreana_days_missing, legend = "right",
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


ggsurvplot(KM_MR_islands_missing, legend = "right", surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = "Okabe-Ito")


### 1. Compare between Time and Days Passed for eaten mericarps ####

#### Time and Eaten mericarps ####
KM_MR_time_eaten <- survfit( Surv(time, Eaten_Birds) ~ size + treatment + island,
                  data = MR_survival_filter)
# Using 2018 and 2019 categories

#### Days passed and Eaten mericarps ####
KM_MR_days_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment + island,
                            data = MR_survival_filter)

# I think days is better and more intuitive than time.

### 2. Compare Eaten and Missing mericarps ####

KM_MR_days_missing <- survfit(Surv(days_pass, Present) ~ size + treatment + island,
                            data = MR_survival_filter)

# There is some nuance here, but I think it may be worth for discussion.

# So far, it seems that eaten mericarps although there are less events
# it can be more consistent?

### 3. All treatments ####
KM_MR_days_eaten_2018 <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment + island,
                                 data = MR_survival_2018)

### 4. Island KM estimates ####
#### Comparing survival between islands ####
KM_MR_islands_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ island + size,
                         data = MR_survival_filter)

#### Islands missing mericarps ####
KM_MR_islands_missing <- survfit(Surv(days_pass, Present) ~ island,
                         data = MR_survival_filter)
# In general missing mericarps have much lower survival estimates but it seems that
# comparing between islands it stills holds some consistency.

## Compare survival plots ####
#ggsurvplot(KM_MR_time_eaten, legend = "right", surv.median.line = "hv")
ggsurvplot(KM_MR_days_eaten_2018, legend = "right", surv.median.line = "hv")






ggsurvplot_facet(KM_MR_days_eaten_2018, MR_survival_2018, facet.by = "island",
                 surv.median.line = "hv",
                 legend.labs = c("Large - Lower spines", "Large - Upper spines", 
                                 "Small - Lower spines", "Small - Upper spines"),
                 legend.title = "Treatments",
                 legend = "right",
                 xlab = "Time (Days)",
                 ggtheme = theme_minimal(),
                 short.panel.labs = T,
                 panel.labs.font = list(face = "bold"),
                 conf.int = F,
                 palette = "Dark2"
)

# It seems that large upper spines have more survival prob.
# in Santa Cruz.
# This is hard to compare but it may be worth to include.

### Island facet plot ####
### Showing all islands and treatments in a single plot.
ggsurvplot_facet(KM_MR_days_eaten, MR_survival_filter, facet.by = "island", 
                 surv.median.line = "hv", 
                 legend.labs = c("Large - All spines", "Large - No spines", 
                                 "Small - All spines", "Small - No spines"),
                 legend.title = "Treatments",
                 legend = "right",
                 xlab = "Time (Days)",
                 ggtheme = theme_minimal(),
                 short.panel.labs = T,
                 panel.labs.font = list(face = "bold"),
                 conf.int = F,
                 palette = "Dark2"
                 )



# The island plot may be useful
ggsurvplot(KM_MR_islands_missing, legend = "right", surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = "Dark2")


ggsurvplot(KM_MR_islands_eaten, legend = "right", surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = "Dark2")

ggsurvplot_facet(KM_MR_islands_eaten, MR_survival_filter,
                 facet.by = "island",
                 palette = "Dark2")



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
