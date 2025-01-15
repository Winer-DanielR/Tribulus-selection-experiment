# Survival Analysis tests ####
# Analysis of only 2018 to compare lower and upper spine treatments.
# Using eaten and missing mericarps
# and simple Cox models

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

## Dataset for upper an lower spines treatment ####
# Filter survival dataset to test all categories
MR_survival_2018 <- filter(MR_survival, year == "2018")
target1 <- c("Lower spines", "Upper spines")
MR_survival_lower_upper_spines <- filter(MR_survival_2018, treatment %in% target1)

MR_lower_upper_Isabela <- filter(MR_survival_lower_upper_spines, island == "Isabela")
MR_lower_upper_Cruz <- filter(MR_survival_lower_upper_spines, island == "Santa Cruz")

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

#### KM Isabela eaten####
KM_Isabela_lower_upper <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                 data = MR_lower_upper_Isabela)

Isabela_diff_lower_upper <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                               data = MR_lower_upper_Isabela)

#### Isabela Cox Eaten ####
Isabela_cox_lower_upper <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                           data = MR_lower_upper_Isabela)

summary(Isabela_cox_lower_upper)
Anova(Isabela_cox_lower_upper)

# Size is significant difference in survival

#### Survival plot Isabela eaten ####
ggsurvplot(KM_Isabela_lower_upper, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - Lower spines",
                           "Large - Upper spines",
                           "Small - Lower spines",
                           "Small - Upper spines"),
           title = "Isabela",
           ggtheme = plot_theme)

#### KM Isabela missing####
KM_Isabela_lower_upper_miss <- survfit(Surv(days_pass, Present) ~ size + treatment,
                                  data = MR_lower_upper_Isabela)

Isabela_diff_lower_upper_miss <- survdiff(Surv(days_pass, Present) ~ size + treatment,
                                      data = MR_lower_upper_Isabela)

#### Isabela Cox Eaten ####
Isabela_cox_lower_upper_miss <- coxph(Surv(days_pass, Present) ~ size + treatment,
                                 data = MR_lower_upper_Isabela)

summary(Isabela_cox_lower_upper_miss)
Anova(Isabela_cox_lower_upper_miss)

# Size is significant difference in survival

#### Survival plot Isabela missing ####
ggsurvplot(KM_Isabela_lower_upper_miss, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - Lower spines",
                           "Large - Upper spines",
                           "Small - Lower spines",
                           "Small - Upper spines"),
           title = "Isabela",
           ggtheme = plot_theme)



#### KM Santa Cruz eaten ####
KM_Cruz_lower_upper <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                              data = MR_lower_upper_Cruz)

Cruz_diff_lower_upper <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                            data = MR_lower_upper_Cruz)

##### Cruz Cox Eaten ####
Cruz_cox_lower_upper <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment 
                        # + mark_position
                        , data = MR_lower_upper_Cruz)

summary(Cruz_cox_lower_upper)
Anova(Cruz_cox_lower_upper)

#### Survival plot Santa Cruz eaten ####
ggsurvplot(KM_Cruz_lower_upper, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - Lower spines",
                           "Large - Upper spines",
                           "Small - Lower spines",
                           "Small - Upper spines"),
           title = "Santa Cruz",
           ggtheme = plot_theme)


#### KM Santa Cruz missing ####
KM_Cruz_lower_upper_miss <- survfit(Surv(days_pass, Present) ~ size + treatment,
                               data = MR_lower_upper_Cruz)

Cruz_diff_lower_upper_miss <- survdiff(Surv(days_pass, Present) ~ size + treatment,
                                  data = MR_lower_upper_Cruz)

##### Cruz Cox Eaten ####
Cruz_cox_lower_upper_miss <- coxph(Surv(days_pass, Present) ~ size + treatment 
                              # + mark_position
                              , data = MR_lower_upper_Cruz)

summary(Cruz_cox_lower_upper_miss)
Anova(Cruz_cox_lower_upper_miss)

#### Survival plot Santa Cruz missing ####
ggsurvplot(KM_Cruz_lower_upper_miss, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - Lower spines",
                           "Large - Upper spines",
                           "Small - Lower spines",
                           "Small - Upper spines"),
           title = "Santa Cruz",
           ggtheme = plot_theme)
