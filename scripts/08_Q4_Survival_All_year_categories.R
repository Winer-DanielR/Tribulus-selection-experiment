# Survival Analysis tests ####
# Analysis of only 2018 to compare all treatments per island.
# This way we wont be using 2019 but I think we can make a better 
# sense of the results this way.
# 
# 2. I can also compare with 2019 and 2018 together.


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
Floreana_MR$Missing <- ifelse(Floreana_MR$Present == 0, 1, 0)

Isabela_MR$Categories <- paste(Isabela_MR$size, Isabela_MR$treatment, sep = "_")
Isabela_MR$Categories <- sub(" ", "_", Isabela_MR$Categories)

Cruz_MR$Categories <- paste(Cruz_MR$size, Cruz_MR$treatment, sep = "_")
Cruz_MR$Categories <- sub(" ", "_", Cruz_MR$Categories)

## Selecting columns for model analysis ####

Floreana_survival <- select(Floreana_MR, c(2:10, 18:21, 27))
Isabela_survival <- select(Isabela_MR, c(2:10, 18:21, 26))
Cruz_survival <- select(Cruz_MR, c(2:10, 18:21, 27))

Isabela_survival <- filter(Isabela_survival, year == "2018")
Cruz_survival <- filter(Cruz_survival, year == "2018")

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

# So, Island_survival datasets includes 2018 and 2019 data with all the categories included
# Let's test a model that includes all treatments

# Floreana ####
## Eaten mericarps ####

KM_Floreana_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                  data = Floreana_survival)

Floreana_surv_diff <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                     data = Floreana_survival)

#### Floreana Cox Eaten ####
Floreana_cox <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                                 data = Floreana_survival)

# If year included it shows some issues tending to infinity

summary(Floreana_cox)
Anova(Floreana_cox)

emmeans::emmeans(Floreana_cox, ~ treatment|size, type = "response")

# So by using emmeans we estimate the relative Hazard Ratio of each treatment by size
# which is useful to show which one has more risk of being eaten.
# This is useful for describing the results in terms of HR.

# So in this case, Floreana's mericarps have the highest risk if they are small
# and with no spines

#### Survival plot Floreana eaten ####
ggsurvplot(KM_Floreana_eaten, legend = "right",
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

## Missing mericarps ####

KM_Floreana_miss <- survfit(Surv(days_pass, Present) ~ size + treatment,
                             data = Floreana_survival)

Floreana_surv_diff_miss <- survdiff(Surv(days_pass, Present) ~ size + treatment,
                               data = Floreana_survival)

#### Floreana Cox Missing ####
Floreana_cox_miss <- coxph(Surv(days_pass, Present) ~ size + treatment,
                      data = Floreana_survival)

# If year included it shows some issues tending to infinity

summary(Floreana_cox_miss)
Anova(Floreana_cox_miss)

emmeans::emmeans(Isabela_cox_miss, ~ treatment|size, type = "response")


#### Survival plot Floreana missing ####
ggsurvplot(KM_Floreana_miss, legend = "right",
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


# Isabela ####
## Eaten mericarps ####

KM_Isabela_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                             data = Isabela_survival)

Isabela_surv_diff <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                               data = Isabela_survival)

#### Isabela Cox Eaten ####
Isabela_cox <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                      data = Isabela_survival)

# If year included it shows some issues tending to infinity

summary(Isabela_cox)
Anova(Isabela_cox)

emmeans::emmeans(Isabela_cox, ~ treatment|size, type = "response")

#### Survival plot Isabela eaten ####
ggsurvplot(KM_Isabela_eaten, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - Lower spines",
                           "Large - No spines",
                           "Large - Upper spines",
                           "Small - All spines",
                           "Small - Lower spines",
                           "Small - No spines",
                           "Small - Upper spines"),
           title = "Isabela",
           ggtheme = plot_theme)


## Missing mericarps ####

KM_Isabela_miss <- survfit(Surv(days_pass, Present) ~ size + treatment,
                            data = Isabela_survival)

Isabela_surv_diff_miss <- survdiff(Surv(days_pass, Present) ~ size + treatment,
                              data = Isabela_survival)

#### Isabela Cox Eaten ####
Isabela_cox_miss <- coxph(Surv(days_pass, Present) ~ size + treatment,
                     data = Isabela_survival)

# If year included it shows some issues tending to infinity

summary(Isabela_cox_miss)
Anova(Isabela_cox_miss)

emmeans::emmeans(Isabela_cox_miss, ~ treatment|size, type = "response")

#### Survival plot Isabela eaten ####
ggsurvplot(KM_Isabela_miss, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - Lower spines",
                           "Large - No spines",
                           "Large - Upper spines",
                           "Small - All spines",
                           "Small - Lower spines",
                           "Small - No spines",
                           "Small - Upper spines"),
           title = "Isabela",
           ggtheme = plot_theme)

# Santa Cruz ####
## Eaten mericarps ####

KM_Cruz_eaten <- survfit(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                            data = Cruz_survival)

Cruz_surv_diff <- survdiff(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                              data = Cruz_survival)

#### Cruz Cox Eaten ####
Cruz_cox <- coxph(Surv(days_pass, Eaten_Birds) ~ size + treatment,
                     data = Cruz_survival)

# If year included it shows some issues tending to infinity

summary(Cruz_cox)
Anova(Cruz_cox)

emmeans::emmeans(Cruz_cox, ~ treatment|size, type = "response")

#### Survival plot Cruz eaten ####
ggsurvplot(KM_Cruz_eaten, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - Lower spines",
                           "Large - No spines",
                           "Large - Upper spines",
                           "Small - All spines",
                           "Small - Lower spines",
                           "Small - No spines",
                           "Small - Upper spines"),
           title = "Santa Cruz",
           ggtheme = plot_theme)


## Missing mericarps ####

KM_Cruz_miss <- survfit(Surv(days_pass, Present) ~ size + treatment,
                           data = Cruz_survival)

Cruz_surv_diff_miss <- survdiff(Surv(days_pass, Present) ~ size + treatment,
                                   data = Cruz_survival)

#### Cruz Cox Eaten ####
Cruz_cox_miss <- coxph(Surv(days_pass, Present) ~ size + treatment,
                          data = Cruz_survival)

# If year included it shows some issues tending to infinity

summary(Cruz_cox_miss)
Anova(Cruz_cox_miss)

emmeans::emmeans(Cruz_cox_miss, ~ treatment|size, type = "response")

#### Survival plot Cruz eaten ####
ggsurvplot(KM_Cruz_miss, legend = "right",
           surv.median.line = "hv",
           pval = T,
           conf.int = T,
           palette = colors,
           xlab = "Time (Days)",
           legend.title = "Mericarp Categories",
           legend.labs = c("Large - All spines",
                           "Large - Lower spines",
                           "Large - No spines",
                           "Large - Upper spines",
                           "Small - All spines",
                           "Small - Lower spines",
                           "Small - No spines",
                           "Small - Upper spines"),
           title = "Santa Cruz",
           ggtheme = plot_theme)


# Additional test ####
# Used this one to test a better way to interpret the results
----
# Run univariate models per each covariate and summarize indvidual hazard
# rations and beta estimates
covariates <- c("size", "treatment")
univ_formula <- sapply(covariates,
                       function(x) as.formula(paste('Surv(days_pass,Eaten_Birds)~', x)))

univ_models <- lapply(univ_formula, function(x){coxph(x,data=Floreana_survival)})

# Extract data
univ_results <- lapply(univ_models, function(x){
                    x <- summary(x)
                    p.value <- signif(x$wald["pvalue"], digits = 2)
                    wald.test <- signif(x$wald["test"], digits = 2)
                    beta<-signif(x$coef[1], digits=2);#coeficient beta
                    HR <-signif(x$coef[2], digits=2);#exp(beta)
                    HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                    HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                    HR <- paste0(HR, " (", 
                                 HR.confint.lower, "-", HR.confint.upper, ")")
                    res<-c(beta, HR, wald.test, p.value)
                    names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                  "p.value")
                    return(res)
                    #return(exp(cbind(coef(x),confint(x))))
})

res <- t(as.data.frame(univ_results, check.names = F))
as.data.frame(res)

# Size is significant difference in survival


