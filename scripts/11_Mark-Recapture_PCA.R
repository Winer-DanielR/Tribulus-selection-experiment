# PCA script

# Load the mark recapture datasets ####
# The mark recapture datasets are separated by island and year. Here I am going to merge all
# the datasets and keep 2018 experiment and 2019 experiment separate. 
# They are independent experiments and should be treated as such.

# These datasets have time as a single column, which produces repeated measurements
# I will extract the measurements of time 0, then merge the measurements of all islands per year.

# Time as a single column (Time 0 - 3)

## Datasets 2018 ####
Floreana_18 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2018.csv")
Isabela_18 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2018.csv")
Cruz_18 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2018.csv")

Floreana_18 <- rename(Floreana_18, mark_position = "mark position")
Isabela_18 <- rename(Isabela_18, mark_position = "mark position")
Cruz_18 <- rename(Cruz_18, mark_position = "mark position")

## Datasets 2019 ####
Floreana_19 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2019.csv")
Isabela_19 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2019.csv")
Cruz_19 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/santa Cruz 2019.csv")

Floreana_19 <- rename(Floreana_19, mark_position = "mark position")
Isabela_19 <- rename(Isabela_19, mark_position = "mark position")
Cruz_19 <- rename(Cruz_19, mark_position = "mark position")


# Filter time 0 ####
# I need to filter the time column. Only need one set of measurements.

Floreana18_filter <- filter(Floreana_18, time == "0")
Isabela18_filter <- filter(Isabela_18, time == "0")
Cruz18_filter <- filter(Cruz_18, time == "0")

Floreana19_filter <- filter(Floreana_19, time == "0")
Isabela19_filter <- filter(Isabela_19, time == "0")
Cruz19_filter <- filter(Cruz_19, time == "0")

# Data arragements
# I noticed that some datasets needed some changes before merging

str(Floreana18_filter)
str(Cruz18_filter)
str(Isabela18_filter)

str(Floreana19_filter)
str(Cruz19_filter)
str(Isabela19_filter)

Floreana19_filter <- Floreana19_filter %>% mutate_at(vars(Number_Seeds_Eaten), list(as.double))


Isabela19_filter <- Isabela19_filter %>% mutate_at(vars(Eaten_Insects,
                                                        Number_Seeds_Eaten, Germinated,
                                                        Seed_Germinated), list(as.double))

# Merge filtered datasets
# This is based on Andrew's comment to join years even if they are independent
# but firts I think I need to merge them per year and then merge them both.
# In that way, if we decide to keep them separate I do not need to code again.
 

MR_2018 <- bind_rows(Floreana18_filter, Isabela18_filter, Cruz18_filter)

MR_2019 <- bind_rows(Floreana19_filter, Isabela19_filter, Cruz19_filter)

Mark_recapture <- bind_rows(MR_2018, MR_2019)

# I need to create a year column

Mark_recapture$year <- format(as.Date(Mark_recapture$date, format = "%d/%m/%Y"), "%Y")

#write_csv(Mark_recapture, "Mark_recapture_dataset.csv")

# Starting dataset ####
Mark_recapture <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Mark_recapture_dataset.csv")

hist(Mark_recapture$length, breaks = 50)
hist(Mark_recapture$width, breaks = 50) # There is a measurement error!
hist(Mark_recapture$depth, breaks = 50)
# There is an individual error, I need to correct it. On width.

Mark_recapture[669,11] = 3.12



## Data preparation for PCA ####
## If I remove all NAs, this removes the no spine treatments,
## This leaves us with two potential sets:
## One, were we include size, spine and position but this removes
## all the no spine treatments.
## 
## Two, another dataset where we only select size traits and lower spines,
## ## this one includes all treatments of the experiment but wont include
## spine treatments like longest spine or spine position 
## 
## Three, another dataset where I only use size traits. This is because those are
## the only ones that all mericarps have and we can include most of the individuals

summary(is.na(Mark_recapture))

# This dataset selects all traits and removes NAs (Removes no spine treatments)
MR_all_traits <- Mark_recapture  %>%
  select(year, island, treatment, size, color, mark_position,
         length, width, depth, longest_spine, 
         spine_position, lower_spine, Present, Eaten_Birds, Eaten_Insects)  %>%
  drop_na()


hist(MR_all_traits$length, breaks = 50)
hist(MR_all_traits$width, breaks = 50)
hist(MR_all_traits$depth, breaks = 50)

# This dataset uses all the measured mericarp traits. However, it excludes most
# of the treatments. In fact, it only includes all spines and upper spines only.
# 
# I realized that we did not measured lower spines for the no spine treatment
# for Isabela and Santa Cruz in 2019. This reduces the number of lower spines individuals for all
# treatments.
# 
# This dataset has 1190 observations.

# This dataset selects all treatments and removes spine traits (longest spine, spine position)
MR_all_treatments <- Mark_recapture  %>%
  select(year, island, treatment, size, color, mark_position,
         length, width, depth, #longest_spine, 
         #spine_position, 
         lower_spine, 
         Present, Eaten_Birds, Eaten_Insects)  %>%
  drop_na()

# This dataset has 1989 observations and drops Isabela and Santa Cruz from 2019

hist(MR_all_treatments$length, breaks = 50)
hist(MR_all_treatments$width, breaks = 50)
hist(MR_all_treatments$depth, breaks = 50)


MR_all_size <- Mark_recapture  %>%
  select(year, island, treatment, size, color, mark_position,
         length, width, depth, #longest_spine, 
         #spine_position, 
         #lower_spine, 
         Present, Eaten_Birds, Eaten_Insects)  %>%
  drop_na()

# This dataset uses only mericarp size traits for the PCA and has most of the mericarps
# for some reason there are no measurements of size in floreana for a couple of mericarps used.
# I really cannot recall why we did this. But this removes 10 mericarps.

hist(MR_all_size$length, breaks = 50)
hist(MR_all_size$width, breaks = 50)
hist(MR_all_size$depth, breaks = 50)

## PCA ####
### All traits ####
MR_all_traits_pca <- prcomp(MR_all_traits[,c(7:12)], scale=TRUE)
summary(MR_all_traits_pca)

### All treatments ####
MR_all_treatments_pca <- prcomp(MR_all_treatments[,c(7:10)], scale=TRUE)
summary(MR_all_treatments_pca)

### All treatments size only ####
MR_all_size_pca <- prcomp(MR_all_size[,c(7:9)], scale=TRUE)
summary(MR_all_size_pca)


# Eigenvalues
fviz_eig(MR_all_traits_pca)
fviz_eig(MR_all_treatments_pca)
fviz_eig(MR_all_size_pca)

# The loadings show the proportions for each trait
loadings_traits <- MR_all_traits_pca$rotation # "rotation" is what R calls the PCA loadings
loadings_traits <- as.data.frame(loadings_traits)

loadings_treatments <- MR_all_treatments_pca$rotation # "rotation" is what R calls the PCA loadings
loadings_treatments <- as.data.frame(loadings_treatments)

loadings_size <- MR_all_size_pca$rotation # "rotation" is what R calls the PCA loadings
loadings_size <- as.data.frame(loadings_size)

# Export loadings

# write_csv(loadings_traits, "Trait_PCA_Mark_Recapture.csv")
# write_csv(loadings_treatments, "Treatments_PCA_Mark_Recapture.csv")
# write_csv(loadings_size, "Size_PCA_Mark_Recapture.csv")


# This is the PCA scores, used for the models later
scores_traits <- MR_all_traits_pca$x # "x" is what R calls the species scores
scores_traits <- as.data.frame(scores_traits)

scores_treatments <- MR_all_treatments_pca$x # "x" is what R calls the species scores
scores_treatments <- as.data.frame(scores_treatments)

scores_size <- MR_all_size_pca$x # "x" is what R calls the species scores
scores_size <- as.data.frame(scores_size)

# Combine the scores and the mericarp dataset.
MR_all_traits <- bind_cols(MR_all_traits, scores_traits)
MR_all_treatments <- bind_cols(MR_all_treatments, scores_treatments)
MR_all_size <- bind_cols(MR_all_size, scores_size)

# I exported this dataset to use it for the later questions.

var1 <- get_pca_var(MR_all_traits_pca)
var2 <- get_pca_var(MR_all_treatments_pca)
var3 <- get_pca_var(MR_all_size_pca)

# Perhaps this is used for the variance table?
trait_contrib1 <- var1$contrib
trait_contrib1 <- as.data.frame(trait_contrib1)

trait_contrib2 <- var2$contrib
trait_contrib2 <- as.data.frame(trait_contrib2)

trait_contrib3 <- var3$contrib
trait_contrib3 <- as.data.frame(trait_contrib3)

# Exported thecontributions per trait as a table

# write_csv(trait_contrib1, "Trait_contrib_Mark_Recapture.csv")
# write_csv(trait_contrib2, "Treatments_contrib_Mark_Recapture.csv")
# write_csv(trait_contrib3, "Size_contrib_Mark_Recapture.csv")

## PCA plots ####
fviz_pca_var(MR_all_traits_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             axes = c(1,2)) # plot axes

# It seems that PC1 is Size and PC2 is Defense AND Position

fviz_pca_var(MR_all_treatments_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             axes = c(1,2)) # plot axes

# It seems that PC1 is Size and PC2 is lower spines

fviz_pca_var(MR_all_size_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             axes = c(1,2)) # plot axes

# In this case, PC1 is size but PC2 is length-width and depth is PC3.
# For the size PCA I think that PC1 is enough

## Extract and transform PCs ####
# Size <- (-1)*MR_all_size_pca$x[,1]
# Size <- as.data.frame(Size)
# #Why we change signs? I think changing the signs makes it more intuitive,
# #Larger mericarps are positive with this transformation.
# 
# Defense <- MR_all_size_pca$x[,2]
# Defense <- as.data.frame(Defense)
# # This would be the same as PC2
# 
# # In this case for all mericarps it seems that PC1 and PC2 are enough.
# 
# # Join the transformed PC scores into the mericarp dataset
# MR_all_size <- bind_cols(MR_all_size, Size)
# MR_all_size <- bind_cols(MR_all_treatments, Defense)


# Expport mericarp dataset for models
# write_csv(MR_all_traits, "Traits_MR_PCA_scores.csv")
# write_csv(MR_all_treatments, "Treatments_MR_PCA_scores.csv")
# write_csv(MR_all_size, "Size_MR_PCA_scores.csv")

### Individual PCA ####
# It uses mericarp_NA as habillage because lower spines there is a factor.
fviz_pca_ind(MR_all_traits_pca, repel = T, geom = c("point"), habillage = MR_all_traits$size, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)

fviz_pca_biplot(MR_all_size_pca, repel = T,
                geom = c("point"),
                habillage = MR_all_size$size,
                col.var = "black",
                addEllipses = T
)


fviz_pca_ind(MR_all_treatments_pca, repel = T, geom = c("point"), habillage = MR_all_treatments$treatment, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)

fviz_pca_biplot(MR_all_treatments_pca, repel = T,
                geom = c("point"),
                habillage = MR_all_treatments$island,
                col.var = "black",
                addEllipses = T
)


fviz_pca_ind(MR_all_size_pca, repel = T, geom = c("point"), habillage = MR_all_size$size, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)

fviz_pca_biplot(MR_all_traits_pca, repel = T,
                geom = c("point"),
                habillage = MR_all_traits$size,
                col.var = "black",
                addEllipses = T
)

### Theme individual Biplot ####
biplot2 <- fviz_pca_biplot(mericarp_pca,
                           # Fill individuals by groups
                           title = "PCA Point in Time
                           ",
                           axes = c(1,3),
                           geom.ind = "point",
                           pointshape = c(21),
                           #label = "none",
                           #habillage = mericarp$island,
                           pointsize = 3.2,
                           stroke = 0.8,
                           fill.ind = mericarp$island,
                           col.ind = "black",
                           # Color variable by groups
                           legend.title = "Islands",
                           repel = T,
                           col.var = "black", 
                           labelsize = 5,
                           addEllipses = T,
                           palette = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73"),
                           
) + theme_transparent() + 
   scale_color_manual(values = c("#a95aa1", "#85c0f9", "#f5793a",  "#0f2080", "#009e73")) +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(fill = NA, size = 0))

biplot2

biplot <- fviz_pca_biplot(mericarp_pca,
                          habillage = mericarp$island,
                          addEllipses = T,
                          label = "none",
                          ellipse.level = 0.95)

### Theme individual Variable plots ####

var2 <- fviz_pca_var(mericarp_pca,
                     col.var = "contrib",
                     axes = c(2,3),
                     title = "Variables contribution
                           ",
                     gradient.cols = c("#f5793a", "#a95aa1", "#85c0f9", "#0f2080", "#009e73"),
                     repel = TRUE,
                     legend.title = "Contribution"
) +
  theme_transparent() +
  # PCA theme, adds custom font and sizes that matches the other plots    
  theme(axis.line = element_line(linetype = "solid", size = 1.5), 
        axis.title = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(size = 11), 
        plot.title = element_text(size = 16, face = "bold", hjust = 0),
        text = element_text(family = "Noto Sans"),
        legend.text = element_text(size = 12, face = "bold"), 
        legend.title = element_text(size = 14, face = "bold"),
        legend.position = "right",
        legend.background = element_rect(fill = NA, size = 0))
var2


# Data preparation for  Models ####
## Question 1 ####


# mericarp_size <- glmmTMB(eaten ~ size + (1|island/population),
#                          REML = F,
#                          family = binomial(link = "logit"),
#                          data = mericarp)
# 
# # Test residuals model using DHARMa
# testResiduals(mericarp_size)
# hist(resid(mericarp_size), breaks = 50)
# 
# # Model summary
# summary(mericarp_size)
# Anova(mericarp_size, type = "3")
# 
# plot(ggpredict(mericarp_size, terms = "size [all]",
#                allow.new.levels = T))
# 
