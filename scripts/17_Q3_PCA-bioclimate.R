# PCA script

# Load the PCA for populations to extract bioclimate variables ####
# This dataset is the point in time dataset for all years

env <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_population_NAs.csv")
env <- as_tibble(env)

### PCA populations ####
env <- env %>% mutate_at(vars(island, population), list(factor))
str(env)
env <- na.omit(env)

## Data preparation ####
bioclimate <- env  %>%
  select(island, population, Bio_1, Bio_4, Bio_12, Bio_15, finch_beak)  %>%
  drop_na()

str(bioclimate)

# This removes all the NAs leaving only 3393 individuals from 2017 and 2018 where all traits were collected

## PCA ####
bioclimate_pca <- prcomp(bioclimate[,c(3:7)], scale=TRUE)
summary(bioclimate_pca)

# Eigenvalues
fviz_eig(bioclimate_pca)

# The loadings show the proportions for each trait
loadings <- bioclimate_pca$rotation # "rotation" is what R calls the PCA loadings
loadings <- as.data.frame(loadings)

#write_csv(loadings, "PCA_bioclimate_loadings.csv")
# I exported this for reference as a csv.

# This is the PCA scores, used for the models later
scores <- bioclimate_pca$x # "x" is what R calls the species scores
scores <- as.data.frame(scores)

# Combine the scores and the mericarp dataset.
bioclimate <- bind_cols(bioclimate, scores)

bioclimate <- rename(bioclimate,
                     PC1_bioclimate = PC1,
                     PC2_bioclimate = PC2,
                     PC3_bioclimate = PC3,
                     PC4_bioclimate = PC4,
                     PC5_bioclimate = PC5)

# I need to join both datasets. The PCA bioclimates and the population mericarps

bioclimate_pcs <- select(bioclimate,
                         PC1_bioclimate,
                         PC2_bioclimate,
                         PC3_bioclimate,
                         PC4_bioclimate,
                         PC5_bioclimate)

env <- bind_cols(env, bioclimate_pcs)

#write_csv(env, "PCA_populations_bioclimate.csv")

var <- get_pca_var(bioclimate_pca)
# Perhaps this is used for the variance table?
trait_contrib <- var$contrib
trait_contrib <- as.data.frame(trait_contrib)

# Exported thecontributions per trait as a table
#write_csv(trait_contrib, "PCA_bioclimate_contrib.csv")

## PCA plots ####
fviz_pca_var(bioclimate_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             axes = c(2,3)) # plot axes


### Individual PCA ####
# It uses mericarp_NA as habillage because lower spines there is a factor.
fviz_pca_ind(bioclimate_pca, repel = T, geom = c("point"), habillage = bioclimate$island, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)

fviz_pca_biplot(bioclimate_pca, repel = T,
                geom = c("point"),
                habillage = bioclimate$island,
                col.var = "black",
                addEllipses = T
)

### Theme individual Biplot ####
biplot2 <- fviz_pca_biplot(bioclimate_pca,
                           # Fill individuals by groups
                           title = "PCA Bioclimate
                           ",
                           axes = c(1,2),
                           geom.ind = "point",
                           pointshape = c(21),
                           #label = "none",
                           #habillage = mericarp$island,
                           pointsize = 3.2,
                           stroke = 1,
                           fill.ind = bioclimate$island,
                           col.ind = "black",
                           # Color variable by groups
                           legend.title = "Islands",
                           repel = T,
                           col.var = "black", 
                           labelsize = 4,
                           arrowsize = 0.8,
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

### Theme individual Variable plots ####

var2 <- fviz_pca_var(bioclimate_pca,
                     col.var = "contrib",
                     axes = c(1,5),
                     arrowsize = 0.8,
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

# I need to join both datasets. The PCA bioclimates and the population mericarps
# 
bioclimate <- rename(bioclimate,
                     PC1_bioclimate = PC1,
                     PC2_bioclimate = PC2,
                     PC3_bioclimate = PC3,
                     PC4_bioclimate = PC4)

bioclimate_pcs <- select(bioclimate,
                         PC1_bioclimate,
                         PC2_bioclimate,
                         PC3_bioclimate,
                         PC4_bioclimate)

env <- bind_cols(env, bioclimate_pcs)

#write_csv(env, "PCA_populations_bioclimate.csv")

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
