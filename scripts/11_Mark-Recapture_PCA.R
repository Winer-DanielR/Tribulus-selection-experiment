# PCA script

# Load the point in time dataset ####
# This dataset is the point in time dataset for all years

point_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors
point_time <- point_time %>% mutate_at(vars(year,
                                            island, population, 
                                            #lower_spine, 
                                            #spine_position, 
                                            eaten, eaten_insects,
                                            `year island`, year_pop,
                                            seed_position_1, seed_position_2,
                                            seed_position_3, seed_position_4,
                                            seed_position_5, seed_position_6,
                                            germinated, germinated_position_1,
                                            germinated_position_2, germinated_position_3,
                                            germinated_position_4, germinated_position_5,
                                            germinated_position_6), list(factor))
str(point_time)

## Data preparation ####
mericarp <- point_time  %>%
  select(year, island, population, mericarp, length, width, depth, 
         longest_spine, spine_position, lower_spine, eaten)  %>%
  drop_na()

str(mericarp)

# This removes all the NAs leaving only 3393 individuals from 2017 and 2018 where all traits were collected

## PCA ####
mericarp_pca <- prcomp(mericarp[,c(5:10)], scale=TRUE)
summary(mericarp_pca)

# Eigenvalues
fviz_eig(mericarp_pca)

# The loadings show the proportions for each trait
loadings <- mericarp_pca$rotation # "rotation" is what R calls the PCA loadings
loadings <- as.data.frame(loadings)
# I exported this for reference as a csv. To check which traits are better.
# With the loadings, I was able to check that I could transform PC1 and PC3,
# because their values are negative.

# This is the PCA scores, used for the models later
scores <- mericarp_pca$x # "x" is what R calls the species scores
scores <- as.data.frame(scores)

# Combine the scores and the mericarp dataset.
mericarp <- bind_cols(mericarp, scores)
# I exported this dataset to use it for the later questions.

#Extract and transform PCs
Size <- (-1)*mericarp_pca$x[,1]
Size <- as.data.frame(Size)
#Why we change signs? I think changing the signs makes it more intuitive,
#Larger mericarps are positive with this transformation.

Defense <- mericarp_pca$x[,2]
Defense <- as.data.frame(Defense)
# This would be the same as PC2

Position <- (-1)*mericarp_pca$x[,3]
Position <- as.data.frame(Position)
# I think we could transform the spine position PC3 as well.

# Join the transformed PC scores into the mericarp dataset
mericarp <- bind_cols(mericarp, Size)
mericarp <- bind_cols(mericarp, Defense)
mericarp <- bind_cols(mericarp, Position)

# Expport mericarp dataset for models
#write_csv(mericarp, "PCA_scores.csv")

var <- get_pca_var(mericarp_pca)
# Perhaps this is used for the variance table?
trait_contrib <- var$contrib
trait_contrib <- as.data.frame(trait_contrib)

# Exported thecontributions per trait as a table
#write_csv(trait_contrib, "PCA_var_contrib.csv")

## PCA plots ####
fviz_pca_var(mericarp_pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = T,
             axes = c(1,3)) # plot axes


### Individual PCA ####
# It uses mericarp_NA as habillage because lower spines there is a factor.
fviz_pca_ind(mericarp_pca, repel = T, geom = c("point"), habillage = mericarp$eaten, palette = NULL,
             addEllipses = T, col.ind = "blue", col.ind.sup = "darkblue",
             alpha.ind = 1, shape.ind = 19, col.quali.var = "black",
             select.ind = list(name = NULL, cos2 = NULL, contrib = NULL),
             gradient.cols = NULL)

fviz_pca_biplot(mericarp_pca, repel = T,
                geom = c("point"),
                habillage = mericarp$eaten,
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
