# Question 1: Are Galápagos mericarps generally under selection? ####

# Date: 09 May, 2024
# By: Daniel Reyes

# Point in time PCA data prep ####
# Mean PCA scores ####
# I think the way to do this is either use the individual PC
# scores here and the mean values, because I would need the mean values anyway.
# For the model for this question I can do it with the individual values or mean values
# Here I am taking the mean values.

# Import dataset
pca <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PCA/PCA_scores.csv")

# Convert factors
pca <- pca %>% mutate_at(vars(year, island, 
                              population, lower_spine, 
                              eaten), list(factor))
str(pca)
# Keep in mind that Size, Defense and Position are the transformed PC scores (*-1)

## Dataset prep ####
# Eaten, Uneaten
# First I think I need to separate the PC axes that I am using
# PC1, PC2 and PC3. These are now my trait values.

pca_eaten_ind <- select(pca, year, island, population, mericarp, eaten,
                        c(12:14, 18:20))
# This selects the axes of interest and if they were eaten/uneaten.
# Next is to estimate the means and
# separate them into values for eaten and uneaten mericarps

# Group for mean estimates ####
### Grouping the PC axes to take PC scores means by all eaten mericarps,
### island and population within islands.

## Group by island all mericarps mean ####
## This is for estimating the average of the PC scores by island
pca_eaten_ind <- group_by(pca_eaten_ind, island)

pca_means <- pca_eaten_ind %>%
  summarise_each(funs(mean = mean), c(5:10))

## Group by island eaten####
pca_eaten_ind <- group_by(pca_eaten_ind, island, eaten)

pca_means_island <- pca_eaten_ind %>%  
  summarise_each(funs(mean = mean,
                      var = var,
                      se = sd(.)/sqrt(n()),
                      n = length,
  ), c(4:9))


# Pivot table
## To compare eaten and uneaten mericarps
pca_means_island <- pivot_wider(pca_means_island, names_from = eaten,
                                values_from = c(3:26))

### S estimates ####
pca_means_island$S_PC1 <- (pca_means_island$PC1_mean_0 - 
                             pca_means_island$PC1_mean_1)

pca_means_island$S_PC2 <- (pca_means_island$PC2_mean_0 - 
                             pca_means_island$PC2_mean_1)

pca_means_island$S_PC3 <- (pca_means_island$PC3_mean_0 - 
                             pca_means_island$PC3_mean_1)

pca_means_island$S_Size <- (pca_means_island$Size_mean_0 - 
                              pca_means_island$Size_mean_1)

pca_means_island$S_Defense <- (pca_means_island$Defense_mean_0 - 
                                 pca_means_island$Defense_mean_1)

pca_means_island$S_Position <- (pca_means_island$Position_mean_0 - 
                                  pca_means_island$Position_mean_1)

# I can estimate the difference of uneaten and eaten mericarps (selection) in this dataset.

# Join the two mean datasets
pca_means_island <- left_join(pca_means, pca_means_island, by = "island")

# Export this dataset for plots
#write_csv(pca_means_island, "PCA_islands.csv")

## Group by population ####
# All mericarp means by population
pca_eaten_ind <- group_by(pca_eaten_ind, island, population)

pca_mean <- pca_eaten_ind %>%
  summarise_each(funs(mean = mean), c(4:9))

## Means by population eaten 
pca_eaten_ind <- group_by(pca_eaten_ind, island, population, eaten)

pca_means_pop <- pca_eaten_ind %>%  
  summarise_each(funs(mean = mean,
                      var = var,
                      se = sd(.)/sqrt(n()),
                      n = length,
  ), c(3:8))

#### Pivot table 
## To compare eaten and uneaten mericarps
pca_means_pop <- pivot_wider(pca_means_pop, names_from = eaten,
                             values_from = c(4:27))

# Replace NAs as 0 frequencies
pca_means_pop[is.na(pca_means_pop)] = 0

### S estimates ####
pca_means_pop$S_PC1 <- (pca_means_pop$PC1_mean_0 - 
                          pca_means_pop$PC1_mean_1)

pca_means_pop$S_PC2 <- (pca_means_pop$PC2_mean_0 - 
                          pca_means_pop$PC2_mean_1)

pca_means_pop$S_PC3 <- (pca_means_pop$PC3_mean_0 - 
                          pca_means_pop$PC3_mean_1)

pca_means_pop$S_Size <- (pca_means_pop$Size_mean_0 - 
                           pca_means_pop$Size_mean_1)

pca_means_pop$S_Defense <- (pca_means_pop$Defense_mean_0 - 
                              pca_means_pop$Defense_mean_1)

pca_means_pop$S_Position <- (pca_means_pop$Position_mean_0 - 
                               pca_means_pop$Position_mean_1)

# Join the datasets
pca_means_pop <- left_join(pca_mean, pca_means_pop, by = c("island", "population"))

# Export the tables 
#write_csv(pca_means_pop, "PCA_population_NAs.csv")

