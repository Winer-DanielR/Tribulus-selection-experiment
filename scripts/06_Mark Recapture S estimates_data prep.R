#### Mark Recapture S* estimates per year, island and population ####
# By: Daniel Reyes Corral
# In this script I want to group mericarps per year, island, treatments and population
# Estimate their mean differences per trait with their CIs.
# First I need to copile the eaten information from the different times it was monitored
# into a single eaten column. To do this I am extracting the eaten columns per time and add them into 
# a single one that corresponds to any eaten mericarp included, regarldless of when it was found eaten.
# this would make it similar to the point in time S* estimates per island, treatment, and year.

# This script will use each dataset per island and per year and modify them to match this new eaten column
# I will start with each dataset. 2018s datasets have at least 4 sampled times. 2019s datasets only have one.


# Santa Cruz 2018 ####
# Load the point in time dataset
MC_StCruz_18 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2018.csv")
MC_StCruz_18 <- as_tibble(MC_StCruz_18)

# Changed variables to factors
MC_StCruz_18 <- MC_StCruz_18 %>% mutate_at(vars(time,
                                            island, treatment, size, color, plate,
                                            Present, Eaten_Birds,
                                            Eaten_Insects, days_pass, Number_Seeds_Eaten,
                                            Germinated), list(factor))
str(MC_StCruz_18)
MC_StCruz_18 <- dplyr::rename(MC_StCruz_18, mericarp = mericarps)
# The subset selects time, island, treatment, size, color and the mericarp traits, with eaten birds
MC_StCruz_subset <- select(MC_StCruz_18, time, c(4:14), Present, Eaten_Birds)
str(MC_StCruz_subset)
MC_StCruz_subset <- group_by(MC_StCruz_subset, treatment, size, color)
# Group them by these variables which I am interested in doing S*comparisons

# This separates the time and eaten birds columns, so it shows the eaten mericarps over each monitored time
MC_StCruz_pivot <- pivot_wider(MC_StCruz_subset, names_from = time,
                                   values_from = c(13:14))

MC_StCruz_eaten <- select(MC_StCruz_pivot, Eaten_Birds_0,
                      Eaten_Birds_1,
                      Eaten_Birds_2,
                      Eaten_Birds_3,
                      Eaten_Birds_4)
str(MC_StCruz_eaten)

# Here I convert eaten back into numbers to converge them into a single column.
eaten1 <- ungroup(MC_StCruz_eaten)
eaten1 <- select(eaten1, c(4:8))
eaten1[c(1:5)] <- lapply(eaten1, function(x) as.numeric(as.character(x)))                    
eaten1 <- mutate(eaten1, eaten_sum = Eaten_Birds_0 + ifelse(is.na(Eaten_Birds_1), 0, Eaten_Birds_1 +
                                              ifelse(is.na(Eaten_Birds_2), 0, Eaten_Birds_2 +
                                              ifelse(is.na(Eaten_Birds_3), 0, Eaten_Birds_3 +
                                              ifelse(is.na(Eaten_Birds_4), 0, Eaten_Birds_4)))))
eaten1$eaten_sum[eaten1$eaten_sum > 1] <- 1

# Eaten sum 1 represents the outcome if we assume that NAs (lost mericarps) are eaten mericarps
eaten1 <- mutate(eaten1, eaten_sum1 = Eaten_Birds_0 + ifelse(is.na(Eaten_Birds_1), 1, Eaten_Birds_1 +
                                                              ifelse(is.na(Eaten_Birds_2), 1, Eaten_Birds_2 +
                                                                       ifelse(is.na(Eaten_Birds_3), 1, Eaten_Birds_3 +
                                                                                ifelse(is.na(Eaten_Birds_4), 1, Eaten_Birds_4)))))

eaten1$eaten_sum1[eaten1$eaten_sum1 > 1] <- 1

# Finally, I add the eaten sum column into the pivot table so it includes this column to calculate
# the S* estimate
MC_StCruz_18_eaten <- bind_cols(MC_StCruz_pivot, eaten1$eaten_sum, eaten1$eaten_sum1)
MC_StCruz_18_eaten <- dplyr::rename(MC_StCruz_18_eaten, eaten_sum = ...22, eaten_sum1 = ...23)
# Added a new column with the year of the dataset
year <- rep("2018",400)
MC_StCruz_18_eaten$year <- year

MC_StCruz_18_eaten <- MC_StCruz_18_eaten %>% mutate_at(vars(eaten_sum, eaten_sum1, year), list(factor))

# Floreana 2018 ####
# Load the point in time dataset
MC_Floreana_18 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2018.csv")
MC_Floreana_18 <- as_tibble(MC_Floreana_18)

# Changed variables to factors
MC_Floreana_18 <- MC_Floreana_18 %>% mutate_at(vars(time,
                                                island, treatment, size, color, plate,
                                                Present, Eaten_Birds,
                                                Eaten_Insects, days_pass, Number_Seeds_Eaten,
                                                Germinated), list(factor))
str(MC_Floreana_18)
MC_Floreana_18 <- dplyr::rename(MC_Floreana_18, mericarp = mericarps)
# The subset selects time, island, treatment, size, color and the mericarp traits, with eaten birds
MC_Floreana_subset <- select(MC_Floreana_18, time, c(4:14), Present, Eaten_Birds)
str(MC_Floreana_subset)
MC_Floreana_subset <- group_by(MC_Floreana_subset, treatment, size, color)
# Group them by these variables which I am interested in doing S*comparisons

# This separates the time and eaten birds columns, so it shows the eaten mericarps over each monitored time
MC_Floreana_pivot <- pivot_wider(MC_Floreana_subset, names_from = time,
                               values_from = c(13:14))

MC_Floreana_eaten <- select(MC_Floreana_pivot, Eaten_Birds_0,
                          Eaten_Birds_1,
                          Eaten_Birds_2,
                          Eaten_Birds_3,
                          )
str(MC_Floreana_eaten)
# Here, note that some samples were missing even in the first time. So what we are doing here
# is to estimate the eaten mericarps of only the mericarps we recovered. The missing ones
# are considered uneaten even though they may be lost or eaten.

# Here I convert eaten back into numbers to converge them into a single column.
eaten2 <- ungroup(MC_Floreana_eaten)
eaten2 <- select(eaten2, c(4:7))
eaten2[c(1:4)] <- lapply(eaten2, function(x) as.numeric(as.character(x)))                    
eaten2 <- mutate(eaten2, eaten_sum = Eaten_Birds_0 + ifelse(is.na(Eaten_Birds_1), 0, Eaten_Birds_1 +
                                                              ifelse(is.na(Eaten_Birds_2), 0, Eaten_Birds_2 +
                                                                       ifelse(is.na(Eaten_Birds_3), 0, Eaten_Birds_3))))
eaten2$eaten_sum[eaten2$eaten_sum > 1] <- 1

# Eaten sum 1 represents the outcome if we assume that NAs (lost mericarps) are eaten mericarps
eaten2 <- mutate(eaten2, eaten_sum1 = Eaten_Birds_0 + ifelse(is.na(Eaten_Birds_1), 1, Eaten_Birds_1 +
                                                               ifelse(is.na(Eaten_Birds_2), 1, Eaten_Birds_2 +
                                                                        ifelse(is.na(Eaten_Birds_3), 1, Eaten_Birds_3))))

eaten2$eaten_sum1[eaten2$eaten_sum1 > 1] <- 1

# Finally, I add the eaten sum column into the pivot table so it includes this column to calculate
# the S* estimate
MC_Floreana_18_eaten <- bind_cols(MC_Floreana_pivot, eaten2$eaten_sum, eaten2$eaten_sum1)
MC_Floreana_18_eaten <- dplyr::rename(MC_Floreana_18_eaten, eaten_sum = ...22, eaten_sum1 = ...23)
# Added a new column with the year of the dataset
year <- rep("2018",400)
MC_Floreana_18_eaten$year <- year 

MC_Floreana_18_eaten <- MC_Floreana_18_eaten %>% mutate_at(vars(eaten_sum, eaten_sum1, year), list(factor))

# Isabela 2018 ####
# Load the point in time dataset
MC_Isabela_18 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2018.csv")
MC_Isabela_18 <- as_tibble(MC_Isabela_18)

# Changed variables to factors
MC_Isabela_18 <- MC_Isabela_18 %>% mutate_at(vars(time,
                                                    island, treatment, size, color, plate,
                                                    Present, Eaten_Birds,
                                                    Eaten_Insects, days_pass, Number_Seeds_Eaten,
                                                    Germinated), list(factor))
str(MC_Isabela_18)
MC_Isabela_18 <- dplyr::rename(MC_Isabela_18, mericarp = mericarps)
# The subset selects time, island, treatment, size, color and the mericarp traits, with eaten birds
MC_Isabela_subset <- select(MC_Isabela_18, time, c(4:14), Present, Eaten_Birds)
MC_Isabela_subset <- group_by(MC_Isabela_subset, treatment, size, color)
str(MC_Isabela_subset)
# Group them by these variables which I am interested in doing S*comparisons

# This separates the time and eaten birds columns, so it shows the eaten mericarps over each monitored time
MC_Isabela_pivot <- pivot_wider(MC_Isabela_subset, names_from = time,
                                 values_from = c(13:14))

MC_Isabela_eaten <- select(MC_Isabela_pivot, Eaten_Birds_0,
                            Eaten_Birds_1,
                            Eaten_Birds_2,
                            Eaten_Birds_3)
str(MC_Isabela_eaten)
# Here, note that some samples were missing even in the first time. So what we are doing here
# is to estimate the eaten mericarps of only the mericarps we recovered. The missing ones
# are considered uneaten even though they may be lost or eaten.

# Here I convert eaten back into numbers to converge them into a single column.
eaten3 <- ungroup(MC_Isabela_eaten)
eaten3 <- select(eaten3, c(4:7))
eaten3[c(1:4)] <- lapply(eaten3, function(x) as.numeric(as.character(x)))                    
eaten3 <- mutate(eaten3, eaten_sum = Eaten_Birds_0 + ifelse(is.na(Eaten_Birds_1), 0, Eaten_Birds_1 +
                                                              ifelse(is.na(Eaten_Birds_2), 0, Eaten_Birds_2 +
                                                                       ifelse(is.na(Eaten_Birds_3), 0, Eaten_Birds_3))))
eaten3$eaten_sum[eaten3$eaten_sum > 1] <- 1

eaten3 <- mutate(eaten3, eaten_sum1 = Eaten_Birds_0 + ifelse(is.na(Eaten_Birds_1), 1, Eaten_Birds_1 +
                                                              ifelse(is.na(Eaten_Birds_2), 1, Eaten_Birds_2 +
                                                                       ifelse(is.na(Eaten_Birds_3), 1, Eaten_Birds_3))))
eaten3$eaten_sum1[eaten3$eaten_sum1 > 1] <- 1

# Finally, I add the eaten sum column into the pivot table so it includes this column to calculate
# the S* estimate
MC_Isabela_18_eaten <- bind_cols(MC_Isabela_pivot, eaten3$eaten_sum, eaten3$eaten_sum1)
MC_Isabela_18_eaten <- dplyr::rename(MC_Isabela_18_eaten, eaten_sum = ...22, eaten_sum1=...23)
# Added a new column with the year of the dataset
year <- rep("2018",400)
MC_Isabela_18_eaten$year <- year 

MC_Isabela_18_eaten <- MC_Isabela_18_eaten %>% mutate_at(vars(eaten_sum, year, eaten_sum1), list(factor))

# Santa Cruz 2019 ####
# For the 2019 mark recapture datasets I only was able to collect data once. So it means that the eaten bird
# column from time 1 is the final data. I will create a new column with another name to match the eaten sum column
# and year column to potentially match all these new datasets into a single dataset where we can do the S* estimates

# Load the point in time dataset
MC_StCruz_19 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2019.csv")
MC_StCruz_19 <- as_tibble(MC_StCruz_19)


# Changed variables to factors
MC_StCruz_19 <- MC_StCruz_19 %>% mutate_at(vars(time,
                                                island, treatment, size, color, plate,
                                                Present, Eaten_Birds,
                                                Eaten_Insects, days_pass, Number_Seeds_Eaten,
                                                Germinated), list(factor))
str(MC_StCruz_19)

# The subset selects time, island, treatment, size, color and the mericarp traits, with eaten birds
MC_StCruz_subset_19 <- select(MC_StCruz_19, time, c(4:14), Present, Eaten_Birds)
MC_StCruz_subset_19 <- group_by(MC_StCruz_subset_19, treatment, size, color)
str(MC_StCruz_subset_19)

# Group them by these variables which I am interested in doing S*comparisons

# This separates the time and eaten birds columns, so it shows the eaten mericarps over each monitored time
MC_StCruz_pivot_19 <- pivot_wider(MC_StCruz_subset_19, names_from = time,
                               values_from = c(13:14))

# I arrange the eaten mericarps to assume that NAs are not eaten (eaten sum) and that NAs are eaten (eaten sum 1)
eaten4 <- MC_StCruz_pivot_19$Eaten_Birds_1
eaten4 <- lapply(eaten4, function(x) as.numeric(as.character(x)))  
eaten4 <- as.numeric(eaten4)
eaten4 <- as.tibble(eaten4)
eaten4 <- dplyr::rename(eaten4, eaten_sum = value)
eaten4_1 <- eaten4
eaten4[is.na(eaten4)] = 0
eaten4_1[is.na(eaten4_1)] = 1
eaten4 <- cbind(eaten4, eaten4_1)
eaten4 <- dplyr::rename(eaten4, eaten_sum1 = c(2))


MC_StCruz_pivot_19$eaten_sum <- eaten4$eaten_sum
MC_StCruz_pivot_19$eaten_sum1 <- eaten4$eaten_sum1

year <- rep("2019",400)
MC_StCruz_pivot_19$year <- year

MC_StCruz_eaten_19 <- MC_StCruz_pivot_19

MC_StCruz_eaten_19 <- MC_StCruz_eaten_19 %>% mutate_at(vars(eaten_sum, eaten_sum1, year), list(factor))

# Floreana 2019 ####
MC_Floreana_19 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2019.csv")
MC_Floreana_19 <- as_tibble(MC_Floreana_19)


# Changed variables to factors
MC_Floreana_19 <- MC_Floreana_19 %>% mutate_at(vars(time,
                                                island, treatment, size, color, plate,
                                                Present, Eaten_Birds,
                                                Eaten_Insects, days_pass, Number_Seeds_Eaten,
                                                Germinated), list(factor))
str(MC_Floreana_19)

# The subset selects time, island, treatment, size, color and the mericarp traits, with eaten birds
MC_Floreana_subset_19 <- select(MC_Floreana_19, time, c(4:14), Present, Eaten_Birds)
MC_Floreana_subset_19 <- group_by(MC_Floreana_subset_19, treatment, size, color)
str(MC_Floreana_subset_19)

# Group them by these variables which I am interested in doing S*comparisons

# This separates the time and eaten birds columns, so it shows the eaten mericarps over each monitored time
MC_Floreana_pivot_19 <- pivot_wider(MC_Floreana_subset_19, names_from = time,
                                  values_from = c(13:14))

eaten5 <- MC_Floreana_pivot_19$Eaten_Birds_1
eaten5 <- lapply(eaten5, function(x) as.numeric(as.character(x)))
eaten5 <- as.numeric(eaten5)
eaten5 <- as.tibble(eaten5)
eaten5 <- dplyr::rename(eaten5, eaten_sum = value)
eaten5_1 <- eaten5
eaten5[is.na(eaten5)] = 0
eaten5_1[is.na(eaten5_1)] = 1
eaten5 <- cbind(eaten5, eaten5_1)
eaten5 <- dplyr::rename(eaten5, eaten_sum1 = c(2))

MC_Floreana_pivot_19$eaten_sum <- eaten5$eaten_sum
MC_Floreana_pivot_19$eaten_sum1 <- eaten5$eaten_sum1

year <- rep("2019",400)
MC_Floreana_pivot_19$year <- year
MC_Floreana_eaten_19 <- MC_Floreana_pivot_19

MC_Floreana_eaten_19 <- MC_Floreana_eaten_19 %>% mutate_at(vars(eaten_sum, eaten_sum1, year), list(factor))

# Isabela 2019 ####
MC_Isabela_19 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2019.csv")
MC_Isabela_19 <- as_tibble(MC_Isabela_19)


# Changed variables to factors
MC_Isabela_19 <- MC_Isabela_19 %>% mutate_at(vars(time,
                                                    island, treatment, size, color, plate,
                                                    Present, Eaten_Birds,
                                                    Eaten_Insects, days_pass, Number_Seeds_Eaten,
                                                    Germinated), list(factor))
str(MC_Isabela_19)

# The subset selects time, island, treatment, size, color and the mericarp traits, with eaten birds
MC_Isabela_subset_19 <- select(MC_Isabela_19, time, c(4:14), Present, Eaten_Birds)
MC_Isabela_subset_19 <- group_by(MC_Isabela_subset_19, treatment, size, color)
str(MC_Isabela_subset_19)

# Group them by these variables which I am interested in doing S*comparisons

# This separates the time and eaten birds columns, so it shows the eaten mericarps over each monitored time
MC_Isabela_pivot_19 <- pivot_wider(MC_Isabela_subset_19, names_from = time,
                                    values_from = c(13:14))

eaten6 <- MC_Isabela_pivot_19$Eaten_Birds_1
eaten6 <- lapply(eaten6, function(x) as.numeric(as.character(x)))  
eaten6 <- as.numeric(eaten6)
eaten6 <- as.tibble(eaten6)
eaten6 <- dplyr::rename(eaten6, eaten_sum = value)
eaten6_1 <- eaten6
eaten6[is.na(eaten6)] = 0
eaten6_1[is.na(eaten6_1)] = 1
eaten6 <- cbind(eaten6, eaten6_1)
eaten6 <- dplyr::rename(eaten6, eaten_sum1 = c(2))


MC_Isabela_pivot_19$eaten_sum <- eaten6$eaten_sum
MC_Isabela_pivot_19$eaten_sum1 <- eaten6$eaten_sum1
year <- rep("2019",401)
MC_Isabela_pivot_19$year <- year
MC_Isabela_eaten_19 <- MC_Isabela_pivot_19

MC_Isabela_eaten_19 <- MC_Isabela_eaten_19 %>% mutate_at(vars(eaten_sum, eaten_sum1, year), list(factor))

# Join all these databases into a single one to calculate the S* Estimates
# Database joining ####

mark_recapture_18 <- bind_rows(MC_StCruz_18_eaten,
                            MC_Floreana_18_eaten,
                            MC_Isabela_18_eaten)

mark_recapture_19 <- bind_rows(MC_StCruz_eaten_19,
                               MC_Floreana_eaten_19,
                               MC_Isabela_eaten_19)

mark_recapture_19$longest_spine <- as.numeric(mark_recapture_19$longest_spine)
mark_recapture_19$spine_tip_distance <- as.numeric(mark_recapture_19$spine_tip_distance)

## Export processed datasets
# write.csv(mark_recapture_18, "Mark Recapture dataset 2018.csv")
# write.csv(mark_recapture_19, "Mark Recapture dataset 2019.csv")


# Join 2018 and 2019 datasets
mark_recapture_S <- bind_rows(mark_recapture_18, mark_recapture_19)

# write.csv(mark_recapture_S, "Mark Recapture S.csv")
