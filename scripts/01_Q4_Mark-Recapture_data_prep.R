# Question 4 Data Preparation ####
# By Daniel Reyes
# 2023

## Description: ####
# This script is to arrange and prepare the data of the mark recapture experiment
#  create the figure for question 4
# The figure would be a bar plot with the proportions of eaten mericarps
# per treatments.

 
# Data loading ####
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

# Mark recapture histogram distributions ####

### Santa Cruz 2018 ####
SC2018 <- ggplot(Cruz_2018) +
 aes(x = length, fill = size) +
 geom_histogram(bins = 30L, color="black") +
 scale_fill_brewer(palette = "Dark2", 
 direction = 1) +
 labs(x = "Mericarp Length (mm)", y = "Frequency", title = "Santa Cruz 2018", fill = "Size Treatment") +
 theme_classic()

### Santa Cruz 2019 ####
SC2019 <- ggplot(Cruz_2019) +
  aes(x = length, fill = size) +
  geom_histogram(bins = 30L, color="black") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Mericarp Length (mm)", y = "Frequency", title = "Santa Cruz 2019", fill = "Size Treatment") +
  theme_classic()

### Floreana 2018 ####
FL2018 <- ggplot(Floreana_2018) +
  aes(x = length, fill = size) +
  geom_histogram(bins = 30L, color="black") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Mericarp Length (mm)", y = "Frequency", title = "Floreana 2018", fill = "Size Treatment") +
  theme_classic()

### Floreana 2019 ####
FL2019 <- ggplot(Floreana_2019) +
  aes(x = length, fill = size) +
  geom_histogram(bins = 30L, color="black") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Mericarp Length (mm)", y = "Frequency", title = "Floreana 2019", fill = "Size Treatment") +
  theme_classic()

### Isabela 2018 ####
IS2018 <- ggplot(Isabela_2018) +
  aes(x = length, fill = size) +
  geom_histogram(bins = 30L, color="black") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Mericarp Length (mm)", y = "Frequency", title = "Isabela 2018", fill = "Size Treatment") +
  theme_classic()

### Isabela 2019 ####
IS2019 <- ggplot(Isabela_2019) +
  aes(x = length, fill = size) +
  geom_histogram(bins = 30L, color="black") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Mericarp Length (mm)", y = "Frequency", title = "Isabela 2019", fill = "Size Treatment") +
  theme_classic()

#### Combine histogram plots ####
MR_length_hist <- ggarrange(SC2018,
                            SC2019,
                            FL2018,
                            FL2019,
                            IS2018,
                            IS2019,
                              ncol = 2,
                              nrow = 3,
                            common.legend = TRUE, legend="right") + 
  theme(text = element_text(family = "Noto Sans"))

MR_length_hist <- annotate_figure(MR_length_hist,
                                             top = text_grob("Mark Recapture Size Histograms",
                                                             color = "black", face = "bold", size = 16))

# The figure was imported into Appendix 2. ##

### Size summary stats ####
#### Santa Cruz 2018 ####
Cruz_2018_summary <- Cruz_2018 %>% group_by(size) %>% summarise(mean = mean(length),
                                                                sd = sd(length),
                                                                min = min(length),
                                                                max = max(length),
                                                                q25 = quantile(length,0.25),
                                                                q75 = quantile(length,0.75)
                                                                )
#### Santa Cruz 2019 ####
Cruz_2019_summary <- Cruz_2019 %>% group_by(size) %>% summarise(mean = mean(length),
                                                                sd = sd(length),
                                                                min = min(length),
                                                                max = max(length),
                                                                q25 = quantile(length,0.25),
                                                                q75 = quantile(length,0.75)
)

#### Floreana 2018 ####
FL_2018_summary <- Floreana_2018 %>% group_by(size) %>%
  filter(!is.na(length)) %>% summarise(mean = mean(length),
                                                                sd = sd(length),
                                                                min = min(length),
                                                                max = max(length),
                                                                q25 = quantile(length,0.25),
                                                                q75 = quantile(length,0.75)
)
#### Floreana 2019 ####
FL_2019_summary <- Floreana_2019 %>% group_by(size) %>%
  filter(!is.na(length)) %>% summarise(mean = mean(length),
                                       sd = sd(length),
                                       min = min(length),
                                       max = max(length),
                                       q25 = quantile(length,0.25),
                                       q75 = quantile(length,0.75)
  )

#### Isabela 2018 ####
IS_2018_summary <- Isabela_2018 %>% group_by(size) %>%
  filter(!is.na(length)) %>% summarise(mean = mean(length),
                                       sd = sd(length),
                                       min = min(length),
                                       max = max(length),
                                       q25 = quantile(length,0.25),
                                       q75 = quantile(length,0.75)
  )

#### Isabela 2019 ####
IS_2019_summary <- Isabela_2019 %>% group_by(size) %>%
  filter(!is.na(length)) %>% summarise(mean = mean(length),
                                       sd = sd(length),
                                       min = min(length),
                                       max = max(length),
                                       q25 = quantile(length,0.25),
                                       q75 = quantile(length,0.75)
  )

# The summary stats were imported into Appendix 2.


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

# Selecting columns for model analysis ####

Floreana_MR <- select(Floreana_MR, c(2:3, 5:10, 18:20))
Isabela_MR <- select(Isabela_MR, c(2:3, 5:10, 18:20))
Cruz_MR <- select(Cruz_MR, c(2:3, 5:10, 18:20))

# 1. Life Span Analysis####
#### Selecting columns used in model analysis ####
# Select columns for analysis. This selects columns:
# year (2018-2019)
# time (0-4)
# island (Floreana, Isabela, Santa Cruz)
# treatment (All Spines, Lower spines, Upper spines, No spines)
# size (Large, Small)
# color (BLue, Green, Yellow, Red)
# mark position (Upper Left, Upper Right, Down Left, Down Right, Left, Right)
# mericarp ID
# plate ID
# Present (uneaten)
# Eaten

#### Filtering time intervals ####
# Times 0 and 4 are almost absolute values, so for the model analysis
# I filter time and only used times 1-3.
# Renamed MR1 for approach 1

# Removes time 0
Floreana_MR1 <- filter(Floreana_MR, !time == "0")
#Floreana_MR <- filter(Floreana_MR, !time == "4")

Isabela_MR1 <- filter(Isabela_MR, !time == "0")
#Isabela_MR <- filter(Isabela_MR, !time == "4")

Cruz_MR1 <- filter(Cruz_MR, !time == "0")
#Cruz_MR <- filter(Cruz_MR, !time == "4")


#### Filtering mericarps ####
#### Filtering present mericarps for the model analysis, present uneaten mericarps
#### are the ones that survived predation.

# Filtering present mericarps
Floreana_MR1 <- filter(Floreana_MR1, Present == "1")
# Filtering uneaten mericarps from those present, these we are sure survived the experiment
Floreana_MR1 <- filter(Floreana_MR1, Eaten_Birds == "0")

Isabela_MR1 <- filter(Isabela_MR1, Present == "1")
Isabela_MR1 <- filter(Isabela_MR1, Eaten_Birds == "0")

Cruz_MR1 <- filter(Cruz_MR1, Present == "1")
Cruz_MR1 <- filter(Cruz_MR1, Eaten_Birds == "0")

# Data prepartation Floreana ####

#### Creating a new column that groups all tratment combinations ####
#### This column will combine size and spine treatments, is named categories
#### Each island dataset will have the new Categories column

# This will create a new column for the categories
Floreana_MR1$Categories <- paste(Floreana_MR1$size, Floreana_MR1$treatment, sep = "_")
Floreana_MR1$Categories <- sub(" ", "_", Floreana_MR1$Categories)

#### Pivot tables ####
# Now I have to pivot the columns for each time passed, so I will create the life span score based on the added
# present values.


Floreana_survival <- group_by(Floreana_MR1, mericarp, year)
Floreana_survival <- pivot_wider(Floreana_survival, names_from = time, values_from = c(Present))

# I needed to turn the categorical variables into factors before merging everything by individual mericarp number.

# This will replace NAs into 0 for each monitoring time

Floreana_survival$`1`[is.na(Floreana_survival$`1`)] <- 0
Floreana_survival$`2`[is.na(Floreana_survival$`2`)] <- 0
Floreana_survival$`3`[is.na(Floreana_survival$`3`)] <- 0
Floreana_survival$`4`[is.na(Floreana_survival$`4`)] <- 0

## Life span estimate ####
# This is Floreana's lifespan estimate wich is the sum of all the 1s for each monitoring time
# the mericarps were present the new column is called life span and is the response variable
Floreana_survival$Life_span <- rowSums(Floreana_survival[, c(11:13)],na.rm = T)

# The dataset is exported for the model analysis!

#write_csv(Floreana_survival, "Floreana_life_span.csv")

# The same process goes for the other island datasets

# Data prepartation Isabela ####

#### Creating a new column that groups all treatment combinations ####
# This will create a new column for the size and spine categories
Isabela_MR1$Categories <- paste(Isabela_MR1$size, Isabela_MR1$treatment, sep = "_")
Isabela_MR1$Categories <- sub(" ", "_", Isabela_MR1$Categories)

#### Pivot tables ####
# Now I have to pivot the columns for each time passed, so I will create the life span score based on the added
# present values.
Isabela_survival <- group_by(Isabela_MR1, mericarp, year)
Isabela_survival <- pivot_wider(Isabela_survival, names_from = time, values_from = c(Present))
# I needed to turn the categorical variables into factors before merging everything by individual mericarp number.

# This will replace NAs into 0
Isabela_survival$`1`[is.na(Isabela_survival$`1`)] <- 0
Isabela_survival$`2`[is.na(Isabela_survival$`2`)] <- 0
Isabela_survival$`3`[is.na(Isabela_survival$`3`)] <- 0
Isabela_survival$`4`[is.na(Isabela_survival$`4`)] <- 0

## Life span estimate ####
Isabela_survival$Life_span <- rowSums(Isabela_survival[, c(11:14)],na.rm = T)

#write_csv(Isabela_survival, "Isabela_life_span.csv")

# Data prepartation Cruz ####
#### Creating a new column that groups all treatment combinations ####
# This will create a new column for the categories
Cruz_MR1$Categories <- paste(Cruz_MR1$size, Cruz_MR1$treatment, sep = "_")
Cruz_MR1$Categories <- sub(" ", "_", Cruz_MR1$Categories)

#### Pivot tables ####
# Now I have to pivot the columns for each time passed, so I will create the life span score based on the added
# present values.
Cruz_survival <- group_by(Cruz_MR1, mericarp, year)
Cruz_survival <- pivot_wider(Cruz_survival, names_from = time, values_from = c(Present))
# I needed to turn the categorical variables into factors before merging everything by individual mericarp number.

# This will replace NAs into 0
Cruz_survival$`1`[is.na(Cruz_survival$`1`)] <- 0
Cruz_survival$`2`[is.na(Cruz_survival$`2`)] <- 0
Cruz_survival$`3`[is.na(Cruz_survival$`3`)] <- 0
Cruz_survival$`4`[is.na(Cruz_survival$`4`)] <- 0

## Life span estimate ####
Cruz_survival$Life_span <- rowSums(Cruz_survival[, c(11:14)],na.rm = T)

#write_csv(Cruz_survival, "Cruz_life_span.csv")


# 2. Counting frequencies analysis ####

# This was used for the ternary plot
# Survival categories: Present Uneaten, Eaten, Missing
# Treatments: Upper, Lower, All, No spines. Large and Small sizes.
# 
# They need to add up to 100, across time.
# Example:
# LS1 = LS1PU + LS1PE + LS1M = 100
# LS is lower spines
# Where, PU, PE and M is Survival and LS is the category in question.
# So, for time1 all of them are present, so it should be (100, 0, 0).

# For this approach we added a new column defining survival categories in a
# more intuitive way.
# Survival days were added later using the raw datasets. 

## Create the Survival column ####

# Survival is a new column where we put together the three possible outputs of
# the experiment: Present-uneaten, present-eaten, missing mericarps.

# Finally, I will use the time column too for these plots.

#### Floreana Survival column ####
Floreana_MR$Bird_Survival <- (Floreana_MR$Present + Floreana_MR$Eaten_Birds)

# This creates a new column that adds the values of Present and Eaten columns
# This means that: 
#   A value of 2 are present and eaten mericarps
#   A value of 1s are present uneaten 
#   A value of 0s are missing mericarps.

# Then we are checking if there is any NAs on the new created column and replace
# them with 0 
Floreana_MR$Bird_Survival[is.na(Floreana_MR$Bird_Survival)] <- 0

#### Creating a new column that groups all tratment combinations ####
#### This column will combine size and spine treatments, is named categories
#### Each island dataset will have the new Categories column

# This will create a new column for the categories
Floreana_MR$Categories <- paste(Floreana_MR$size, Floreana_MR$treatment, sep = "_")
Floreana_MR$Categories <- sub(" ", "_", Floreana_MR$Categories)


#### Isabela survival column ####

Isabela_MR$Bird_Survival <- (Isabela_MR$Present + Isabela_MR$Eaten_Birds)
# Replacing NAs with 0s
Isabela_MR$Bird_Survival[is.na(Isabela_MR$Bird_Survival)] <- 0

#### Creating a new column that groups all treatment combinations ####
# This will create a new column for the size and spine categories
Isabela_MR$Categories <- paste(Isabela_MR$size, Isabela_MR$treatment, sep = "_")
Isabela_MR$Categories <- sub(" ", "_", Isabela_MR$Categories)


#### Santa Cruz survival column ####

Cruz_MR$Bird_Survival <- (Cruz_MR$Present + Cruz_MR$Eaten_Birds)
# Replacing Nas with 0
Cruz_MR$Bird_Survival[is.na(Cruz_MR$Bird_Survival)] <- 0

#### Creating a new column that groups all treatment combinations ####
# This will create a new column for the categories
Cruz_MR$Categories <- paste(Cruz_MR$size, Cruz_MR$treatment, sep = "_")
Cruz_MR$Categories <- sub(" ", "_", Cruz_MR$Categories)


## Floreana counts ####
## The dataset is named ternary because it was used for a preliminary ternary plot
## that was later discarded for the paper, however, this approach to test missing
## mericarps is still used in the models later.

Floreana_ternary <- group_by(Floreana_MR, time, Categories, Bird_Survival)

Floreana_ternary <- dplyr::count(Floreana_ternary, Bird_Survival)
# This counts present mericarps by survival when grouped by time and categories
# NOTE: Grouping them like this joins 2018 and 2019 experiments!

#### Floreana relative percentages ####

Floreana_ternary <- Floreana_ternary %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)
# This estimates the percentage of the counts.

#### Pivot table for the different survival categories
#### Present, Eaten, Missing
Floreana_ternary_pivot <- pivot_wider(Floreana_ternary, names_from = Bird_Survival,
                               values_from = c(4:5))

# Need to replace NAs for 0s
Floreana_ternary_pivot[is.na(Floreana_ternary_pivot)] <- 0

# I also need to rename the columns because the survival numbers are not that intutitive
Floreana_ternary_pivot <- rename(Floreana_ternary_pivot,
                                 n_uneaten = n_1,
                                 n_eaten = n_2,
                                 n_missing = n_0,
                                 Uneaten_freq = freq_cat_1,
                                 Eaten_freq = freq_cat_2,
                                 Missing_freq = freq_cat_0)

## Santa Cruz counts ####
Cruz_ternary <- group_by(Cruz_MR, time, Categories, Bird_Survival)

Cruz_ternary <- dplyr::count(Cruz_ternary, Bird_Survival)

# Here I am separating the spine categories into two datasets,
# the first one is ALL SPINES containing ALL and NO spine treatments
# the second one is LOWER SPINES containing LOWER and UPPER spine treatments
# 
# For Santa Cruz I think I can filter out the categories and match them to floreana's
# and calculate the percentages after filter the categories out.
# 
# IMPORTANT 
# The main reason to separate and filter spine treatments is beacuase in 2019 we only
# used the all and no spines treatments, and since we combined the numbers of the
# two experiments for this approach makes sense to only compare the spine
# treatments that were present on both years.

### Filter Santa Cruz spine treatments ####
#### No spines - All spines ####
Cruz_ternary_all_spines <- filter(Cruz_ternary, (Categories %in% c("Large_No_spines",
                                                                    "Large_All_spines",
                                                                    "Small_No_spines",
                                                                    "Small_All_spines")))
#### Lower - Upper spines ####
Cruz_ternary_lower_spines <- filter(Cruz_ternary, (Categories %in% c("Large_Lower_spines",
                                                                   "Large_Upper_spines",
                                                                   "Small_Lower_spines",
                                                                   "Small_Upper_spines")))
## Frequency estimates based on filtered datasets. These percentages are now relative to the 4
## categories

### Santa Cruz relative percentages ####
#### All - No spines treatments ####
Cruz_ternary_all_spines <- Cruz_ternary_all_spines %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)

#### Lower - Upper spine treatments ####
Cruz_ternary_lower_spines <- Cruz_ternary_lower_spines %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)

## Santa Cruz pivot datasets
Cruz_all_spines_pivot <- pivot_wider(Cruz_ternary_all_spines, names_from = Bird_Survival,
                                      values_from = c(4:5))

Cruz_lower_spines_pivot <- pivot_wider(Cruz_ternary_lower_spines, names_from = Bird_Survival,
                                     values_from = c(4:5))


## Replace NAs and rename columns for Santa Cruz datasets

Cruz_all_spines_pivot[is.na(Cruz_all_spines_pivot)] <- 0
Cruz_lower_spines_pivot[is.na(Cruz_lower_spines_pivot)] <- 0

Cruz_all_spines_pivot <- rename(Cruz_all_spines_pivot,
                                 n_uneaten = n_1,
                                 n_eaten = n_2,
                                 n_missing = n_0,
                                 Uneaten_freq = freq_cat_1,
                                 Eaten_freq = freq_cat_2,
                                 Missing_freq = freq_cat_0)

Cruz_lower_spines_pivot <- rename(Cruz_lower_spines_pivot,
                             n_uneaten = n_1,
                             n_eaten = n_2,
                             n_missing = n_0,
                             Uneaten_freq = freq_cat_1,
                             Eaten_freq = freq_cat_2,
                             Missing_freq = freq_cat_0)

# The new relative percentages can be used to better visualize the
# categories for Santa Cruz.

## Isabela counts ####
Isabela_ternary <- group_by(Isabela_MR, time, Categories, Bird_Survival)

Isabela_ternary <- dplyr::count(Isabela_ternary, Bird_Survival)

# For Isabela I think I can filter out the categories and match them to floreana's
# and calculate the percentages after filter the categories out.

### Filter Isabela spine treatments ####
#### No spines - All spines ####
Isabela_ternary_all_spines <- filter(Isabela_ternary, (Categories %in% c("Large_No_spines",
                                                                   "Large_All_spines",
                                                                   "Small_No_spines",
                                                                   "Small_All_spines")))

#### Upper - Lower spines ####
Isabela_ternary_lower_spines <- filter(Isabela_ternary, (Categories %in% c("Large_Lower_spines",
                                                                     "Large_Upper_spines",
                                                                     "Small_Lower_spines",
                                                                     "Small_Upper_spines")))

### Isabela relative percentages ####
## Frequency estimates based on filtered datasets. These percentages are now relative to the 4
## categories
Isabela_ternary_all_spines <- Isabela_ternary_all_spines %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)


Isabela_ternary_lower_spines <- Isabela_ternary_lower_spines %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)

## Isabela pivot datasets
Isabela_all_spines_pivot <- pivot_wider(Isabela_ternary_all_spines, names_from = Bird_Survival,
                                     values_from = c(4:5))

Isabela_lower_spines_pivot <- pivot_wider(Isabela_ternary_lower_spines, names_from = Bird_Survival,
                                       values_from = c(4:5))


## Replace NAs and rename columns for Santa Isabela datasets

Isabela_all_spines_pivot[is.na(Isabela_all_spines_pivot)] <- 0
Isabela_lower_spines_pivot[is.na(Isabela_lower_spines_pivot)] <- 0

Isabela_all_spines_pivot <- rename(Isabela_all_spines_pivot,
                                n_uneaten = n_1,
                                n_eaten = n_2,
                                n_missing = n_0,
                                Uneaten_freq = freq_cat_1,
                                Eaten_freq = freq_cat_2,
                                Missing_freq = freq_cat_0)

Isabela_lower_spines_pivot <- rename(Isabela_lower_spines_pivot,
                                  n_uneaten = n_1,
                                  n_eaten = n_2,
                                  n_missing = n_0,
                                  Uneaten_freq = freq_cat_1,
                                  Eaten_freq = freq_cat_2,
                                  Missing_freq = freq_cat_0)


# Combining frequency datasets ####
# Combining datasets per island into a single dataset for the ternary plot
# Now, this pivoted dataframe is the one that I need for the ternary plot per
# category and time.

# Combine island datasets for second ternary plot

Floreana_ternary_pivot$Island <- paste("Floreana")
Isabela_all_spines_pivot$Island <- paste("Isabela")
Isabela_lower_spines_pivot$Island <- paste("Isabela")
Cruz_all_spines_pivot$Island <- paste("Santa_Cruz")
Cruz_lower_spines_pivot$Island <- paste("Santa_Cruz")


Island_ternary <- bind_rows(Floreana_ternary_pivot,
                            Isabela_all_spines_pivot,
                            Isabela_lower_spines_pivot,
                            Cruz_all_spines_pivot,
                            Cruz_lower_spines_pivot)


Island_ternary$Island <- as.factor(Island_ternary$Island)
Island_ternary <- as_tibble(Island_ternary)
str(Island_ternary)

# Need to check if this combined ternary plot was used somewhere.
