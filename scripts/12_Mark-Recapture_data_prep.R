# This script is to arrange and create the figure for question 4
# The figure would be a bar plot with the proportions of eaten mericarps
# per treatments.
# Also, create a ternary plot.
# 
# First I will upload and merge the datasets
# 
# Data loading ####
## Floreana ####
Floreana_2018 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2018.csv")
Floreana_2019 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Floreana 2019.csv")

## Isabela ####
Isabela_2018 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2018.csv")
Isabela_2019 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Isabela 2019.csv")

## Santa Cruz ####
Cruz_2018 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2018.csv")
Cruz_2019 <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Raw/Santa Cruz 2019.csv")


# Check the columns first
str(Floreana_2018)
str(Floreana_2019)

str(Isabela_2018)
str(Isabela_2019)

str(Cruz_2018)
str(Cruz_2019)

# Merge the datasets per year
Floreana_MR <- bind_rows(Floreana_2018, Floreana_2019)
Isabela_MR <- bind_rows(Isabela_2018, Isabela_2019)
Cruz_MR <- bind_rows(Cruz_2018, Cruz_2019)

# Data preparation ####
# Select columns for analysis
Floreana_MR <- select(Floreana_MR, c(2:10, 18:21))
Isabela_MR <- select(Isabela_MR, c(2:10, 18:21))
Cruz_MR <- select(Cruz_MR, c(2:10, 18:21))
# This selects the columns needed for the plot

# Create the Survival column ####
# Survival is a new column where we put together the three possible outputs of
# the experiment: Present-uneaten, present-eaten, missing mericarps.

# I also create a new column named Categories, were I unite all the size and
# spine treatments.

# Finally, I will use the time column too for these plots.

## Floreana ####
## I am creating two columns one for Bird survival and one for insect survival
## I would like to explore insect predation even if is only a few mericarps.

Floreana_MR$Bird_Survival <- (Floreana_MR$Present + Floreana_MR$Eaten_Birds)
Floreana_MR$Insect_Survival <- (Floreana_MR$Present + Floreana_MR$Eaten_Insects)

# This creates two new columns that adds the values of Present and Eaten columns
# This means that:
# 2s are present eaten mericarps
# 1s are present uneaten 
# 0s are missing mericarps.

Floreana_MR$Bird_Survival[is.na(Floreana_MR$Bird_Survival)] <- 0
Floreana_MR$Insect_Survival[is.na(Floreana_MR$Insect_Survival)] <- 0

# This transforms NAs into 0s for the new survival columns

Floreana_MR$Categories <- paste(Floreana_MR$size, Floreana_MR$treatment, sep = "_")
Floreana_MR$Categories <- sub(" ", "_", Floreana_MR$Categories)


# This creates a new column that combines size and treatments

## Isabela ####
Isabela_MR$Bird_Survival <- (Isabela_MR$Present + Isabela_MR$Eaten_Birds)
Isabela_MR$Insect_Survival <- (Isabela_MR$Present + Isabela_MR$Eaten_Insects)

Isabela_MR$Bird_Survival[is.na(Isabela_MR$Bird_Survival)] <- 0
Isabela_MR$Insect_Survival[is.na(Isabela_MR$Insect_Survival)] <- 0

Isabela_MR$Categories <- paste(Isabela_MR$size, Isabela_MR$treatment, sep = "_")
Isabela_MR$Categories <- sub(" ", "_", Isabela_MR$Categories)


## Santa Cruz ####
Cruz_MR$Bird_Survival <- (Cruz_MR$Present + Cruz_MR$Eaten_Birds)
Cruz_MR$Insect_Survival <- (Cruz_MR$Present + Cruz_MR$Eaten_Insects)

Cruz_MR$Bird_Survival[is.na(Cruz_MR$Bird_Survival)] <- 0
Cruz_MR$Insect_Survival[is.na(Cruz_MR$Insect_Survival)] <- 0

Cruz_MR$Categories <- paste(Cruz_MR$size, Cruz_MR$treatment, sep = "_")
Cruz_MR$Categories <- sub(" ", "_", Cruz_MR$Categories)

# In this way we have the categories for treatments,
# outputs of survival and time for the plots.

# Data transformation ####
# Transform some columns into factors for the plot
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


# Counting and estimating frequencies ####
# For the ternary plot I need to group counts into the categories.
# They need to add up to 100, across time.
# Example:
# LS1 = LS1PU + LS1PE + LS1M = 100
# Where, PU, PE and M is Survival and LS is the category in question.
# So, for time1 all of them are present, so it should be (100, 0, 0).

## Floreana ####
Floreana_ternary <- group_by(Floreana_MR, time, Categories, Bird_Survival)

Floreana_ternary <- dplyr::count(Floreana_ternary, Bird_Survival)
# This counts present mericarps by survival when grouped by time and categories
# NOTE: Grouping them like this joins 2018 and 2019 experiments!

Floreana_ternary <- Floreana_ternary %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)
# This estimates the percentage of the counts.

# I think I need to pivot the table into something closer to the sum that
# Andrew told me.
Floreana_ternary_pivot <- pivot_wider(Floreana_ternary, names_from = Bird_Survival,
                               values_from = c(4:5))
# I think this is what I need all the values are set in the way we need.
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

## Santa Cruz ####
Cruz_ternary <- group_by(Cruz_MR, time, Categories, Bird_Survival)

Cruz_ternary <- dplyr::count(Cruz_ternary, Bird_Survival)

# For Santa Cruz I think I can filter out the categories and match them to floreana's
# and calculate the percentages after filter the categories out.

### Filter Santa Cruz ####
### No spines - All spines
Cruz_ternary_all_spines <- filter(Cruz_ternary, (Categories %in% c("Large_No_spines",
                                                                    "Large_All_spines",
                                                                    "Small_No_spines",
                                                                    "Small_All_spines")))

Cruz_ternary_lower_spines <- filter(Cruz_ternary, (Categories %in% c("Large_Lower_spines",
                                                                   "Large_Upper_spines",
                                                                   "Small_Lower_spines",
                                                                   "Small_Upper_spines")))
## Frequency estimates based on filtered datasets. These percentages are now relative to the 4
## categories
Cruz_ternary_all_spines <- Cruz_ternary_all_spines %>%  
  group_by(time, Categories) %>% mutate(freq_cat = n/sum(n)*100)


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

# Hopefully these new relative percentages can be used to better visualize the
# categories for Santa Cruz.

## Isabela ####
Isabela_ternary <- group_by(Isabela_MR, time, Categories, Bird_Survival)

Isabela_ternary <- dplyr::count(Isabela_ternary, Bird_Survival)

# For Santa Isabela I think I can filter out the categories and match them to floreana's
# and calculate the percentages after filter the categories out.

### Filter Isabela ####
### No spines - All spines
Isabela_ternary_all_spines <- filter(Isabela_ternary, (Categories %in% c("Large_No_spines",
                                                                   "Large_All_spines",
                                                                   "Small_No_spines",
                                                                   "Small_All_spines")))

Isabela_ternary_lower_spines <- filter(Isabela_ternary, (Categories %in% c("Large_Lower_spines",
                                                                     "Large_Upper_spines",
                                                                     "Small_Lower_spines",
                                                                     "Small_Upper_spines")))
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

# I am not sure about this combined island plot, but maybe it can work!

Island_ternary$Island <- as.factor(Island_ternary$Island)
Island_ternary <- as_tibble(Island_ternary)
str(Island_ternary)

