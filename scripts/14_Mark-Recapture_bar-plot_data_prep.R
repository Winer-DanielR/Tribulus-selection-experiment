# Mark recapture data prep for bar plot
# This is the data preparation for the second plot. One using bar plots
# of only present-uneaten mericarps

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

## Floreana ####
Floreana_counts <- group_by(Floreana_MR, time, Categories, Bird_Survival)
Floreana_counts <- dplyr::count(Floreana_counts, Bird_Survival)
## Santa Cruz ####
Cruz_counts <- group_by(Cruz_MR, time, Categories, Bird_Survival)
Cruz_counts <- dplyr::count(Cruz_counts, Bird_Survival)
## Isabela ####
Isabela_counts <- group_by(Isabela_MR, time, Categories, Bird_Survival)
Isabela_counts <- dplyr::count(Isabela_counts, Bird_Survival)

# Rename the categories for each island
# This will rename the categories like so:
# 2s are present eaten mericarps
# 1s are present uneaten 
# 0s are missing mericarps.

levels(Floreana_counts$Bird_Survival)  
levels(Floreana_counts$Bird_Survival) <- c("Missing", "Uneaten", "Eaten")

levels(Isabela_counts$Bird_Survival)  
levels(Isabela_counts$Bird_Survival) <- c("Missing", "Uneaten", "Eaten")

levels(Cruz_counts$Bird_Survival)  
levels(Cruz_counts$Bird_Survival) <- c("Missing", "Uneaten", "Eaten")

# This changed the names of the survival categories from numbers to words.

## Filter Uneaten mericarps ####
## Now, we need to filter uneaten mericarps for the datasets.

Floreana_uneaten <- filter(Floreana_counts, (Bird_Survival %in% c("Uneaten")))

Isabela_uneaten <- filter(Isabela_counts, (Bird_Survival %in% c("Uneaten")))

Cruz_uneaten <- filter(Cruz_counts, (Bird_Survival %in% c("Uneaten")))


# Now, for Isabela and Santa Cruz we have additional categories that I need to filter out
# Large all spines
# Large no spines
# Small all spines
# Small no spines
# These are one group, the remaining categories goes into another group.

Cruz_uneaten_all_spines <- filter(Cruz_uneaten, (Categories %in% c("Large_No_spines",
                                                                   "Large_All_spines",
                                                                   "Small_No_spines",
                                                                   "Small_All_spines")))

Cruz_uneaten_lower_spines <- filter(Cruz_uneaten, (Categories %in% c("Large_Lower_spines",
                                                                     "Large_Upper_spines",
                                                                     "Small_Lower_spines",
                                                                     "Small_Upper_spines")))


Isabela_uneaten_all_spines <- filter(Isabela_uneaten, (Categories %in% c("Large_No_spines",
                                                                   "Large_All_spines",
                                                                   "Small_No_spines",
                                                                   "Small_All_spines")))

Isabela_uneaten_lower_spines <- filter(Isabela_uneaten, (Categories %in% c("Large_Lower_spines",
                                                                     "Large_Upper_spines",
                                                                     "Small_Lower_spines",
                                                                     "Small_Upper_spines")))



## Now we need to calculate the relative frequency of the categories for uneaten
## mericarps OVER time. So, the total of time is up to a 100.

Floreana_uneaten <- Floreana_uneaten %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)

# Total of mericarps for Isabela and Santa Cruz
Isabela_uneaten <- Isabela_uneaten %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)

Cruz_uneaten <- Cruz_uneaten %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)


# Filtered Isabela and Santa Cruz groups
Isabela_uneaten_all_spines <- Isabela_uneaten_all_spines %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)

Isabela_uneaten_lower_spines <- Isabela_uneaten_lower_spines %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)


Cruz_uneaten_all_spines <- Cruz_uneaten_all_spines %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)

Cruz_uneaten_lower_spines <- Cruz_uneaten_lower_spines %>%  
  group_by(time) %>% mutate(freq_time = n/sum(n)*100)


# Bar plots over time ####
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

# All categories per island ####

## Floreana
ggplot(Floreana_uneaten) +
 aes(x = time, y = freq_time, fill = Categories) +
 geom_col(color = "black", size = 0.6) +
 scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                   labels = c("Large All Spines",
                              "Large No Spines",
                              "Small All Spines",
                              "Small No Spines")) +
 # scale_fill_brewer(palette = "Dark2", 
 # direction = 1) +
 labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Floreana") +
 plot_theme

## Isabela
ggplot(Isabela_uneaten) +
  aes(x = time, y = freq_time, fill = Categories) +
  geom_col(color = "black", size = 0.6) +
  # scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
  #                   labels = c("Large All Spines",
  #                              "Large No Spines",
  #                              "Small All Spines",
  #                              "Small No Spines")) +
  scale_fill_brewer(palette = "Dark2", 
  direction = 1) +
  labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Isabela") +
  plot_theme

## Santa Cruz
ggplot(Cruz_uneaten) +
  aes(x = time, y = freq_time, fill = Categories) +
  geom_col(color = "black", size = 0.6) +
  # scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
  #                   labels = c("Large All Spines",
  #                              "Large No Spines",
  #                              "Small All Spines",
  #                              "Small No Spines")) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Santa Cruz") +
  plot_theme

# Filtered groups per island ####
## Isabela all spines ####
ggplot(Isabela_uneaten_all_spines) +
  aes(x = time, y = freq_time, fill = Categories) +
  geom_col(color = "black", size = 0.6) +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large All Spines",
                               "Large No Spines",
                               "Small All Spines",
                               "Small No Spines")) +
  # scale_fill_brewer(palette = "Dark2", 
  # direction = 1) +
  labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Isabela All Spines") +
  plot_theme

## Santa Cruz all spines ####
ggplot(Cruz_uneaten_all_spines) +
  aes(x = time, y = freq_time, fill = Categories) +
  geom_col(color = "black", size = 0.6) +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large All Spines",
                               "Large No Spines",
                               "Small All Spines",
                               "Small No Spines")) +
  # scale_fill_brewer(palette = "Dark2", 
  # direction = 1) +
  labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Santa Cruz All Spines") +
  plot_theme

## Isabela lower spines ####
ggplot(Isabela_uneaten_lower_spines) +
  aes(x = time, y = freq_time, fill = Categories) +
  geom_col(color = "black", size = 0.6) +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large Lower Spines",
                               "Large Upper Spines",
                               "Small Lower Spines",
                               "Small Upper Spines")) +
  # scale_fill_brewer(palette = "Dark2", 
  # direction = 1) +
  labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Isabela Lower Spines") +
  plot_theme

## Santa Cruz lower spines ####
ggplot(Cruz_uneaten_lower_spines) +
  aes(x = time, y = freq_time, fill = Categories) +
  geom_col(color = "black", size = 0.6) +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large Lower Spines",
                               "Large Upper Spines",
                               "Small Lower Spines",
                               "Small Upper Spines")) +
  # scale_fill_brewer(palette = "Dark2", 
  # direction = 1) +
  labs(x = "Time", y = "Frequency Uneaten Mericarps (%)", title = "Santa Cruz Lower Spines") +
  plot_theme

