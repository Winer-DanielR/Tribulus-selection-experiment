# This script is to arrange and create the figure for question 4
# The figure would be a bar plot with the proportios of eaten mericarps
# per treatments.
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

# Transform some columns into factors for the plot
Floreana_MR <- Floreana_MR %>% mutate_at(vars(year,
                                              time,
                                              island,
                                              treatment,
                                              size,
                                              color,
                                              mark_position,
                                              plate), list(factor))


Isabela_MR <- Isabela_MR %>% mutate_at(vars(year,
                                              time,
                                              island,
                                              treatment,
                                              size,
                                              color,
                                              mark_position,
                                              plate), list(factor))


Cruz_MR <- Cruz_MR %>% mutate_at(vars(year,
                                              time,
                                              island,
                                              treatment,
                                              size,
                                              color,
                                              mark_position,
                                              plate), list(factor))


str(Floreana_MR)
str(Isabela_MR)
str(Cruz_MR)
# Transform treatments into factors


# Extract the frequencies of present mericarps by size and treatment
# Grouped by year, time and island

## Floreana ####
Floreana_present <- group_by(Floreana_MR, year, time, island, treatment, size)
#Floreana_present <- filter(Floreana_present, !time =="0")
# Perhaps the proportions are better estimated if I take out time 0 which is an absolute.

Floreana_present <- dplyr::count(Floreana_present, Present)
# This counts present mericaprs

Floreana_present <- Floreana_present %>%  
  group_by(year, time, Present) %>% mutate(freq_present = n/sum(n))

Floreana_present$Presence <- ifelse(Floreana_present$Present == "1","Present","Missing")


## Isabela ####
Isabela_present <- group_by(Isabela_MR, year, time, island, treatment, size)
#Isabela_present <- filter(Isabela_present, !time =="0")
# Perhaps the proportions are better estimated if I take out time 0 which is an absolute.

Isabela_present <- dplyr::count(Isabela_present, Present)
# This counts present mericaprs

Isabela_present <- Isabela_present %>%  
  group_by(year, time, Present) %>% mutate(freq_present = n/sum(n))

Isabela_present$Presence <- ifelse(Isabela_present$Present == "1","Present","Missing")


## Santa Cruz ####
Cruz_present <- group_by(Cruz_MR, year, time, island, treatment, size)
#Cruz_present <- filter(Cruz_present, !time =="0")
# Perhaps the proportions are better estimated if I take out time 0 which is an absolute.

Cruz_present <- dplyr::count(Cruz_present, Present)
# This counts present mericarps

Cruz_present <- Cruz_present %>%  
  group_by(year, time, Present) %>% mutate(freq_present = n/sum(n))

Cruz_present$Presence <- ifelse(Cruz_present$Present == "1","Present","Missing")

# Plots ####

Floreana_present %>%
  filter(!(time %in% c("0", "4"))) %>%
  ggplot() +
  aes(x = treatment, y = freq_present, fill = Presence) +
  geom_col(position = position_dodge()
    ) +
  #geom_text(aes(label = paste(freq_present, "%")), vjust = -0.25) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Size and Treatment",
    y = "Frequency %",
    title = "Floreana",
    subtitle = "Joined times 1, 2 and 3",
    fill = "Mericarps"
  ) +
  theme_minimal() +
  facet_wrap(~size, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")

## Isabela ####

Isabela_present %>%
  filter(!(time %in% c("0", "4"))) %>%
  ggplot() +
  aes(x = treatment, y = freq_present, fill = Presence) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Size and Treatment",
    y = "Frequency %",
    title = "Isabela",
    fill = "Mericarps"
  ) +
  theme_minimal() +
  facet_wrap(~size, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")

## Santa Cruz ####

Cruz_present %>%
  filter(!(time %in% c("0", "4"))) %>%
  ggplot() +
  aes(x = treatment, y = freq_present, fill = Presence) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Size and Treatments",
    y = "Frequency %",
    title = "Santa Cruz",
    fill = "Mericarps"
  ) +
  theme_minimal() +
  facet_wrap(~size, strip.position = "bottom", scales = "free_x") +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.placement = "outside")

