# Point in time S* estimates per year, island and population ####

# By: Daniel Reyes Corral


# In this script I want to group mericarps per year, island and population
# I am preparing each trait to estimate their means and calculate the difference
# between mean eaten and mean uneaten mericarps.
# Also calculate selection differentials mean(eaten+uneaten) - mean (uneaten)

# Load the point in time dataset ####
# This dataset is the point in time dataset for all years

## PCA point in time dataset ####
## The PCA dataset is the main set in the paper
point_time_pca <- read_csv()

## Single traits Point in time dataset ####
## This is mainly supplementary material
point_time <- read_csv("~/Thesis reasearch/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors ####
point_time <- point_time %>% mutate_at(vars(year,
                                      island, population, lower_spine, eaten, eaten_insects,
                                      `year island`, year_pop,
                                      seed_position_1, seed_position_2,
                                      seed_position_3, seed_position_4,
                                      seed_position_5, seed_position_6,
                                      germinated, germinated_position_1,
                                      germinated_position_2, germinated_position_3,
                                      germinated_position_4, germinated_position_5,
                                      germinated_position_6), list(factor))
str(point_time)

# Data preparation ####
# I grouped the dataset into year, island, population and survival (eaten/uneaten)
# The idea is to separate each mericarp trait: length, depth, width, spine distance, longest spine, lower spines
# into its own individual datasets to estimate means for the groups above.


# Group by year, island and populations
point_time <- group_by(point_time, year, island, population, eaten)

## Select per traits to check and remove NAs
## 
## For now the trait selected is length. We may need to come back later for the other traits.
## 
## Length ####
point_time_length <- select(point_time, c(1:8), eaten)
point_time_length <- na.omit(point_time_length)
point_time_length <- point_time_length %>%  
  summarise_each(funs(length_mean = mean,
                      length_var = var,
                      length_sd = sd,
                      length_se = sd(.)/sqrt(n()),
                      length_n = length,
  ), length)

### Difference between uneaten and eaten mericarps ####
point_time_length_S <- pivot_wider(point_time_length, names_from = eaten,
                                     values_from = c(5:9))
point_time_length_S$S_length <- (point_time_length_S$length_mean_0 - point_time_length_S$length_mean_1)


### Selection differentials ####
### Defined as mean(eaten+uneaten) - mean (uneaten). Per population

# Estimating population means
point_time_length_S$Pop_mean <- (point_time_length_S$length_mean_0 + point_time_length_S$length_mean_1)/2
# Calculating selection differentials per population
point_time_length_S$Sdif_length <- (point_time_length_S$Pop_mean) - (point_time_length_S$length_mean_0)


# Diverging plot for selection differentials
ggplot(point_time_length_S) +
  aes(x = reorder(island,population), fill = population, weight = Sdif_length) +
  geom_bar(position = "dodge") +
  scale_fill_hue(direction = 1) +
  theme_minimal() + coord_flip() +
  theme(legend.position = "none") + xlab("Populations within Island") +
  ggtitle("Selection differential All mericarps - Uneaten mericarps. Mericarp Length") +
  ylab("Selection differential")


## Width ####
point_time_width <- select(point_time, c(1:7), width, eaten)
point_time_width <- na.omit(point_time_width)
point_time_width <- point_time_width %>%  
  summarise_each(funs(width_mean =mean,
                      width_var = var,
                      width_se = sd(.)/sqrt(n()),
                      width_n = length,
  ), width)

### S*estimates ####
point_time_width_S <- pivot_wider(point_time_width, names_from = eaten,
                                   values_from = c(5:8))
point_time_width_S$S_width<- (point_time_width_S$width_mean_0 - point_time_width_S$width_mean_1)


## Depth ####
point_time_depth <- select(point_time, c(1:7), depth, eaten)
point_time_depth <- na.omit(point_time_depth)
point_time_depth <- point_time_depth %>%  
  summarise_each(funs(depth_mean = mean,
                      depth_var = var,
                      depth_se = sd(.)/sqrt(n()),
                      depth_n = length,
  ), depth)

### S*estimates ####
point_time_depth_S <- pivot_wider(point_time_depth, names_from = eaten,
                                  values_from = c(5:8))
point_time_depth_S$S_depth<- (point_time_depth_S$depth_mean_0 - point_time_depth_S$depth_mean_1)


## Longest spine ####
point_time_longest_spine <- select(point_time, c(1:7), longest_spine, eaten)
point_time_longest_spine <- na.omit(point_time_longest_spine)
point_time_longest_spine <- point_time_longest_spine %>%  
  summarise_each(funs(longest_spine_mean = mean,
                      longest_spine_var = var,
                      longest_spine_se = sd(.)/sqrt(n()),
                      longest_spine_n = length,
  ), longest_spine)

### S*estimates ####
point_time_longest_spine_S <- pivot_wider(point_time_longest_spine, names_from = eaten,
                                  values_from = c(5:8))
point_time_longest_spine_S$S_longest_spine<- (point_time_longest_spine_S$longest_spine_mean_0 - point_time_longest_spine_S$longest_spine_mean_1)


## Spine tip distance ####
point_time_tip_distance <- select(point_time, c(1:7), spine_tip_distance, eaten)
point_time_tip_distance <- na.omit(point_time_tip_distance)
point_time_tip_distance <- point_time_tip_distance %>%  
  summarise_each(funs(tip_distance_mean = mean,
                      tip_distance_var = var,
                      tip_distance_se = sd(.)/sqrt(n()),
                      tip_distance_n = length,
  ), spine_tip_distance)

### S*estimates ####
point_time_tip_distance_S <- pivot_wider(point_time_tip_distance, names_from = eaten,
                                          values_from = c(5:8))
point_time_tip_distance_S$S_tip_distance<- (point_time_tip_distance_S$tip_distance_mean_0 - point_time_tip_distance_S$tip_distance_mean_1)


# Plot per island ####

# This plots are per trait and it shows the S estimates differences of uneaten - eaten mericarps
# This can be useful for later, the models could use these estimates.
# It shows boxplots per island with all years combined and another series of boxplots with island per year.

## Length ####
point_time_length_S %>%
 filter(!(island %in% "Seymour.Norte")) %>%
 ggplot() +
 aes(x = island, y = S_length, fill = island) +
 geom_boxplot(shape = "circle") +
 scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
 San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Islands", 
 y = "S* estimates", title = "Length S* Estimates", subtitle = "S* Estimates: mean (uneaten - eaten)", 
 fill = "Islands") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)


## Width ####

point_time_width_S %>%
 filter(!(island %in% "Seymour.Norte")) %>%
 ggplot() +
 aes(x = island, y = S_width, fill = island) +
 geom_boxplot(shape = "circle") +
 scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Islands", 
 y = "S* estimate", title = "Width S* Estimates", subtitle = "S* Estimates: mean (uneaten - eaten)", 
 fill = "Islands") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 1)


## Depth ####
# Depth was not measured in 2015 and 2016 only from 2017 onward

ggplot(point_time_depth_S) +
 aes(x = island, y = S_depth, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Islands", y = "S* estimates", title = "Depth S* Estimates", 
 subtitle = "S* Estimates: mean (uneaten - eaten)", fill = "Islands") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 1)

## Longest Spine ####

point_time_longest_spine_S %>%
 filter(!(island %in% c("Seymour.Norte"))) %>%
 ggplot() +
 aes(x = island, y = S_longest_spine, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Islands", y = "S* estimate", title = "Longest Spine S* Estimates per island", 
 subtitle = "S* estimates mean (uneaten - eaten)") +
 theme_minimal()  + geom_hline(yintercept = 0, col = "#333333", size = 0.6)


## Spine tip distance ####
point_time_tip_distance_S %>%
 filter(!(island %in% c("Baltra", "Espanola", "Seymour.Norte"))) %>%
 ggplot() +
 aes(x = island, y = S_tip_distance, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Islands", y = "S* estimate", title = "Spine tip distance S* Estimates per island", 
 subtitle = "S* estimates mean (uneaten - eaten)", fill = "Islands") +
 theme_minimal()  + geom_hline(yintercept = 0, col = "#333333", size = 0.6)

# Plots per island per year ####

## Length ####
point_time_length_S %>%
 filter(!(island %in% c("Baltra", "Espanola", "Seymour.Norte"))) %>%
 ggplot() +
 aes(x = year, y = S_length, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Years", y = "S* Estimate", title = "Length S* Estimates", 
 subtitle = "S* estimates: mean (uneaten-eaten)", fill = "Islands") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(island)) + geom_hline(yintercept = 0, col = "#333333", size = 0.6)


## Width ####
point_time_width_S %>%
 filter(!(island %in% c("Baltra", "Espanola", "Seymour.Norte"))) %>%
 ggplot() +
 aes(x = year, y = S_width, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Years", y = "S* Estimate", title = "Width S* Estimates", 
 subtitle = "S* estimates: mean (uneaten-eaten)", fill = "Islands") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(island)) + geom_hline(yintercept = 0, col = "#333333", size = 0.6)


## Depth ####
point_time_depth_S %>%
  filter(!(island %in% c("Baltra", "Espanola", "Seymour.Norte"))) %>%
  ggplot() +
  aes(x = year, y = S_depth, fill = island) +
  geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
  labs(x = "Years", y = "S* Estimate", title = "Depth S* Estimates", 
       subtitle = "S* estimates: mean (uneaten-eaten)", fill = "Islands") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(island)) + geom_hline(yintercept = 0, col = "#333333", size = 0.6)


## Longest spine ####
point_time_longest_spine_S %>%
 filter(!(year %in% "2015")) %>%
 filter(!(island %in% c("Baltra", "Espanola", 
"Seymour.Norte"))) %>%
 ggplot() +
 aes(x = year, y = S_longest_spine, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Years", y = "S* Estimate", title = "Longest spine S* Estimates", 
 subtitle = "S* estimates: mean (uneaten-eaten)", fill = "Islands") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(island)) + geom_hline(yintercept = 0, col = "#333333", size = 0.6)


## Tip distance ####
point_time_tip_distance_S %>%
 filter(year %in% c("2017", "2018")) %>%
 filter(!(island %in% c("Baltra", "Espanola", 
"Seymour.Norte"))) %>%
 ggplot() +
 aes(x = year, y = S_tip_distance, fill = island) +
 geom_boxplot(shape = "circle") +
  scale_fill_manual(values = c(Baltra = "#F5793A", Espanola = "#A95AA1", Floreana = "#85C0F9", Isabela = "#0F2080", 
                               San.Cristobal = "#BDB8AD", Santa.Cruz = "#ABC3C9")) +
 labs(x = "Years", y = "S* Estimates", title = "Spine tip distance S* estimates", 
 subtitle = "S* estimates mean (uneaten-eaten)") +
 theme_minimal() +
 theme(legend.position = "none") +
 facet_wrap(vars(island)) + geom_hline(yintercept = 0, col = "#333333", size = 0.6)

