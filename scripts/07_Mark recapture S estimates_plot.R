#### Mark Recapture S* estimates per year, island and population ####
# By: Daniel Reyes Corral
# Once the one mark recapture data was created I was able to calculate S* estimates per island
# year, treatment, size, etc.

# Load the point in time dataset
mark_recapture <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Mark Recapture S.csv")
mark_recapture <- as_tibble(mark_recapture)
mark_recapture <- dplyr::rename(mark_recapture, eaten = eaten_sum, eaten1 = eaten_sum1,
                                number = ...1)

# Changed variables to factors
mark_recapture <- mark_recapture %>% mutate_at(vars(c(2:6,13:25)), list(factor))
str(mark_recapture)

# Here I divide the dataset into two groups one that uses eaten (assumes that NAs survived)
# The other one uses eaten1 (assumes that NAs did NOT survived)

mark_recapture_0 <- group_by(mark_recapture, island,
                           treatment,
                           size,
                           color,
                           year,
                           eaten)
mark_recapture_1 <- group_by(mark_recapture, island,
                             treatment,
                             size,
                             color,
                             year,
                             eaten1)

# Select per traits to check and remove NAs
## Length 0 ####
MC_length_0 <- select(mark_recapture_0, length)
MC_length_0 <- na.omit(MC_length_0)
MC_length_0 <- MC_length_0 %>%  
  summarise_each(funs(length_mean = mean,
                      length_var = var,
                      length_se = sd(.)/sqrt(n()),
                      length_n = length,
  ), length)

## Length 1 ####
MC_length_1 <- select(mark_recapture_1, length)
MC_length_1 <- na.omit(MC_length_1)
MC_length_1 <- MC_length_1 %>%  
  summarise_each(funs(length_mean = mean,
                      length_var = var,
                      length_se = sd(.)/sqrt(n()),
                      length_n = length,
  ), length)

### S*estimates ####
MC_length_S <- pivot_wider(MC_length_0, names_from = eaten,
                                   values_from = c(7:10))

MC_length_S_1 <- pivot_wider(MC_length_1, names_from = eaten1, values_from = c(7:10))

# Need to change NAs to 0s
MC_length_S[is.na(MC_length_S)] = 0
MC_length_S_1[is.na(MC_length_S_1)] = 0

MC_length_S$S_length <- (MC_length_S$length_mean_0 - MC_length_S$length_mean_1)
MC_length_S_1$S_length_1 <- (MC_length_S_1$length_mean_0 - MC_length_S_1$length_mean_1)

MC_length_S$S_length_1 <- MC_length_S_1$S_length_1

## Width 0 ####
MC_width_0 <- select(mark_recapture_0, width)
MC_width_0 <- na.omit(MC_width_0)
MC_width_0 <- MC_width_0 %>%  
  summarise_each(funs(width_mean = mean,
                      width_var = var,
                      width_se = sd(.)/sqrt(n()),
                      width_n = length,
  ), width)

## Width 1 ####
MC_width_1 <- select(mark_recapture_1, width)
MC_width_1 <- na.omit(MC_width_1)
MC_width_1 <- MC_width_1 %>%  
  summarise_each(funs(width_mean = mean,
                      width_var = var,
                      width_se = sd(.)/sqrt(n()),
                      width_n = length,
  ), width)

### S*estimates ####
MC_width_S <- pivot_wider(MC_width_0, names_from = eaten,
                           values_from = c(7:10))

MC_width_S_1 <- pivot_wider(MC_width_1, names_from = eaten1, values_from = c(7:10))

# Need to change NAs to 0s
MC_width_S[is.na(MC_width_S)] = 0
MC_width_S_1[is.na(MC_width_S_1)] = 0

MC_width_S$S_width <- (MC_width_S$width_mean_0 - MC_width_S$width_mean_1)
MC_width_S_1$S_width_1 <- (MC_width_S_1$width_mean_0 - MC_width_S_1$width_mean_1)

MC_width_S$S_width_1 <- MC_width_S_1$S_width_1

## Depth 0 ####
MC_depth_0 <- select(mark_recapture_0, depth)
MC_depth_0 <- na.omit(MC_depth_0)
MC_depth_0 <- MC_depth_0 %>%  
  summarise_each(funs(depth_mean = mean,
                      depth_var = var,
                      depth_se = sd(.)/sqrt(n()),
                      depth_n = length,
  ), depth)

## Depth 1 ####
MC_depth_1 <- select(mark_recapture_1, depth)
MC_depth_1 <- na.omit(MC_depth_1)
MC_depth_1 <- MC_depth_1 %>%  
  summarise_each(funs(depth_mean = mean,
                      depth_var = var,
                      depth_se = sd(.)/sqrt(n()),
                      depth_n = length,
  ), depth)

### S*estimates ####
MC_depth_S <- pivot_wider(MC_depth_0, names_from = eaten,
                          values_from = c(7:10))

MC_depth_S_1 <- pivot_wider(MC_depth_1, names_from = eaten1, values_from = c(7:10))

# Need to change NAs to 0s
MC_depth_S[is.na(MC_depth_S)] = 0
MC_depth_S_1[is.na(MC_depth_S_1)] = 0

MC_depth_S$S_depth <- (MC_depth_S$depth_mean_0 - MC_depth_S$depth_mean_1)
MC_depth_S_1$S_depth_1 <- (MC_depth_S_1$depth_mean_0 - MC_depth_S_1$depth_mean_1)

MC_depth_S$S_depth_1 <- MC_depth_S_1$S_depth_1

## Longest spine 0 ####
MC_longest_spine_0 <- select(mark_recapture_0, longest_spine)
MC_longest_spine_0 <- na.omit(MC_longest_spine_0)
MC_longest_spine_0 <- MC_longest_spine_0 %>%  
  summarise_each(funs(longest_spine_mean = mean,
                      longest_spine_var = var,
                      longest_spine_se = sd(.)/sqrt(n()),
                      longest_spine_n = length,
  ), longest_spine)

## Longest spine 1 ####
MC_longest_spine_1 <- select(mark_recapture_1, longest_spine)
MC_longest_spine_1 <- na.omit(MC_longest_spine_1)
MC_longest_spine_1 <- MC_longest_spine_1 %>%  
  summarise_each(funs(longest_spine_mean = mean,
                      longest_spine_var = var,
                      longest_spine_se = sd(.)/sqrt(n()),
                      longest_spine_n = length,
  ), longest_spine)

### S*estimates ####
MC_longest_spine_S <- pivot_wider(MC_longest_spine_0, names_from = eaten,
                          values_from = c(7:10))

MC_longest_spine_S_1 <- pivot_wider(MC_longest_spine_1, names_from = eaten1, values_from = c(7:10))

# Need to change NAs to 0s
MC_longest_spine_S[is.na(MC_longest_spine_S)] = 0
MC_longest_spine_S_1[is.na(MC_longest_spine_S_1)] = 0

MC_longest_spine_S$S_longest_spine <- (MC_longest_spine_S$longest_spine_mean_0 - MC_longest_spine_S$longest_spine_mean_1)
MC_longest_spine_S_1$S_longest_spine_1 <- (MC_longest_spine_S_1$longest_spine_mean_0 - MC_longest_spine_S_1$longest_spine_mean_1)

MC_longest_spine_S$S_longest_spine_1 <- MC_longest_spine_S_1$S_longest_spine_1


## Spine tip distance 0 ####
MC_tip_distance_0 <- select(mark_recapture_0, spine_tip_distance)
MC_tip_distance_0 <- na.omit(MC_tip_distance_0)
MC_tip_distance_0 <- MC_tip_distance_0 %>%  
  summarise_each(funs(spine_tip_dist_mean = mean,
                      spine_tip_dist_var = var,
                      spine_tip_dist_se = sd(.)/sqrt(n()),
                      spine_tip_dist_n = length,
  ), spine_tip_distance)

## Spine tip distance 1 ####
MC_tip_distance_1 <- select(mark_recapture_1, spine_tip_distance)
MC_tip_distance_1 <- na.omit(MC_tip_distance_1)
MC_tip_distance_1 <- MC_tip_distance_1 %>%  
  summarise_each(funs(spine_tip_dist_mean = mean,
                      spine_tip_dist_var = var,
                      spine_tip_dist_se = sd(.)/sqrt(n()),
                      spine_tip_dist_n = length,
  ), spine_tip_distance)

### S*estimates ####
MC_tip_distance_S <- pivot_wider(MC_tip_distance_0, names_from = eaten,
                                  values_from = c(7:10))

MC_tip_distance_S_1 <- pivot_wider(MC_tip_distance_1, names_from = eaten1, values_from = c(7:10))

# Need to change NAs to 0s
MC_tip_distance_S[is.na(MC_tip_distance_S)] = 0
MC_tip_distance_S_1[is.na(MC_tip_distance_S_1)] = 0

MC_tip_distance_S$S_spine_tip_distance <- (MC_tip_distance_S$spine_tip_dist_mean_0 - MC_tip_distance_S$spine_tip_dist_mean_1)
MC_tip_distance_S_1$S_spine_tip_distance_1 <- (MC_tip_distance_S_1$spine_tip_dist_mean_0 - MC_tip_distance_S_1$spine_tip_dist_mean_1)

MC_tip_distance_S$S_spine_tip_distance_1 <- MC_tip_distance_S_1$S_spine_tip_distance_1



# Plots per trait ####
## Length ####
ggplot(MC_length_S) +
 aes(x = island, y = S_length, fill = treatment) +
 geom_boxplot(shape = "circle") +
 scale_fill_brewer(palette = "Dark2", direction = 1) +
 labs(x = "Islands", y = "S* estimates", title = "Length S* Estimates per Island-treatment", 
 subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = 0, uneaten)", 
 fill = "Mericarp groups") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_length_S) +
 aes(x = island, y = S_length_1, fill = treatment) +
 geom_boxplot(shape = "circle") +
 scale_fill_brewer(palette = "Dark2", direction = 1) +
 labs(x = "Islands", y = "S* estimates", title = "Length S* Estimates per Island-treatment", 
 subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", 
 fill = "Mericarp groups") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)


ggplot(MC_length_S) +
 aes(x = island, y = S_length, fill = size) +
 geom_boxplot(shape = "circle") +
 scale_fill_brewer(palette = "Dark2", 
 direction = 1) +
 labs(x = "Islands", y = "S* estimates", title = "Length S* Estimates per size", 
 subtitle = "S* estimates", fill = "Mericarp Size") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_length_S) +
 aes(x = island, y = S_length_1, fill = size) +
 geom_boxplot(shape = "circle") +
 scale_fill_brewer(palette = "Dark2", direction = 1) +
 labs(x = "Islands", y = "S* estimates", title = "Length S* Estimates per size", 
 subtitle = "S* estimates", fill = "Mericarp Size") +
 theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)


## Width ####
ggplot(MC_width_S) +
  aes(x = island, y = S_width, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Width S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = 0, uneaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_width_S_1) +
  aes(x = island, y = S_width_1, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Width S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_width_S) +
  aes(x = island, y = S_width, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Width S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = , uneaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_width_S) +
  aes(x = island, y = S_width_1, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Width S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

## Depth ####
ggplot(MC_depth_S) +
  aes(x = island, y = S_depth, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Depth S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = 0, uneaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_depth_S_1) +
  aes(x = island, y = S_depth_1, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Depth S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_depth_S) +
  aes(x = island, y = S_depth, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Depth S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = , uneaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_depth_S) +
  aes(x = island, y = S_depth_1, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Depth S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

## Longest spine ####
ggplot(MC_longest_spine_S) +
  aes(x = island, y = S_longest_spine, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Longest spine S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = 0, uneaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_longest_spine_S_1) +
  aes(x = island, y = S_longest_spine_1, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Longest spine S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_longest_spine_S) +
  aes(x = island, y = S_longest_spine, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Longest spine S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = , uneaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_longest_spine_S) +
  aes(x = island, y = S_longest_spine_1, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Longest spine S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

## Spine tip distance ####
ggplot(MC_tip_distance_S) +
  aes(x = island, y = S_spine_tip_distance, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Spine tip distance S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = 0, uneaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_tip_distance_S_1) +
  aes(x = island, y = S_spine_tip_distance_1, fill = treatment) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Longest spine S* Estimates per Island-treatment", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", 
       fill = "Mericarp groups") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_tip_distance_S) +
  aes(x = island, y = S_spine_tip_distance, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Spine tip distance S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps survived (NA = , uneaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)

ggplot(MC_tip_distance_S) +
  aes(x = island, y = S_spine_tip_distance_1, fill = size) +
  geom_boxplot(shape = "circle") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(x = "Islands", y = "S* estimates", title = "Spine tip distance S* Estimates per size", 
       subtitle = "S* estimates (mean uneaten - mean eaten) Estimates calculated assuming missing mericarps NOT survived (NA = 1, eaten)", fill = "Mericarp Size") +
  theme_classic() + geom_hline(yintercept = 0, col = "#333333", size = 0.5)
