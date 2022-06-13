#### Plot comparing selection estimates per trait: Data preparation ####
# By: Daniel Reyes Corral
# Date: 06/10/2021
# In this script I want to group mericarps per year, island and population
# Then calculate the mean of all mericarps and calculate the mean of surviving mericarps
# PLot these estimates between two traits.

# Load the point in time dataset
point_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors
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

# Group by year, island and populations
pt_mean_survived <- group_by(point_time, year, island, population, eaten) # This groups the dataset up to eaten. Use for estimates of surviving mericarps
pt_mean_all <- group_by(point_time, year, island, population) # This groups them by population. Use for estimates of all mericarps


# Select per traits to check and remove NAs
## Length ####
### Survived mericarps (also eaten) ####
pt_length_eaten <- select(pt_mean_survived, c(1:8), eaten)
pt_length_eaten <- na.omit(pt_length_eaten)
pt_length_eaten <- pt_length_eaten %>%  
  summarise_each(funs(survived = mean,
                      #length_var_surv = var,
                      #length_se_surv = sd(.)/sqrt(n()),
                      #length_n_surv = length,
  ), length)

# Extract only surviving mericarps
pt_length_survived <- filter(pt_length_eaten, eaten == "0")

### All mericarps mean ####
pt_length_all <- select(pt_mean_all, c(1:8))
pt_length_all <- na.omit(pt_length_all)
pt_length_all <- pt_length_all %>%
  summarise_each(funs(all = mean,
                      #length_var_all = var,
                      #length_se_all = sd(.)/sqrt(n()),
                      #length_n_all = length,
  ), length)

### Join datasets ####
pt_length <- left_join(pt_length_all, pt_length_survived)
pt_length <- select(pt_length, !c(5))

length_pivot <- pivot_longer(pt_length, cols = c(4,5),
                             names_to = "survival",
                             values_to = "length_mean")




# This groups them by population per island per year

## Depth ####
### Survived mericarps (also eaten) ####
pt_depth_eaten <- select(pt_mean_survived, c(1:7,10), eaten)
pt_depth_eaten <- na.omit(pt_depth_eaten)
pt_depth_eaten <- pt_depth_eaten %>%  
  summarise_each(funs(survived = mean,
                      #depth_var_surv = var,
                      #depth_se_surv = sd(.)/sqrt(n()),
                      #depth_n_surv = length,
  ), depth)

# Extract only surviving mericarps
pt_depth_survived <- filter(pt_depth_eaten, eaten == "0")

### All mericarps mean ####
pt_depth_all <- select(pt_mean_all, c(1:7, 10))
pt_depth_all <- na.omit(pt_depth_all)
pt_depth_all <- pt_depth_all %>%
  summarise_each(funs(all = mean,
                      #depth_var_all = var,
                      #depth_se_all = sd(.)/sqrt(n()),
                      #depth_n_all = length,
  ), depth)

### Join datasets ####
pt_depth <- left_join(pt_depth_all, pt_depth_survived)

pt_depth <- select(pt_depth, !c(5))

depth_pivot <- pivot_longer(pt_depth, cols = c(4,5),
                             names_to = "survival",
                             values_to = "depth_mean")



length_depth <- left_join(length_pivot, depth_pivot)
length_depth <- na.omit(length_depth)


ggplot(length_depth) +
  aes(
    x = length_mean,
    y = depth_mean,
    group = population,
    colour = population,
    shape = survival
  ) +
  geom_point(size = 3L, show.legend = T) +
  geom_path(aes(group = population), 
            color = "black", 
            arrow = arrow(length = unit(0.3,"cm")), size = 0.7) +
  #scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Length (mm)",
    y = "Depth (mm)",
    title = "Length - Depth Selection Estimates Plot",
    caption = "Means per Population,
    per Island or all mericarps and survived mericarps",
    color = "Populations",
    shape = "Survival"
  ) +
  #geom_text_repel(aes(label = population)) +
  ggthemes::theme_few() + 
  facet_grid(rows = vars(island), cols = vars(year),
             scales = "free")
  

