# Experimental plots data analysis ####
# By: Daniel Reyes Corral
# Date: 11/28/2023
# Load the datasets from the experimental plot folder
# 
# The experimental plots are a series of plots where we placed Tribulus mericarps
# in the field on locations without Tribulus, these mericarps grew in plots of
# XX by YY and were separated by size groups (large, small and two-spines)
# The two spines group are naturally occuring in the source population at a lower
# frequency (12 parental plants).
#  

# Data loading ####

exp_plot_set <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Experimental plot/Experimental_plots.csv")

# This dataset is already filtered, we removed the average group treatment


# Data prepping ####
# Year, island, population and survival are going to be grouping factors
exp_plot_set <- exp_plot_set %>% mutate_at(vars(year, parcel, treatment, eaten, germinated), list(factor))


# Plots ####
# The main plot now is exploratory using length and comparing between
# Treatments and years (start and end of experiment)

ggplot(exp_plot_set) +
  aes(x = length, fill = year, group = year) +
  geom_density(adjust = 1L) +
  scale_fill_brewer(palette = "Dark2", 
                    direction = 1) +
  labs(x = "Length (mm)", y = "Density", title = "Mericarp Length", fill = "Year") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15L, face = "bold")) +
  facet_grid(vars(treatment), 
             vars())


ggplot(exp_plot_set) +
 aes(x = length, fill = treatment, group = treatment) +
 geom_density(adjust = 1L) +
 scale_fill_brewer(palette = "Dark2", direction = 1) +
 labs(x = "Length (mm)", y = "Density", title = "Mericarp Length", 
 fill = "Plot Group") +
 theme_minimal() +
 theme(plot.title = element_text(size = 15L, face = "bold")) +
 facet_grid(vars(year), vars())


exp_plot_set %>%
 filter(!(treatment %in% "Two_spines")) %>%
 ggplot() +
 aes(x = length, fill = treatment) +
 geom_density(adjust = 1L) +
 scale_fill_brewer(palette = "Dark2", 
 direction = 1) +
 theme_minimal() +
 facet_grid(vars(year), vars())


exp_plot_set %>%
 #filter(!(treatment %in% "Two_spines")) %>%
 ggplot() +
 aes(x = longest_spine, fill = treatment) +
 geom_density(adjust = 1L) +
 scale_fill_brewer(palette = "Dark2", 
 direction = 1) +
 labs(x = "Spine Tip Distance", y = "Density", title = "Spine length", fill = "Group") +
 theme_minimal() +
 theme(plot.title = element_text(size = 15L, face = "bold")) +
 facet_grid(vars(year), 
 vars())

