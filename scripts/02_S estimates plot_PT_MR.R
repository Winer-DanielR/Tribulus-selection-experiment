################Tribulus Selection experiments Dataset ##############
# Goal: Compare the two methods of natural selection estimates. 
# Point in time (PT) and Mark Recapture (MR).
# By Daniel Reyes Corral

# The methods comparinson was made using a specific time 2019 where the two methods
# were implemented

####### Goal: Plot the S estimates show comparisons between methods, traits and islands ####

# I used the dataset created using the first script: S_estimates.csv

S_summary_dataset <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/S_estimates.csv")
str(S_summary_dataset)
S_summary_dataset <- S_summary_dataset %>% mutate_at(vars(island, method), list(factor))

# Select S estimates from the dataset
S_estimates <- select(S_summary_dataset, method, island, c(60:63))

# Arrange the estimates per trait and then per method
# This creates a new column (name) with the traits and another (value) with the S
# estimates
S_estimates <- pivot_longer(S_estimates,
                                  cols = c(3:6),
                                  names_prefix = "S_")
# Then change the names accordingly
S_estimates <- dplyr::rename(S_estimates, trait = name,
                                     S = value)
# I use the pivot wider function to create two columns per method Point in time and Mark recapture
S_estimates <- pivot_wider(S_estimates, names_from = method, values_from = S)

# I then change the names to be more coding friendly
S_estimates <- dplyr::rename(S_estimates, Mark_Recap_S = "Mark Recapture",
                             P_Time_S = "Point in time")

S_estimates <- S_estimates %>% mutate_at(vars(island, trait), list(factor))
# Select estimated CI from the dataset
Low_CI_estimates <- select(S_summary_dataset, method, island, c(64,66,68,70))

Low_CI_estimates <- pivot_longer(Low_CI_estimates,
                             cols = c(3:6),
                             names_prefix = "low_CI_")
# Then change the names accordingly
Low_CI_estimates <- dplyr::rename(Low_CI_estimates, trait = name,
                             lower_CI = value)
# I use the pivot wider function to create two columns per method Point in time and Mark recapture
Low_CI_estimates <- pivot_wider(Low_CI_estimates, names_from = method, values_from = lower_CI)

# I then change the names to be more coding friendly
Low_CI_estimates <- dplyr::rename(Low_CI_estimates, Mark_Recap_Low_CI = "Mark Recapture",
                             P_Time_Low_CI = "Point in time")

# Select estimated CI from the dataset
Upper_CI_estimates <- select(S_summary_dataset, method, island, c(65,67,69,71))

Upper_CI_estimates <- pivot_longer(Upper_CI_estimates,
                                 cols = c(3:6),
                                 names_prefix = "upper_CI_")
# Then change the names accordingly
Upper_CI_estimates <- dplyr::rename(Upper_CI_estimates, trait = name,
                                  upper_CI = value)
# I use the pivot wider function to create two columns per method Point in time and Mark recapture
Upper_CI_estimates <- pivot_wider(Upper_CI_estimates, names_from = method, values_from = upper_CI)

# I then change the names to be more coding friendly
Upper_CI_estimates <- dplyr::rename(Upper_CI_estimates, Mark_Recap_Upper_CI = "Mark Recapture",
                                  P_Time_Upper_CI = "Point in time")

CI_estimates <- cbind(Low_CI_estimates,Upper_CI_estimates)
CI_estimates <- select(CI_estimates, !c(5,6))

S_CI_estimates <- cbind(S_estimates,CI_estimates)
S_CI_estimates <- select(S_CI_estimates, !c(5,6))

# Plots ####
# Using the dataset and data prep I create the figure comparing both methods
# I used two methods to create the plots one with Base R the other with ggplot

## Base R plot ####
color_easy <- c("red", "blue", "black")[S_estimates$island]
pcheasy <- c(21,22,23,24)[S_estimates$trait]

plot_estimates <- plot(x=S_estimates$P_Time_S, y=S_estimates$Mark_Recap_S, main = "S* Estimates (mean non eaten - mean eaten)",
      xlab = "Point in Time S* estimates", ylab =  "Mark Recapture S* estimates", 
      col = color_easy, pch = pcheasy, bg = color_easy, cex = 1.5)

legend("bottomright", legend = c("depth", "length", "longest spine", "width"), pch = c(16,15,18,17), cex = 1.1)
legend("topright", legend = c("Floreana", "Isabela", "Santa Cruz"), pch = c(16),
       col = c("red", "blue", "black"), cex = 1.2)
abline(v=0, h=0, col = "black")
abline(a=0, b=1, col = "red")

### The base R plot is a draft ####


## Ggplot ####
# This plot uses the estimated CI calculated per island for both methods.
# The larger CI corresponds to the mark recapture estimates which are larger!
# However, this plot shows that both methods are infering the same
# which is that selection is positive for most traits
# Caveat: lower spines and spine position is not included ####
ggplot(S_CI_estimates) +
  aes(x = P_Time_S, y = Mark_Recap_S, colour = island) +
  geom_point(size = 0) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "S* Estimates Point in Time",
    y = "S* Estimates Mark Recapture",
    title = "S* Estimates (Mean uneaten - Mean eaten)",
    label = "Trait",
    color = "Island"
  ) +
  theme_classic() +
  geom_hline(yintercept = 0, col = "#333333", size = 0.5) +
  geom_vline(xintercept = 0, col = "#333333", size = 0.5) +
  geom_point(aes(shape = trait), size = 4) +
  geom_linerange(xmin = S_CI_estimates$P_Time_Low_CI,
                 xmax = S_CI_estimates$P_Time_Upper_CI,
                 size = 1) +
  geom_linerange(ymin = S_CI_estimates$Mark_Recap_Low_CI,
                 ymax = S_CI_estimates$Mark_Recap_Upper_CI,
                 size = 1)
  
                 
            
  