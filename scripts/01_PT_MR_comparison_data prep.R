################Tribulus Selection experiments Dataset##############
# Goal: Compare the two methods of natural selection estimates. 
# Point in time (PT) and Mark Recapture (MR).
# By Daniel Reyes Corral

##### Data preparation ####
# Objective: Estimate the mean of survivors (non-eaten/present) 
# and dead (eaten/missing) mericarps per trait.
# Objective: Estimate the difference of the means per method and plot the means.
# Product: Created a new dataset called S_estimates which have the calculated estimates

##### Insert the data

dataset <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/PT and MR comparison.csv")
dataset <- as_tibble(dataset)
dataset

# Check the dataset
names(dataset) #Check the titles of the columns
head(dataset) #Check the first 6 rows of the table
str(dataset)

# Changed variables to factors
dataset <- dataset %>% mutate_at(vars(method,
island, lower_spine, Present, eaten,
seed_position_1, seed_position_2,
seed_position_3, seed_position_4,
seed_position_5, seed_position_6,
germinated, germinated_position_1,
germinated_position_2, germinated_position_3,
germinated_position_4, germinated_position_5,
germinated_position_6), list(factor))

############################# Select and filter ############################
# This selects for all mericarp traits and methods.
dataset_select <- select(dataset, method, 
                         year, 
                         island, 
                         mericarp, 
                         length, 
                         width,
                         depth,
                         longest_spine,
                         eaten)
# dataset_PT <- filter(dataset_select, method == "Point in time")
# dataset_PT <- as.tibble(dataset_PT)
dataset_select <- group_by(dataset_select, method, island, eaten)
dataset_summary <- dataset_select %>%  
  summarise_each(funs(mean,
            sd,
            var,
            se = sd(.)/sqrt(n()),
            n = length,
            ), length, 
            width,
            depth,
            longest_spine)
# This summary estimates the ns of all traits wich is redundant
# Select all the columns but only one that estimates the length
dataset_summary <- select(dataset_summary, c(1:20))
dataset_summary <- dplyr::rename(dataset_summary, n = length_n)

dataset_var <- select(dataset_summary, eaten,
                      length_var,
                      width_var,
                      depth_var,
                      longest_spine_var,
                      n)

dataset_var$df <- dataset_var$n - 1
dataset_var_v <- dataset_var %>%
  summarise_each(funs((.)*(df)), length_var,
                 width_var,
                 depth_var,
                 longest_spine_var)
dataset_var_v <- dplyr::rename(dataset_var_v, length_v = length_var,
                               width_v = width_var,
                               depth_v = depth_var,
                               longest_spine_v = longest_spine_var)

dataset_summary_v <- cbind(dataset_summary, dataset_var_v)
dataset_summary_v <- dataset_summary_v %>% select(-method...21, -island...22)
dataset_summary_v <- dplyr::rename(dataset_summary_v, method = method...1,
                                   island = island...2)

# Pivot table to make uneaten and eaten mericarps
dataset_summary_v <- group_by(dataset_summary_v, method, island, eaten)
dataset_summary_v <- pivot_wider(dataset_summary_v, names_from = eaten,
                                 values_from = c(4:24))

# Convert NA to zero
dataset_summary_v[is.na(dataset_summary_v)] = 0


# Sum of V uneaten and eaten per trait
dataset_summary_v$length_v_total <- dataset_summary_v$length_v_0 + 
  dataset_summary_v$length_v_1
dataset_summary_v$width_v_total <- dataset_summary_v$width_v_0 + 
  dataset_summary_v$width_v_1
dataset_summary_v$depth_v_total <- dataset_summary_v$depth_v_0 + 
  dataset_summary_v$depth_v_1
dataset_summary_v$longest_spine_v_total <- dataset_summary_v$longest_spine_v_0 + 
  dataset_summary_v$longest_spine_v_1

# Sum on uneaten and eaten n's (n1+n2-2)
dataset_summary_v$df2 <- (dataset_summary_v$n_0 + dataset_summary_v$n_1 - 2)

# Calculate pooled variance per trait sp (v_total/df2)
dataset_summary_v$length_sp <- dataset_summary_v$length_v_total/dataset_summary_v$df2
dataset_summary_v$width_sp <- dataset_summary_v$width_v_total/dataset_summary_v$df2
dataset_summary_v$depth_sp <- dataset_summary_v$depth_v_total/dataset_summary_v$df2
dataset_summary_v$longest_spine_sp <- dataset_summary_v$longest_spine_v_total/dataset_summary_v$df2

# Calculate margin for CIs using the poobled variance per trait
# Caulculate margin df n0 + n1 - 1
dataset_summary_v$df_margin <- (dataset_summary_v$n_0+dataset_summary_v$n_1 - 1)
# Margins per trait
dataset_summary_v$margin_length <- qt(0.975,dataset_summary_v$df_margin)*
  sqrt((dataset_summary_v$length_sp/dataset_summary_v$n_0) + (dataset_summary_v$length_sp/dataset_summary_v$n_1))

dataset_summary_v$margin_width <- qt(0.975,dataset_summary_v$df_margin)*
  sqrt((dataset_summary_v$width_sp/dataset_summary_v$n_0) + (dataset_summary_v$width_sp/dataset_summary_v$n_1))

dataset_summary_v$margin_depth <- qt(0.975,dataset_summary_v$df_margin)*
  sqrt((dataset_summary_v$depth_sp/dataset_summary_v$n_0) + (dataset_summary_v$depth_sp/dataset_summary_v$n_1))

dataset_summary_v$margin_longest_spine <- qt(0.975,dataset_summary_v$df_margin)*
  sqrt((dataset_summary_v$longest_spine_sp/dataset_summary_v$n_0) + (dataset_summary_v$longest_spine_sp/dataset_summary_v$n_1))

# Calculate S* estimates per trait

dataset_summary_v$S_length <- (dataset_summary_v$length_mean_0 - dataset_summary_v$length_mean_1)

dataset_summary_v$S_width <- (dataset_summary_v$width_mean_0 - dataset_summary_v$width_mean_1)

dataset_summary_v$S_depth <- (dataset_summary_v$depth_mean_0 - dataset_summary_v$depth_mean_1)

dataset_summary_v$S_longest_spine <- (dataset_summary_v$longest_spine_mean_0 - dataset_summary_v$longest_spine_mean_1)

# Calculate confidence intervals for the mean differences (S) per traits

dataset_summary_v$low_CI_length <- dataset_summary_v$S_length - dataset_summary_v$margin_length 
dataset_summary_v$upper_CI_length <- dataset_summary_v$S_length + dataset_summary_v$margin_length 

dataset_summary_v$low_CI_width <- dataset_summary_v$S_width - dataset_summary_v$margin_width 
dataset_summary_v$upper_CI_width <- dataset_summary_v$S_width + dataset_summary_v$margin_width 

dataset_summary_v$low_CI_depth <- dataset_summary_v$S_depth - dataset_summary_v$margin_depth 
dataset_summary_v$upper_CI_depth <- dataset_summary_v$S_depth + dataset_summary_v$margin_depth 

dataset_summary_v$low_CI_longest_spine <- dataset_summary_v$S_longest_spine - dataset_summary_v$margin_longest_spine 
dataset_summary_v$upper_CI_longest_spine <- dataset_summary_v$S_longest_spine + dataset_summary_v$margin_longest_spine 


##### Write .csv file of S* estimates ####
S_estimates <- dataset_summary_v
write.csv(S_estimates, "S_estimates.csv")

