
################Tribulus Selection experiments Dataset##############

### Goal: Compare the two methods of natural selection estimates. Point in time (PT) and Mark Recapture (MR).
### By Daniel Reyes Corral

##### Data wrangling ####
# Objective: Estimate the mean of survivors (non-eaten/present) and dead (eaten/missing) mericarps per trait.
# Objective: Estimate the difference of the means per method and plot the means.

##### Insert the data

dataset <- read_csv("C:/Users/Daniel/Documents/R/Tribulus/Tribulus Selection experiment/Data/Processed/PT and MR comparison.csv")
dataset <- as_tibble(dataset)
dataset

# Check the dataset
names(dataset) #Check the titles of the columns
head(dataset) #Check the first 6 rows of the table
str(dataset)

# Changed variables to factors
dataset <- dataset %>% mutate_at(vars(method,
year, island, lower_spine, Present, eaten,
seed_position_1, seed_position_2,
seed_position_3, seed_position_4,
seed_position_5, seed_position_6,
germinated, germinated_position_1,
germinated_position_2, germinated_position_3,
germinated_position_4, germinated_position_5,
germinated_position_6), list(factor))

############################# Select and filter ############################

#### Length ####
length_select <- select(dataset, method, year, island, mericarp, length, Present, eaten)

  ## Length Point in time ##### 
length_PT <- filter(length_select, method == "Point in time") 
PT_length_island <- group_by(length_PT, island, eaten)

# Create a summary of the eaten and uneaten column
PT_length_summary <- PT_length_island %>% summarise_each(funs(mean), length) # Add this to estimate the rest of the stats, sd, se=sd(.)/sqrt(n())

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
PT_length_pivot <- pivot_wider(PT_length_summary, names_from = eaten, values_from = length) 

# PT Add S* estimate ####
PT_length_S <- transmute(PT_length_pivot, non_eaten = `0`, eaten = `1`, S_PT = `0` - `1`, trait = "length")
PT_length_S1 <- select(PT_length_S, island, S_PT)


  ## Length Mark recapture ####
length_MR <- filter(length_select, method == "Mark Recapture")
MR_length_island <- group_by(length_MR, island, eaten)
#MR_length_island_present <- group_by(length_MR, island, Present)

# Create a summary of the eaten and uneaten column
MR_length_summary <- MR_length_island %>% summarise_each(funs(mean), length)
#MR_length_mean_present <- MR_length_island_present %>% summarise_each(funs(mean), length)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
MR_length_pivot <- pivot_wider(MR_length_summary, names_from = eaten, values_from = length)

# MR Add S* estimate ####
MR_lentgth_S <- transmute(MR_length_pivot, non_eaten = `0`, eaten = `1`, S_MR = `0` - `1`, trait = "length")
MR_length_S1 <- select(MR_lentgth_S, island, S_MR)

##### Length methods binding
length_S_methods <- bind_cols(PT_length_S1, MR_length_S1)
length_S_methods <- transmute(length_S_methods, island = island...1, S_PT, S_MR,trait = "length")

################################################################################

#### Width ####
width_select <- select(dataset, method, year, island, mericarp, width, Present, eaten)

## Width Point in time ##### 
width_PT <- filter(width_select, method == "Point in time") 
PT_width_island <- group_by(width_PT, island, eaten)

# Create a summary of the eaten and uneaten column
PT_width_summary <- PT_width_island %>% summarise_each(funs(mean), width)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
PT_width_pivot <- pivot_wider(PT_width_summary, names_from = eaten, values_from = width) 

# PT Add S* estimate ####
PT_width_S <- transmute(PT_width_pivot, non_eaten = `0`, eaten = `1`, S_PT = `0` - `1`, trait = "width")
PT_width_S1 <- select(PT_width_S, island, S_PT)

## Width Mark recapture ####
width_MR <- filter(width_select, method == "Mark Recapture")
MR_width_island <- group_by(width_MR, island, eaten)
#MR_width_island_present <- group_by(width_MR, island, Present)

# Create a summary of the eaten and uneaten column
MR_width_summary <- MR_width_island %>% summarise_each(funs(mean), width)
#MR_width_summary_present <- MR_width_island_present %>% summarise_each(funs(mean), width)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
MR_width_pivot <- pivot_wider(MR_width_summary, names_from = eaten, values_from = width)

# MR Add S* estimate ####
MR_width_S <- transmute(MR_width_pivot, non_eaten = `0`, eaten = `1`, S_MR = `0` - `1`, trait = "width")
MR_width_S1 <- select(MR_width_S, island, S_MR)

##### Width methods binding. Putting together both Point in time and Mark recapture data
width_S_methods <- bind_cols(PT_width_S1, MR_width_S1)
width_S_methods <- transmute(width_S_methods, island = island...1, S_PT, S_MR,trait = "width")



###########################################################################################

###### Depth #####
depth_select <- select(dataset, method, year, island, mericarp, depth, Present, eaten)

## Width Point in time ##### 
depth_PT <- filter(depth_select, method == "Point in time") 
PT_depth_island <- group_by(depth_PT, island, eaten)

# Create a summary of the eaten and uneaten column
PT_depth_summary <- PT_depth_island %>% summarise_each(funs(mean), depth)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
PT_depth_pivot <- pivot_wider(PT_depth_summary, names_from = eaten, values_from = depth) 

# PT Add S* estimate ####
PT_depth_S <- transmute(PT_depth_pivot, non_eaten = `0`, eaten = `1`, S_PT = `0` - `1`, trait = "depth")
PT_depth_S1 <- select(PT_depth_S, island, S_PT)

## Width Mark recapture ####
depth_MR <- filter(depth_select, method == "Mark Recapture")
MR_depth_island <- group_by(depth_MR, island, eaten)
#MR_width_island_present <- group_by(width_MR, island, Present)

# Create a summary of the eaten and uneaten column
MR_depth_summary <- MR_depth_island %>% summarise_each(funs(mean), depth)
#MR_width_summary_present <- MR_width_island_present %>% summarise_each(funs(mean), width)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
MR_depth_pivot <- pivot_wider(MR_depth_summary, names_from = eaten, values_from = depth)

# MR Add S* estimate ####
MR_depth_S <- transmute(MR_depth_pivot, non_eaten = `0`, eaten = `1`, S_MR = `0` - `1`, trait = "depth")
MR_depth_S1 <- select(MR_depth_S, island, S_MR)

##### Width methods binding. Putting together both Point in time and Mark recapture data
depth_S_methods <- bind_cols(PT_depth_S1, MR_depth_S1)
depth_S_methods <- transmute(depth_S_methods, island = island...1, S_PT, S_MR,trait = "depth")


###########################################################################################


###### longsp (longest_spine) ####
longsp_select <- select(dataset, method, year, island, mericarp, longest_spine, Present, eaten)
longsp_select <- filter(longsp_select, !is.na(longest_spine))
## Width Point in time ##### 
longsp_PT <- filter(longsp_select, method == "Point in time")
PT_longsp_island <- group_by(longsp_PT, island, eaten)

# Create a summary of the eaten and uneaten column
PT_longsp_summary <- PT_longsp_island %>% summarise_each(funs(mean), longest_spine)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
PT_longsp_pivot <- pivot_wider(PT_longsp_summary, names_from = eaten, values_from = longest_spine) 

# PT Add S* estimate ####
PT_longsp_S <- transmute(PT_longsp_pivot, non_eaten = `0`, eaten = `1`, S_PT = `0` - `1`, trait = "longest_spine")
PT_longsp_S1 <- select(PT_longsp_S, island, S_PT)

## Width Mark recapture ####
longsp_MR <- filter(longsp_select, method == "Mark Recapture")
MR_longsp_island <- group_by(longsp_MR, island, eaten)
#MR_width_island_present <- group_by(width_MR, island, Present)

# Create a summary of the eaten and uneaten column
MR_longsp_summary <- MR_longsp_island %>% summarise_each(funs(mean), longest_spine)
#MR_width_summary_present <- MR_width_island_present %>% summarise_each(funs(mean), width)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
MR_longsp_pivot <- pivot_wider(MR_longsp_summary, names_from = eaten, values_from = longest_spine)

# MR Add S* estimate ####
MR_longsp_S <- transmute(MR_longsp_pivot, non_eaten = `0`, eaten = `1`, S_MR = `0` - `1`, trait = "longest_spine")
MR_longsp_S1 <- select(MR_longsp_S, island, S_MR)

##### Width methods binding. Putting together both Point in time and Mark recapture data
longsp_S_methods <- bind_cols(PT_longsp_S1, MR_longsp_S1)
longsp_S_methods <- transmute(longsp_S_methods, island = island...1, S_PT, S_MR,trait = "longest_spine")



###########################################################################################



###### Spine tip distance (MR only) ####
tip_select <- select(dataset, method, year, island, mericarp, spine_tip_distance, Present, eaten)
tip_select <- filter(tip_select, !is.na(spine_tip_distance))

## Width Mark recapture ####
tip_MR <- filter(tip_select, method == "Mark Recapture")
MR_tip_island <- group_by(tip_MR, island, eaten)
#MR_width_island_present <- group_by(width_MR, island, Present)

# Create a summary of the eaten and uneaten column
MR_tip_summary <- MR_tip_island %>% summarise_each(funs(mean), spine_tip_distance)
#MR_width_summary_present <- MR_width_island_present %>% summarise_each(funs(mean), width)

# Pivot the previous table to put survivors (non eaten) and dead (eaten) in the same row 
MR_tip_pivot <- pivot_wider(MR_tip_summary, names_from = eaten, values_from = spine_tip_distance)

# MR Add S* estimate ####
MR_tip_S <- transmute(MR_tip_pivot, non_eaten = `0`, eaten = `1`, S_MR = `0` - `1`, trait = "spine_tip_distance")
MR_tip_S1 <- select(MR_tip_S, island, S_MR)

##### Width methods binding. Putting together both Point in time and Mark recapture data
tip_S_methods <- transmute(MR_tip_S1, S_MR,trait = "longest_spine")



##### Binding all traits ####
S_LW_estimates <- bind_rows(length_S_methods, width_S_methods)
S_LWD_estimates <- bind_rows(S_LW_estimates, depth_S_methods)
S_longsp_estimates <- bind_rows(S_LWD_estimates, longsp_S_methods)


##### Write .csv file of S* estimates ####
S_estimates <- S_longsp_estimates
write.csv(S_estimates, "S_estimates.csv")

