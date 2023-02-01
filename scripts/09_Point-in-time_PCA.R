# PCA script

# Load the point in time dataset ####
# This dataset is the point in time dataset for all years

point_time <- read_csv("~/Vault of Ideas/20 - 29 Tribulus Research/24 Chapter. Tribulus natural selection experiment/24.03 R code/Tribulus Selection experiment/Data/Processed/Point in time populations.csv")
point_time <- as_tibble(point_time)
point_time

# Changed variables to factors
point_time <- point_time %>% mutate_at(vars(year,
                                            island, population, lower_spine, 
                                            spine_position, 
                                            eaten, eaten_insects,
                                            `year island`, year_pop,
                                            seed_position_1, seed_position_2,
                                            seed_position_3, seed_position_4,
                                            seed_position_5, seed_position_6,
                                            germinated, germinated_position_1,
                                            germinated_position_2, germinated_position_3,
                                            germinated_position_4, germinated_position_5,
                                            germinated_position_6), list(factor))
str(point_time)
